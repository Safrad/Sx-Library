unit uProjectInfo;

interface

uses SysUtils;

type
	TProjectInfoName = (
		piFileVersion, // Displayed twice in Windows file property, used in log
		piFileDescription, // Displayed as "Description" in Windows file property
		piLegalCopyright, // Displayed as "Copyright" in Windows file property

		piCompanyName, // Displayed as "Company" in Windows file property
		piInternalName,
//		piLanguage, // Automatic
		piLegalTrademarks,
		piOriginalFilename, // Displayed as "Original File name" in Windows file property
		piProductName,
		piProductVersion,
//		piPrivateBuild, // Displayed as "Private Build Description" in Windows file property
//		piSpecialBuild, // Displayed as "Special Build Description" in Windows file property

		piAuthor, // Custom
		piEMail, // Custom
		piRelease, // Custom
		piWeb // Custom
	);
	TProjectInfo = class
	private
		FProjectInfoNames: array[TProjectInfoName] of string;
		FApplicationFileName: TFileName;
		procedure SetProjectInfos;
	public
		constructor Create(const ApplicationFileName: TFileName);
		function GetProjectInfo(const ProjectInfoName: TProjectInfoName): string;
		procedure SetProjectInfo(const ProjectInfoName: TProjectInfoName; const Value: string);
	property
		ApplicationFileName: TFileName read FApplicationFileName;
	end;

var
	ProjectInfoStr: array[TProjectInfoName] of string;

function ApplicationProjectInfo: TProjectInfo;
function GetProjectInfo(const ProjectInfo: TProjectInfoName): string;

var
	ThisProjectInfo: TProjectInfo;

implementation

uses
	uTypes,
	uFiles,
	Windows,
	uOutputFormat, uStrings,
	TypInfo;

{ TProjectInfo }

procedure TProjectInfo.SetProjectInfos;
var
	AppFileName: PChar;
	AppSize: UG;
	i: TProjectInfoName;
	Buf: PWideChar;
	Value: {$ifdef UNICODE}PWideChar{$else}PAnsiChar{$endif};
	ValueA: PAnsiChar;
	Id: string;
	// Unused
	LenOfValue: U4;
	Handle: Cardinal;
//	__VS_FIXEDFILEINFO: ^VS_FIXEDFILEINFO;
begin
	AppFileName := PChar(ExpandDir(FApplicationFileName));
	AppSize := GetFileVersionInfoSize(AppFileName, Handle{ API function initialize always to 0 });
	if AppSize > 0 then
	begin
		Buf := AllocMem(AppSize);
		try
			if GetFileVersionInfo(AppFileName, Handle{ Unused in API function }, AppSize, Buf) then
			begin
{				__VS_FIXEDFILEINFO := AllocMem(AppSize);
				LenOfValue := 0;
				if VerQueryValue(Buf, PChar('\'), Pointer(__VS_FIXEDFILEINFO), LenOfValue) then
				begin
					Id := Value;
				end;}
				Id := '040904E4';
				for i := Low(TProjectInfoName) to High(TProjectInfoName) do
				begin
{$ifdef UNICODE}
					if VerQueryValueW(Buf, PWideChar(WideString('StringFileInfo\' + Id + '\' + ReplaceF(ProjectInfoStr[i], CharSpace, ''))), Pointer(Value), LenOfValue) then
{$else}
					if VerQueryValueA(Buf, PAnsiChar(AnsiString('StringFileInfo\' + Id + '\' + ReplaceF(ProjectInfoStr[i], CharSpace, ''))), Pointer(Value), LenOfValue) then
{$endif}
					begin
						FProjectInfoNames[i] := string(Value);
						if i in [piProductVersion, piFileVersion] then
							Replace(FProjectInfoNames[i], ', ', '.');
					end;
				end;
{$ifdef UNICODE}
				if VerQueryValueW(Buf, PWideChar(WideString('\VarFileInfo\Translation')), Pointer(ValueA), LenOfValue) then
{$else}
				if VerQueryValueA(Buf, PAnsiChar(AnsiString('\VarFileInfo\Translation')), Pointer(ValueA), LenOfValue) then
{$endif}
				begin
					if LenOfValue = 4 then
					begin
						NumericBase := 16;
						try
							Id := NToS(SG(ValueA[1]), '00') + NToS(SG(ValueA[0]), '00') + NToS(SG(ValueA[3]), '00') + NToS(SG(ValueA[2]), '00');
						finally
							NumericBase := 10;
						end;
					end;
				end;
				for i := Low(TProjectInfoName) to High(TProjectInfoName) do
				begin
{$ifdef UNICODE}
					if VerQueryValueW(Buf, PWideChar(WideString('StringFileInfo\' + Id + '\' + ReplaceF(ProjectInfoStr[i], CharSpace, ''))), Pointer(Value), LenOfValue) then
{$else}
					if VerQueryValueA(Buf, PAnsiChar(AnsiString('StringFileInfo\' + Id + '\' + ReplaceF(ProjectInfoStr[i], CharSpace, ''))), Pointer(Value), LenOfValue) then
{$endif}
					begin
						FProjectInfoNames[i] := string(Value);
						if i in [piProductVersion, piFileVersion] then
							Replace(FProjectInfoNames[i], ', ', '.');
					end;
				end;
				if (Pos('.', FProjectInfoNames[piFileVersion]) = 0) and (Pos('.', FProjectInfoNames[piProductVersion]) <> 0) then
					FProjectInfoNames[piFileVersion] := FProjectInfoNames[piProductVersion] + '.' + FProjectInfoNames[piFileVersion];
			end;
		finally
			FreeMem(Buf, AppSize);
		end;
	end;
end;

constructor TProjectInfo.Create(const ApplicationFileName: TFileName);
begin
	FApplicationFileName := ApplicationFileName;
	if ProjectInfoStr[piFileVersion] = '' then
		EnumToStrEx(TypeInfo(TProjectInfoName), ProjectInfoStr);
	SetProjectInfos;
end;

function TProjectInfo.GetProjectInfo(const ProjectInfoName: TProjectInfoName): string;
begin
	Result := FProjectInfoNames[ProjectInfoName];
end;

procedure TProjectInfo.SetProjectInfo(
  const ProjectInfoName: TProjectInfoName; const Value: string);
begin
  FProjectInfoNames[ProjectInfoName] := Value;
end;

function ApplicationProjectInfo: TProjectInfo;
begin
	if not Assigned(ThisProjectInfo) then
	begin
		ThisProjectInfo := TProjectInfo.Create(GetModuleFileNameFunc);
		if ThisProjectInfo.FProjectInfoNames[piInternalName] = '' then // if not initialized in ProjectInfo
			ThisProjectInfo.FProjectInfoNames[piInternalName] := DelFileExt(ExtractFileName(ThisProjectInfo.FApplicationFileName));
		if ThisProjectInfo.FProjectInfoNames[piInternalName] = '' then
      ThisProjectInfo.FProjectInfoNames[piInternalName] := 'Unknown';

		if ThisProjectInfo.FProjectInfoNames[piCompanyName] = '' then
      ThisProjectInfo.FProjectInfoNames[piCompanyName] := 'Sx Software';

		if IsDebug then
    begin
			AppendStr(ThisProjectInfo.FProjectInfoNames[piFileVersion], '+');
			AppendStr(ThisProjectInfo.FProjectInfoNames[piProductVersion], '+');
		end;
	end;
	Result := ThisProjectInfo;
end;

function GetProjectInfo(const ProjectInfo: TProjectInfoName): string;
begin
	Result := ApplicationProjectInfo.GetProjectInfo(ProjectInfo);
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
	FreeAndNil(ThisProjectInfo);
{$ENDIF NoFinalization}
end.

