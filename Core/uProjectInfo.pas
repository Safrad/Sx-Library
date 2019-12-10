unit uProjectInfo;

interface

uses
  SysUtils,
  uTypes;

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
    FPatched: BG;
    FPreRelease: BG;
    FDebug: BG;
    FSpecialBuild: BG;
    FInfoInferred: BG;
    FPrivateBuild: BG;
		procedure SetProjectInfos;
	public
		constructor Create(const ApplicationFileName: TFileName);

    // Input
	  property ApplicationFileName: TFileName read FApplicationFileName;
		procedure SetProjectInfo(const ProjectInfoName: TProjectInfoName; const Value: string);

    // Output
		function GetProjectInfo(const ProjectInfoName: TProjectInfoName): string;
    property Debug: BG read FDebug;
    property PreRelease: BG read FPreRelease;
    property Patched: BG read FPatched;
    property PrivateBuild: BG read FPrivateBuild;
    property InfoInferred: BG read FInfoInferred;
    property SpecialBuild: BG read FSpecialBuild;
    function GetSuffix: string;
	end;

var
	ProjectInfoStr: array[TProjectInfoName] of string;

function ApplicationProjectInfo: TProjectInfo;
function GetProjectInfo(const ProjectInfo: TProjectInfoName): string;

var
	ThisProjectInfo: TProjectInfo;

implementation

uses
	uFiles,
  uProjectVersion,
{$ifdef MSWINDOWS}
	Winapi.Windows,
{$endif}
{$ifdef Android}
  Androidapi.Helpers,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.NativeActivity,
{$endif}
	uOutputFormat, uStrings,
	TypInfo;

{ TProjectInfo }

procedure TProjectInfo.SetProjectInfos;
{$ifdef MSWINDOWS}
var
	AppFileName: PChar;
	AppSize: UG;
	i: TProjectInfoName;
	Buf: PWideChar;
	Value: {$ifdef UNICODE}PWideChar{$else}PAnsiChar{$endif};
	ValueA: PAnsiChar;
	Id: string;
  FixedFileInfo: ^tagVS_FIXEDFILEINFO;
	// Unused
	LenOfValue: U4;
	Handle: Cardinal;
  Version: TProjectVersion;
begin
	AppFileName := PChar(ExpandDir(FApplicationFileName));
	AppSize := GetFileVersionInfoSize(AppFileName, Handle{ API function initialize always to 0 });
	if AppSize > 0 then
	begin
		Buf := AllocMem(AppSize);
		try
			if GetFileVersionInfo(AppFileName, Handle{ Unused in API function }, AppSize, Buf) then
			begin
				LenOfValue := 0;
{$ifdef UNICODE}
					if VerQueryValueW(Buf, PWideChar(WideString('\')), Pointer(FixedFileInfo), LenOfValue) then
{$else}
					if VerQueryValueA(Buf, PAnsiChar(AnsiString('\')), Pointer(FixedFileInfo), LenOfValue) then
{$endif}
				begin
          Assert(LenOfValue = SizeOf(tagVS_FIXEDFILEINFO));
          if LenOfValue <> 0 then
          begin
            Version.Major := FixedFileInfo.dwFileVersionMS shr 16;
            Version.Minor := FixedFileInfo.dwFileVersionMS and $FFFF;
            Version.Release := FixedFileInfo.dwFileVersionLS shr 16;
            Version.Build := FixedFileInfo.dwFileVersionLS and $FFFF;
            FProjectInfoNames[piFileVersion] := VersionToStr(Version);

            FDebug := FixedFileInfo.dwFileFlags and VS_FF_DEBUG <> 0;
            FPreRelease := FixedFileInfo.dwFileFlags and VS_FF_PRERELEASE <> 0;
            FPatched := FixedFileInfo.dwFileFlags and VS_FF_PATCHED <> 0;
            FPrivateBuild := FixedFileInfo.dwFileFlags and VS_FF_PRIVATEBUILD <> 0;
            FInfoInferred := FixedFileInfo.dwFileFlags and VS_FF_INFOINFERRED <> 0;
            FSpecialBuild := FixedFileInfo.dwFileFlags and VS_FF_SPECIALBUILD <> 0;
          end;
				end;

				Id := '040904E4';
				for i := piFileDescription to High(TProjectInfoName) do
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
  					Id := IntToHex(U1(ValueA[1])) + IntToHex(U1(ValueA[0])) + IntToHex(U1(ValueA[3])) + IntToHex(U1(ValueA[2]));
					end;
				end;
				for i := piFileDescription to High(TProjectInfoName) do
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
{$else}
function GetPackageInfo: JPackageInfo;
var
  Activity: JActivity;
begin
  Activity := TJNativeActivity.Wrap(PANativeActivity(System.DelphiActivity)^.clazz);
  Result := Activity.getPackageManager.getPackageInfo(Activity.getPackageName, 0);
end;
var
  PackageInfo: JPackageInfo;
  PackageName: string;
  DotPos: SG;
begin
  PackageInfo := GetPackageInfo;
  PackageName := JStringToString(PackageInfo.packageName);
  DotPos := Pos('.', PackageName);
  if DotPos <> 0 then
  begin
    FProjectInfoNames[piCompanyName] := Copy(PackageName, 1, DotPos - 1);
    FProjectInfoNames[piProductName] := Copy(PackageName, DotPos + 1);
  end
  else
  begin
    raise EArgumentException.Create('Invalid package name');
  end;
  FProjectInfoNames[piFileVersion] := IntToStr(PackageInfo.versionCode);
  FProjectInfoNames[piProductVersion] := JStringToString(PackageInfo.versionName);
{$endif}
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

function TProjectInfo.GetSuffix: string;
begin
  Result := '';
  if ThisProjectInfo.FPreRelease then
  begin
    if ThisProjectInfo.FSpecialBuild then
      AppendStr(Result, 'pre-α')
    else
      AppendStr(Result, 'α');
  end
  else
  begin
    if ThisProjectInfo.FSpecialBuild then
      AppendStr(Result, 'β');
  end;

  if ThisProjectInfo.FPatched then
    AppendStr(Result, ' patched');
  if ThisProjectInfo.FPrivateBuild then
    AppendStr(Result, ' private');
  if IsDebug or ThisProjectInfo.Debug then
    AppendStr(Result, ' debug');
end;

procedure TProjectInfo.SetProjectInfo(
  const ProjectInfoName: TProjectInfoName; const Value: string);
begin
  FProjectInfoNames[ProjectInfoName] := Value;
end;

function DelTrailingZeros(const AText: string): string;
var
  i: SG;
begin
  i := Length(AText);
  while i >= 1 do
  begin
    if not CharInSet(AText[i], ['.', ',', '0']) then
      Break;
    Dec(i);
  end;
  Result := Copy(AText, 1, i);
end;

function ApplicationPlatform: string;
begin
  {$ifdef CPUx64}
  Result := CharSpace + 'x64';
  {$else}
  Result := '';
  {$endif}
end;

function ApplicationProjectInfo: TProjectInfo;
begin
	if not Assigned(ThisProjectInfo) then
	begin
		ThisProjectInfo := TProjectInfo.Create(GetModuleFileNameFunc(HInstance));
		if ThisProjectInfo.FProjectInfoNames[piInternalName] = '' then // if not initialized in ProjectInfo
    begin
			ThisProjectInfo.FProjectInfoNames[piInternalName] := DelFileExt(ExtractFileName(ThisProjectInfo.FApplicationFileName));
  		if ThisProjectInfo.FProjectInfoNames[piInternalName] = '' then
        ThisProjectInfo.FProjectInfoNames[piInternalName] := 'Unknown';
    end;

//    Assert(ThisProjectInfo.FProjectInfoNames[piCompanyName] <> '');
		if ThisProjectInfo.FProjectInfoNames[piCompanyName] = '' then
      ThisProjectInfo.FProjectInfoNames[piCompanyName] := 'Unknown';

  	ThisProjectInfo.FProjectInfoNames[piProductVersion] := DelTrailingZeros(ThisProjectInfo.FProjectInfoNames[piProductVersion]) + ThisProjectInfo.GetSuffix + ApplicationPlatform;
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

