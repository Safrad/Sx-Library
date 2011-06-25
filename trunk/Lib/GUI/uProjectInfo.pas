//* File:     Lib\uProjectInfo.pas
//* Created:  2006-06-22
//* Modified: 2007-11-25
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

{$ifdef Console}
//'Warning: Console does not contain any version info!'
{$endif}

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
		procedure SetProjectInfo;
	public
		constructor Create(const ApplicationFileName: TFileName);
		function GetProjectInfo(const ProjectInfoName: TProjectInfoName): string;
	property
		ApplicationFileName: TFileName read FApplicationFileName;
	end;

var
	ProjectInfoStr: array[TProjectInfoName] of string;

function GetProjectInfo(const ProjectInfo: TProjectInfoName): string;

implementation

uses
	uTypes,
	uFiles,
	Windows,
	uOutputFormat, uStrings,
	TypInfo;

var
	ThisProjectInfo: TProjectInfo;

procedure InitProjectInfoStr;
var
	i: TProjectInfoName;
begin
	for i := Low(TProjectInfoName) to High(TProjectInfoName) do
	begin
		ProjectInfoStr[i] := Copy(GetEnumName(TypeInfo(TProjectInfoName), Ord(i)), 3, MaxInt);
	end;
end;

{ TProjectInfo }

procedure TProjectInfo.SetProjectInfo;
var
	AppSize: UG;
	i: TProjectInfoName;
	Buf: PWideChar;
	Value: PChar;
	Id: string;
	// Unused
	LenOfValue: UG;
	Handle: THandle;
//	__VS_FIXEDFILEINFO: ^VS_FIXEDFILEINFO;
begin
	AppSize := GetFileVersionInfoSize(PChar(FApplicationFileName), Handle{ API function initialize always to 0 });
	if AppSize > 0 then
	begin
		Buf := AllocMem(AppSize);
		try
			if GetFileVersionInfo(PChar(FApplicationFileName), Handle{ Unused in API function }, AppSize, Buf) then
			begin
{				__VS_FIXEDFILEINFO := AllocMem(AppSize);
				LenOfValue := 0;
				if VerQueryValue(Buf, PChar('\'), Pointer(__VS_FIXEDFILEINFO), LenOfValue) then
				begin
					Id := Value;
				end;}
//				uFiles.WriteBufferToFile('C:\Temp.txt', Buf, AppSize);
				Id := '040904E4';
				if VerQueryValue(Buf, PChar('\VarFileInfo\Translation'), Pointer(Value), LenOfValue) then
				begin
					if LenOfValue = 4 then
					begin
						NumericBase := 16;
						Id := NToS(SG(Value[1]), '00') + NToS(SG(Value[0]), '00') + NToS(SG(Value[3]), '00') + NToS(SG(Value[2]), '00');
						NumericBase := 10;
					end;
				end;
				for i := Low(TProjectInfoName) to High(TProjectInfoName) do
				begin
					if VerQueryValue(Buf, PChar('StringFileInfo\' + Id + '\' + ProjectInfoStr[i]), Pointer(Value), LenOfValue) then
					begin
						FProjectInfoNames[i] := Value;
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

	if FProjectInfoNames[piInternalName] = '' then // if not initialized in ProjectInfo
		FProjectInfoNames[piInternalName] := DelFileExt(ExtractFileName(FApplicationFileName));
end;

constructor TProjectInfo.Create(const ApplicationFileName: TFileName);
begin
	FApplicationFileName := ApplicationFileName;
	SetProjectInfo;
end;

function TProjectInfo.GetProjectInfo(const ProjectInfoName: TProjectInfoName): string;
begin
	Result := FProjectInfoNames[ProjectInfoName];
end;

function GetProjectInfo(const ProjectInfo: TProjectInfoName): string;
begin
	if not Assigned(ThisProjectInfo) then
	begin
		ThisProjectInfo := TProjectInfo.Create(ParamStr(0));
	end;
	Result := ThisProjectInfo.GetProjectInfo(ProjectInfo);
end;

initialization
	InitProjectInfoStr;
end.

