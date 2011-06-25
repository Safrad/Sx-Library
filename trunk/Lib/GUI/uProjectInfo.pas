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

type
	TProjectInfo = (
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

function GetProjectInfo(const ProjectInfo: TProjectInfo): string;

var
	ProjectInfoStr: array[TProjectInfo] of string;
	FProjectInfo: array [TProjectInfo] of string;

implementation

uses
	uTypes,
//	uFiles,
	Windows, SysUtils,
	TypInfo;

var
	Initialized: BG;

procedure InitProjectInfoStr;
var
	i: TProjectInfo;
begin
	for i := Low(TProjectInfo) to High(TProjectInfo) do
	begin
		ProjectInfoStr[i] := Copy(GetEnumName(TypeInfo(TProjectInfo), Ord(i)), 3, MaxInt);
	end;
end;

procedure SetProjectInfo;
var
	AppFileName: string;
	AppSize: UG;
	i: TProjectInfo;
	Buf: PWideChar;
	Value: PChar;
	// Unused
	LenOfValue: UG;
	Handle: THandle;
begin
	Initialized := True;
	InitProjectInfoStr;

	AppFileName := ParamStr(0);
	AppSize := GetFileVersionInfoSize(PChar(AppFileName), Handle{ API function initialize always to 0 });
	if AppSize > 0 then
	begin
		Buf := AllocMem(AppSize);
		try
			if GetFileVersionInfo(PChar(AppFileName), Handle{ Unused in API function }, AppSize, Buf) then
			begin
//				uFiles.WriteBufferToFile('C:\Temp.txt', Buf, AppSize);
				for i := Low(TProjectInfo) to High(TProjectInfo) do
				begin
					if VerQueryValue(Buf, PChar('StringFileInfo\040904E4\' + ProjectInfoStr[i]), Pointer(Value), LenOfValue) then
						FProjectInfo[i] := Value;
				end;
			end;
		finally
			FreeMem(Buf, AppSize);
		end;
	end;
end;

function GetProjectInfo(const ProjectInfo: TProjectInfo): string;
begin
	if not Initialized then
		SetProjectInfo;
	Result := FProjectInfo[ProjectInfo];
end;

end.

