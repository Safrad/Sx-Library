// * File:     Lib\uLink.pas
// * Created:  2001-12-01
// * Modified: 2009-09-05
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uLink;

interface

uses
	uTypes,
	SysUtils;

procedure CreateLink(
	const LinkFileName: UnicodeString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: U2;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);

implementation

uses
	uFiles,
	Windows, ShlObj, ActiveX, ComObj;

procedure CreateLink(
	const LinkFileName: UnicodeString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: U2;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);
var
	MyObject : IUnknown;
	MySLink : IShellLink;
	MyPFile : IPersistFile;
begin
	MyObject := CreateComObject(CLSID_ShellLink);
	MySLink := MyObject as IShellLink;
	MyPFile := MyObject as IPersistFile;

	MySLink.SetArguments(PChar(Arguments));
	MySLink.SetPath(PChar(Target));
	MySLink.SetWorkingDirectory(PChar(StartIn));
	MySLink.SetDescription(PChar(Description));
	MySLink.SetIconLocation(PChar(IconFileName), IconIdex);
	MySLink.SetHotkey(HotKey);

	if CreateDirEx(ExtractFilePath(LinkFileName)) then
		MyPFile.Save(PWChar(LinkFileName), False);
	MySLink := nil;
	MyPFile := nil;
	MyObject := nil;
end;

end.

