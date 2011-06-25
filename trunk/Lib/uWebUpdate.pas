//* File:     Lib\uWebUpdate.pas
//* Created:  2008-09-20
//* Modified: 2009-05-13
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uWebUpdate;

interface

uses uTypes;

const
	LocalVersionFileName = 'version.txt';
	WebVersionFileName = 'version.txt';

procedure DownloadFile(const AURL: string; const TargetFileName: string);
function GetWebVersion(const Web: string): string;
procedure CheckForUpdate(const ShowMessageIfSuccess: BG = True);

implementation

uses
	uInputFormat, uStrings, uUser, uProjectInfo, uFiles, uMsg, uAPI, uProjectVersion,
	Classes, IdHTTP, SysUtils;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
begin
	IdHTTP1 := TIdHTTP.Create(nil);
	try
		IdHTTP1.HandleRedirects := True;
		if (not FileExists(TargetFileName)) or DeleteFileEx(TargetFileName) then
		begin
			AResponseContent := TFileStream.Create(TargetFileName, fmCreate or fmShareDenyNone);
			try
				IdHTTP1.Get(AURL, AResponseContent);
			finally
				AResponseContent.Free;
			end;
		end;
	finally
		IdHTTP1.Free;
	end;
end;

function GetWebVersion(const Web: string): string;
var
	TargetFileName: TFileName;
begin
	Result := '?';
	try
		TargetFileName := TempDir + WebVersionFileName;
		DownloadFile(Web + WebVersionFileName, TargetFileName);
		Result := ReadStringFromFile(TargetFileName);
		DeleteFileEx(TargetFileName);
	except
		on E: Exception do
		begin
			Warning('Can not receive version from the web %1!' + LineSep + E.Message, [Web]);
		end;
	end;
end;

procedure CheckForUpdate(const ShowMessageIfSuccess: BG = True);
var
	WebVersion, LocalVersion: string;
	Web: string;
begin
	Web := MyWeb + '/Software/' + GetProjectInfo(piInternalName) + '/';
	WebVersion := GetWebVersion(Web);

	LocalVersion := GetProjectInfo(piProductVersion);
	case CompareVersion(WebVersion, LocalVersion) of
	FirstGreater:
	begin
		if Confirmation('New version ' + WebVersion + ' is available. Your version is ' + LocalVersion + '. Do you want to download it?', [mbYes, mbNo]) = mbYes then
		begin
			APIOpen(Web + GetProjectInfo(piInternalName) + '.zip');
		end;
	end;
	FirstLess:
	begin
		Warning('You are using newer version ' + LocalVersion + ' that version ' + WebVersion + ' on the web!');
	end
	else
	begin
		if ShowMessageIfSuccess then
			Information('You are using the latest version ' + LocalVersion + '.');
	end;
	end;
end;

end.
