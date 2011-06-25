//* File:     Lib\uWebUpdate.pas
//* Created:  2008-09-20
//* Modified: 2008-09-20
//* Version:  1.1.41.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uWebUpdate;

interface

uses uTypes;

procedure CheckForUpdate(const ShowMessageIfSuccess: BG = True);

implementation

uses
	uInputFormat, uStrings, uUser, uProjectInfo, uFiles, uMsg, uAPI,
	Classes, IdHTTP, SysUtils;

type
	TProjectVersion = record
		MajorVersion, MinorVersion, ReleaseVersion, BuildVersion: SG;
	end;

function CreateVersion(const Version: string): TProjectVersion;
var
	InLineIndex: SG;
begin
	InLineIndex := 1;
	Result.MajorVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 1, MaxInt, 1);
	Result.MinorVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1);
	Result.ReleaseVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1);
	Result.BuildVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1);
end;

const
	FirstGreater = 1;
	FirstLess = -1;
	BothSame = 0;
	
(**
* @return 0 if Version1 = Version2, 1 if Version1 > Version2, -1 if Version1 < Version2
*)
function CompareVersion(const Version1, Version2: TProjectVersion): SG; overload;
begin
	if Version1.MajorVersion = Version2.MajorVersion then
	begin
		if Version1.MinorVersion = Version2.MinorVersion then
		begin
			if Version1.ReleaseVersion = Version2.ReleaseVersion then
			begin
				if Version1.BuildVersion = Version2.BuildVersion then
				begin
					Result := BothSame;
				end
				else if Version1.BuildVersion > Version2.BuildVersion then
					Result := FirstGreater
				else
					Result := FirstLess;
			end
			else if Version1.ReleaseVersion > Version2.ReleaseVersion then
				Result := FirstGreater
			else
				Result := FirstLess;
		end
		else if Version1.MinorVersion > Version2.MinorVersion then
			Result := FirstGreater
		else
			Result := FirstLess;
	end
	else if Version1.MajorVersion > Version2.MajorVersion then
		Result := FirstGreater
	else
		Result := FirstLess;
end;

function CompareVersion(const Version1, Version2: string): SG; overload;
var
	Version1a, Version2a: TProjectVersion;
begin
	Version1a := CreateVersion(Version1);
	Version2a := CreateVersion(Version2);
	Result := CompareVersion(Version1a, Version2a);
end;

procedure CheckForUpdate(const ShowMessageIfSuccess: BG = True);
var
	AResponseContent: TStream;
	WebVersion, LocalVersion: string;
	IdHTTP1: TIdHTTP;
	Web: string;
	FileName: TFileName;
begin
	IdHTTP1 := TIdHTTP.Create(nil);
	try
		try
			FileName := TempDir + 'version.inc';
			IdHTTP1.HandleRedirects := True;
			AResponseContent := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
			Web := MyWeb + '/Software/' + GetProjectInfo(piInternalName) + '/';
			WebVersion := '?';
			try
				IdHTTP1.Get(Web + 'version.inc', AResponseContent);
			finally
				AResponseContent.Free;
				WebVersion := ReadStringFromFile(FileName);
				DeleteFileEx(FileName);
			end;
		except
			on E: Exception do
			begin
				Warning('Can not receive version from the web!' + LineSep + E.Message);
				Exit;
			end;
		end;
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
	finally
		IdHTTP1.Free;
	end;
end;

end.     
