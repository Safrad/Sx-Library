unit uWebUpdate;

interface

uses uTypes;

const
	LocalVersionFileName = 'version.txt';
	WebVersionFileName = 'version.txt';

procedure DownloadFile(const AURL: string; const TargetFileName: string);
function GetWebFile(const Web: string): string;
function GetWebVersion(const Web: string): string;
procedure CheckForUpdate; overload;
procedure CheckForUpdate(const ShowMessageIfSuccess: BG); overload;

implementation

uses
	uLog,
	uInputFormat, uStrings, uProjectInfo, uFiles, uMsg, uAPI, uProjectVersion, ufTextStatus,
	Windows, Classes, IdHTTP, SysUtils;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
begin
	MainLog.Add('Download file ' + AddQuoteF(AURL), mlDebug);
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
		try
			DownloadFile(Web + WebVersionFileName, TargetFileName);
		// TODO : replace with IdHTTP.Get(FileName)
			Result := ReadStringFromFile(TargetFileName);
		finally
			if FileExists(TargetFileName) then
				DeleteFileEx(TargetFileName);
		end;
	except
		on E: Exception do
		begin
			Warning('%1, can not receive project version from %2!', [DelBESpaceF(E.Message), Web + WebVersionFileName]);
		end;
	end;
end;

function GetWebFile(const Web: string): string;
var
	TargetFileName: TFileName;
begin
	Result := '?';
	try
		TargetFileName := TempDir + 'a.txt';
		try
			DownloadFile(Web, TargetFileName);
		// TODO : replace with IdHTTP.Get(FileName)
			Result := ReadStringFromFile(TargetFileName);
		finally
			if FileExists(TargetFileName) then
				DeleteFileEx(TargetFileName);
		end;
	except
		on E: Exception do
		begin
			Warning('%1, can not receive file from %2!', [DelBESpaceF(E.Message), Web]);
		end;
	end;
end;

procedure CheckForUpdate; overload;
begin
	CheckForUpdate(True);
end;

procedure CheckForUpdate(const ShowMessageIfSuccess: BG); overload;
var
	WebVersion, LocalVersion: string;
	Web: string;
begin
//	Web := MyWeb + '/Software/' + GetProjectInfo(piInternalName) + '/';
	Web := GetProjectInfo(piWeb);
  if Web = '' then Exit;
  
//	ShowStatusWindow('Receiving project version from Web.');
	try
		WebVersion := GetWebVersion(Web);
	finally
//		HideStatusWindow;
	end;
	if WebVersion = '?' then
		Exit;

	LocalVersion := GetProjectInfo(piProductVersion);
	case CompareVersion(WebVersion, LocalVersion) of
		FirstGreater:
			begin
				if Confirmation('New version ' + WebVersion + ' is available. Your version is ' +
						LocalVersion + '. Do you want to download it?', [mbYes, mbNo]) = mbYes then
				begin
					APIOpen(Web + GetProjectInfo(piInternalName) + '.zip');
				end;
			end;
		FirstLess:
			begin
				Warning('You are using newer version ' + LocalVersion + ' that version ' + WebVersion +
						' on the web!');
			end
		else
		begin
			if ShowMessageIfSuccess then
				Information('You are using the latest version ' + LocalVersion + '.');
		end;
	end;
end;

end.
