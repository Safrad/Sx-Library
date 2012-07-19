unit uWebUpdate;

interface

uses uTypes;

const
	LocalVersionFileName = 'version.txt';
	WebVersionFileName = 'version.txt';

procedure DownloadFile(const AURL: string; const TargetFileName: string);
function DownloadData(const AURL: string): string;
function GetWebVersion(const Web: string): string;
procedure CheckForUpdate; overload;
procedure CheckForUpdate(const ShowMessageIfSuccess: BG); overload;

implementation

uses
	uLog,
	uInputFormat, uStrings, uProjectInfo, uFiles, uMsg, uAPI, uProjectVersion, ufTextStatus,
	Windows, Classes, IdHTTP, IdException, IdStack, SysUtils;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
begin
  if LogDebug then
	  LogAdd('Download file ' + AddQuoteF(AURL));
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

function DownloadData(const AURL: string): string;
var
	IdHTTP1: TIdHTTP;
begin
  if LogDebug then
    LogAdd('Download data ' + AddQuoteF(AURL));
	IdHTTP1 := TIdHTTP.Create(nil);
	try
		IdHTTP1.HandleRedirects := True;
    Result := IdHTTP1.Get(AURL);
	finally
		IdHTTP1.Free;
	end;
end;

function GetWebVersion(const Web: string): string;
begin
	Result := '?';
	try
		Result := DownloadData(Web + WebVersionFileName);
	except
		on E: Exception do
		begin
      if (E is EIdSocketError) and (EIdSocketError(E).LastError = 11004) then
  			Warning('No internet connection available!', [])
      else
  			ErrorMsg('%1, can not receive project version from %2!', [DelBESpaceF(E.Message), Web + WebVersionFileName]);
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
