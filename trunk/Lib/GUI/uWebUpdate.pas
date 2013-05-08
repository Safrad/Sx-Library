unit uWebUpdate;

interface

uses
  Windows, Classes, SysUtils,
	uTypes;

const
	LocalVersionFileName = 'version.txt';
	WebVersionFileName = 'version.txt';

procedure DownloadFileEx(const AURL: string; const TargetFileName: string);
procedure DownloadFile(const AURL: string; const TargetFileName: string);
function DownloadData(const AURL: string): string;
function DownloadFileWithPost(const AURL: string; const Source: TStrings; const Encode: BG; TargetFileName: string): BG;
function GetWebVersion(const Web: string): string;
procedure CheckForUpdate; overload;
procedure CheckForUpdate(const ShowMessageIfSuccess: BG); overload;

implementation

uses
	uLog,
	uInputFormat, uStrings, uProjectInfo, uFiles, uMsg, uAPI, uProjectVersion, ufTextStatus, uSimulation, uOutputFormat,
  IdHTTP, IdURI, IdMultipartFormData, IdException, IdStack,
  ufStatus,
  ExtActns, Forms;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
  NotOk: BG;
begin
  if LogDebug then
	  LogAdd('Download file ' + AddQuoteF(AURL));
	IdHTTP1 := TIdHTTP.Create(nil);
	try
		IdHTTP1.HandleRedirects := True;
		if (not FileExists(TargetFileName)) or DeleteFileEx(TargetFileName) then
		begin
      NotOk := True;
			AResponseContent := TFileStream.Create(TargetFileName, fmCreate or fmShareDenyNone);
			try
        try
					IdHTTP1.Get(AURL, AResponseContent);
          NotOk := False;
        except
          on E: Exception do
          begin
          	NotOk := True;
            raise;
          end;
        end;
			finally
				AResponseContent.Free;
        if NotOk then
          DeleteFileEx(TargetFileName);
			end;
		end;
	finally
		IdHTTP1.Free;
	end;
end;

type
  TObj = class
  public
    procedure OnDownloadProgress(Sender: TDownLoadURL; Progress,
        ProgressMax: Cardinal; StatusCode: TURLDownloadStatus; StatusText: String;
        var Cancel: Boolean);
  end;

{ TObj }

procedure TObj.OnDownloadProgress(Sender: TDownLoadURL; Progress,
  ProgressMax: Cardinal; StatusCode: TURLDownloadStatus;
  StatusText: String; var Cancel: Boolean);
begin
  Cancel := ufStatus.Cancel;
  UpdateMaximum(ProgressMax);
  UpdateStatus(Progress);
  Application.ProcessMessages;
  Sleep(10);
end;

var
  Obj: TObj;

procedure DownloadFileEx(const AURL: string; const TargetFileName: string);
var
	DownLoadURL: TDownLoadURL;
begin
  if LogDebug then
		LogAdd('Download file ' + AddQuoteF(AURL));
  if Obj = nil then
    Obj := TObj.Create;

  DownLoadURL := TDownLoadURL.Create(nil);
  try
    ShowStatusWindow(nil, nil);
    DownLoadURL.URL := AURL;
    DownLoadURL.Filename := TargetFileName;
    DownLoadURL.OnDownloadProgress := Obj.OnDownloadProgress;
    DownLoadURL.Visible := True;
    DownLoadURL.ExecuteTarget(nil);
  finally
    HideStatusWindow;
    DownLoadURL.Free;
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

function DownloadFileWithPost(const AURL: string; const Source: TStrings; const Encode: BG; TargetFileName: string): BG;
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
  StartTime: U4;
  Stream: TIdMultiPartFormDataStream;
  InLineIndex: SG;
  FieldName, FieldValue: string;
  i: SG;
  PostData: string;
begin
  Result := False;
	IdHTTP1 := TIdHTTP.Create(nil);
	try
//		IdHTTP1.HandleRedirects := True;
	  AResponseContent := TFileStream.Create(TargetFileName, fmCreate or fmShareDenyNone);
    try
      try
        GetGTime;
        StartTime := GTime;
        IdHTTP1.Request.UserAgent := GetProjectInfo(piInternalName);
        if Source.Count > 0 then
        begin
          if Encode then
          begin
//            IdHTTP1.Request.ContentType := 'application/x-www-form-urlencoded';
            // Post do not work in Indy for Delphi 7!!!
            IdHTTP1.Post(AURL, Source, AResponseContent);
          end
          else
          begin
            Stream := TIdMultiPartFormDataStream.Create;
            try
              for i := 0 to Source.Count - 1 do
              begin
                InLineIndex := 1;
                PostData := Source[i];
                FieldName := ReadToChar(PostData, InLineIndex, '=');
                FieldValue := Copy(PostData, InLineIndex, MaxInt);
                {$if CompilerVersion < 19}
                FieldValue := ReplaceF(FieldValue, '%', '%%'); // Format function inside Stream.AddFormField
                {$ifend}
                Stream.AddFormField(FieldName, FieldValue{$if CompilerVersion >= 19}, 'utf-8'{$ifend});
              end;
//              IdHTTP1.Request.ContentType := 'multipart/form-data';
              // Post do not work in Indy for Delphi 7!!!
              IdHTTP1.Post(AURL, Stream, AResponseContent);
            finally
              Stream.Free;
            end;
          end;
        end
        else
        begin
          IdHTTP1.Get(AURL, AResponseContent);
        end;
        StartTime := IntervalFrom(StartTime);
      	MainLog.Add('Download time: ' + MsToStr(StartTime, diSD, 3, False, ofIO) + 's', mlDebug);
        Result := True;
      except
        on E: Exception do
          if LogError then
          	LogAdd(E.Message);
      end;
		finally
  		AResponseContent.Free;
		end;
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

initialization

finalization
  FreeAndNil(Obj);
end.
