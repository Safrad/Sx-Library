unit uWebUpdate;

interface

uses
  Windows, Classes, SysUtils,
	uTypes;

const
	LocalVersionFileName = 'version.txt';
	WebVersionFileName = 'version.txt';

procedure DownloadFile(const AURL: string; const TargetFileName: string);
function DownloadData(const AURL: string): string; overload;
function DownloadData(const AURL: string; const AUserName: string; const APassword: string): string; overload;
function DownloadFileWithPost(const AURL: string; const Source: TStrings; const Encode: BG; TargetFileName: string): BG;
function GetWebVersion(const Web: string): string;

procedure CheckForUpdate; overload;
procedure CheckForUpdate(const ShowMessageIfSuccess: BG); overload;

implementation

uses
	uLog, uStopwatch,
	uInputFormat, uStrings, uProjectInfo, uFiles, uMsg, uProjectVersion, uOutputFormat, uMath, uAPI,
  IdHTTP, IdURI, IdMultipartFormData, IdException, IdStack;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
  NotOk: BG;
begin
  if LogDebug then
	  MainLogAdd('Download file ' + AddQuoteF(AURL), mlDebug);
	IdHTTP1 := TIdHTTP.Create(nil);
	try
		IdHTTP1.HandleRedirects := True;
		IdHTTP1.Request.UserAgent := GetProjectInfo(piProductName);
		IdHTTP1.Request.Referer := GetProjectInfo(piWeb);
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

function DownloadData(const AURL: string): string;
var
	IdHTTP1: TIdHTTP;
begin
  if LogDebug then
    MainLogAdd('Download data ' + AddQuoteF(AURL), mlDebug);
	IdHTTP1 := TIdHTTP.Create(nil);
	try
		IdHTTP1.HandleRedirects := True;
    Result := IdHTTP1.Get(AURL);
	finally
		IdHTTP1.Free;
	end;
end;

function DownloadData(const AURL: string; const AUserName: string; const APassword: string): string;
var
	IdHTTP1: TIdHTTP;
begin
  if LogDebug then
    MainLogAdd('Download data ' + AddQuoteF(AURL), mlDebug);
	IdHTTP1 := TIdHTTP.Create(nil);
	try
    IdHTTP1.Request.Clear;
    IdHTTP1.Request.BasicAuthentication:= true;
    IdHTTP1.Request.UserName := AUserName;
    IdHTTP1.Request.Password := APassword;

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
  Stream: TIdMultiPartFormDataStream;
  InLineIndex: SG;
  FieldName, FieldValue: string;
  i: SG;
  PostData: string;
  Stopwatch: TStopwatch;
begin
  Result := False;
	IdHTTP1 := TIdHTTP.Create(nil);
	try
//		IdHTTP1.HandleRedirects := True;
	  AResponseContent := TFileStream.Create(TargetFileName, fmCreate or fmShareDenyNone);
    Stopwatch := TStopwatch.Create;
    try
      try
        Stopwatch.Start;
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
        Stopwatch.Stop;
      	MainLog.Add('Download time: ' + MsToStr(Stopwatch.Elapsed.Milliseconds, diSD, 3, False, ofIO) + 's', mlDebug);
        Result := True;
      except
        on E: Exception do
          if LogError then
          	MainLogAdd(E.Message, mlError);
      end;
		finally
      Stopwatch.Free;
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
		crFirstGreater:
			begin
				if Confirmation('New version ' + WebVersion + ' is available. Your version is ' +
						LocalVersion + '. Do you want to download it?', [mbYes, mbNo]) = mbYes then
				begin
					APIOpen(Web + GetProjectInfo(piInternalName) + '.zip');
				end;
			end;
		crFirstLess:
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
