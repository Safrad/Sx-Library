unit uWebUpdate;

interface

uses
  uTypes,

  Classes;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
function DownloadData(const AURL: string): string; overload;
function DownloadData(const AURL: string; const AUserName: string; const APassword: string): string; overload;
function DownloadFileWithPost(const AURL: string; const Source: TStrings; const Encode: BG; TargetFileName: string): BG;

implementation

uses
  SysUtils,

  IdHTTP,
  IdURI,
  IdMultipartFormData,
  IdException,
  IdStack,

	uMainLog,
  uStopwatch,
  uStrings,
  uProjectInfo,
  uFiles,
  uOutputFormat;

procedure DownloadFile(const AURL: string; const TargetFileName: string);
var
	IdHTTP1: TIdHTTP;
	AResponseContent: TStream;
  NotOk: BG;
begin
  if MainLog.IsLoggerFor(mlDebug) then
	  MainLog.Add('Download file ' + AddQuoteF(AURL), mlDebug);
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
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Download data ' + AddQuoteF(AURL), mlDebug);
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
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Download data ' + AddQuoteF(AURL), mlDebug);
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
        if MainLog.IsLoggerFor(mlDebug) then
        	MainLog.Add('Download time: ' + MsToStr(Stopwatch.Elapsed.Milliseconds, TDisplay.diSD, 3, False, ofIO) + 's', mlDebug);
        Result := True;
      except
        on E: Exception do
          if MainLog.IsLoggerFor(mlError) then
          	MainLog.Add(E.Message, mlError);
      end;
		finally
      Stopwatch.Free;
  		AResponseContent.Free;
		end;
	finally
		IdHTTP1.Free;
	end;
end;

end.
