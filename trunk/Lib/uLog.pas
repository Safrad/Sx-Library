unit uLog;

interface

uses
	uTypes, uFile, uMsg, uLogger,
	SysUtils;

type
	TLog = class(TLogger)
	private
		FData: string;
		FFileName: TFileName;
		FFile: TFile;
		FDirectWrite: BG;
    FInWrite: BG;
		FLoggingLevel: TMessageLevel;
    FLastMessageLevel: TMessageLevel;
    FMaxLogFiles: SG;
		procedure WriteLine(const Line: string);
	public
		property FileName: TFileName read FFileName;
		constructor Create(const FileName: TFileName; const LogLevel: TMessageLevel = DefaultLoggingLevel; const DirectWrite: BG = True; const MaxLogFiles: SG = 0);
		destructor Destroy; override;
		procedure Add(const Line: string); overload;
		procedure Add(const LogTime: TDateTime; const Line: string); overload;

		procedure Add(const Line: string; const MessageLevel: TMessageLevel); override; deprecated;
		procedure Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); override; deprecated;

    function FatalError: BG;
    function Error: BG;
    function Warning: BG;
    function Information: BG;
    function Debug: BG;
    function Confirmation: BG;

		procedure Flush;
		property LoggingLevel: TMessageLevel read FLoggingLevel write FLoggingLevel default DefaultLoggingLevel;
		property MaxLogFiles: SG read FMaxLogFiles write FMaxLogFiles;
	end;

	TLogMessageProcedure = procedure(const ALine: string; const AMessageLevel: TMessageLevel);

	// Add startup parameter -LogDebug for all debuging information

//  function LogDebug
	function MainLogWrite(const AMessageLevel: TMessageLevel): BG;

   // Use LogAdd with one of LogFatalError, LogError, LogWarning, LogInformation, LogDebug, LogConfirmation
   // i.e. "MainLogAdd('Log message.', mlError);" is equivalent of "if LogError then LogAdd('Log message.');"
	procedure MainLogAdd(const ALine: string; const AMessageLevel: TMessageLevel); deprecated;

  procedure LogAdd(const AMessage: string);

  function LogFatalError: BG;
  function LogError: BG;
  function LogWarning: BG;
  function LogInformation: BG;
  function LogDebug: BG;
  function LogConfirmation: BG;


	procedure InitializeLog;

var
	MainLog: TLog;

implementation

uses
	uParams, uFiles, uCharset,
	uOutputFormat, uEscape, uStrings, {$ifndef Console}uProjectInfo,{$endif}
  uDelete,
	Windows, TypInfo;

const
  MinLogFileSize = 4 * MB;
  MaxLogFileSize = 16 * MB; // > MinLogFileSize

procedure TLog.WriteLine(const Line: string);
var
	LLog: TLog;
	LineA: AnsiString;
begin
  if FInWrite then Exit;
  
  FInWrite := True;
	if Length(Line) > 0 then
	begin
		if FDirectWrite then
		begin
			if not FFile.Opened then Exit;

      if FFile.FileSize + Length(Line) > MaxLogFileSize then
      begin
        FLoggingLevel := mlNone;
      	Exit;
      end;

			LLog := MainLog;
			MainLog := nil;
			try
			//	FFile.Write(Line);
				LineA := ConvertUnicodeToUTF8(Line);
				FFile.BlockWrite(LineA[1], Length(LineA));
			finally
				MainLog := LLog;
			end;
		end
		else
		begin
      if Length(FData) + Length(Line) > MaxLogFileSize then
      begin
        FLoggingLevel := mlNone;
      	Exit;
      end;
			FData := FData + Line;
		end;
	end;
  FInWrite := False;
end;

const
	IdLine = ';Local Date Time	Type	Message' + FileSep;

constructor TLog.Create(const FileName: TFileName; const LogLevel: TMessageLevel = DefaultLoggingLevel; const DirectWrite: BG = True; const MaxLogFiles: SG = 0);
var
	NewFileName: TFileName;
	Instance: SG;
	DeleteOptions: TDeleteOptions;
begin
	inherited Create;
	FLoggingLevel := LogLevel;
  FLastMessageLevel := mlNone;
	FFileName := FileName;
	FDirectWrite := DirectWrite;
  FMaxLogFiles := MaxLogFiles;

	FFile := TFile.Create;
  NewFileName := FFileName;
	for Instance := 1 to 9 do
	begin
		if FFile.Open(NewFileName, fmAppend) then Break;
		NewFileName := DelFileExt(FFileName) +  IntToStr(Instance) + ExtractFileExt(FFileName);
		if Instance = 9 then Exit;
	end;

	if FFile.FileSize >= MinLogFileSize then
	begin
		FFile.Close;
		NewFileName := DelFileExt(FFileName) + '_' + DateToS(FileTimeToDateTime(GetFileModified(FFileName)), ofIO) + ExtractFileExt(FFileName);
		if FileExists(NewFileName) = False then
		begin
			RenameFileEx(FFileName, NewFileName);
      if MaxLogFiles > 0 then
      begin
				DeleteOptions.Mask := '*.log';
        DeleteOptions.MaxDirs := MaxLogFiles;
        DeleteOptions.SelectionType := stOld;
        DeleteOptions.AcceptFiles := True;
        DeleteOptions.Test := False;
        DeleteOptions.DisableLog := True;
      	SxDeleteDirs(ExtractFilePath(FFileName), DeleteOptions);
			end;
		end;
		FFile.Open(FFileName, fmAppend);
	end;

	if FFile.FileSize = 0 then
		WriteLine({';' + ExtractFileName(FFileName) + FileSep + }IdLine); // First line
	Add('Started'{$ifndef Console} + ' Version ' + GetProjectInfo(piFileVersion){$endif}, mlInformation);
end;

var
  InitializingLog: BG; // or DeinitializingLog

destructor TLog.Destroy;
begin
  InitializingLog := True;
	Add('Finished', mlInformation);
	if Assigned(FFile) then
	begin
		Flush;
		FFile.Close;
		FreeAndNil(FFile);
	end;
	FFileName := '';
	inherited;
end;

procedure TLog.Add(const Line: string; const MessageLevel: TMessageLevel);
begin
	if MessageLevel >= FLoggingLevel then
  	Add(Now, Line, MessageLevel);
end;

procedure TLog.Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel);
begin
	Assert(MessageLevel <> mlNone);
	if MessageLevel >= FLoggingLevel then
	begin
		WriteLine(DateTimeToS(LogTime, 3, ofIO) + CharTab + FirstChar(MessageLevelStr[MessageLevel]) + CharTab + AddEscape(Line, True) + FileSep);
//		Flush;
	end;
end;

procedure TLog.Flush;
var LLog: TLog;
begin
	LLog := MainLog;
	MainLog := nil;
	try
		if FDirectWrite then
		begin
			FFile.FlushFileBuffers;
		end
		else
		begin
			FFile.Write(FData);
			FData := '';
			FFile.FlushFileBuffers;
		end;
	finally
		MainLog := LLog;
	end;
end;

procedure TLog.Add(const Line: string);
begin
  Add(Line, FLastMessageLevel);
end;

procedure TLog.Add(const LogTime: TDateTime; const Line: string);
begin
  Add(LogTime, Line, FLastMessageLevel);
end;

function TLog.FatalError: BG;
begin
  Result := False;
  if mlFatalError >= MainLog.FLoggingLevel then
  begin
    MainLog.FLastMessageLevel := mlFatalError;
    Result := True;
  end;
end;

function TLog.Error: BG;
begin
  Result := False;
  if mlError >= MainLog.FLoggingLevel then
  begin
    MainLog.FLastMessageLevel := mlError;
    Result := True;
  end;
end;

function TLog.Warning: BG;
begin
  Result := False;
  if mlWarning >= MainLog.FLoggingLevel then
  begin
    FLastMessageLevel := mlWarning;
    Result := True;
  end;
end;

function TLog.Information: BG;
begin
  Result := False;
  if mlInformation >= MainLog.FLoggingLevel then
  begin
    FLastMessageLevel := mlInformation;
    Result := True;
  end;
end;

function TLog.Debug: BG;
begin
  Result := False;
  if mlDebug >= MainLog.FLoggingLevel then
  begin
    FLastMessageLevel := mlDebug;
    Result := True;
  end;
end;

function TLog.Confirmation: BG;
begin
  Result := False;
  if mlConfirmation >= MainLog.FLoggingLevel then
  begin
    FLastMessageLevel := mlConfirmation;
    Result := True;
  end;
end;

function StrIndex(const s: string; const AStrings: array of string): SG;
var i: SG;
begin
	Result := -1;
	for i := 0 to Length(AStrings) - 1 do
		if s = AStrings[i] then
		begin
			Result := i;
			Break;
		end;
end;

function GetLoggingLevel(const s: string): TMessageLevel;
var Index: SG;
begin
	Result := DefaultLoggingLevel;
	if s = '' then Exit;
	if (s[1] = '0') then
		Result := mlNone
	else
	begin
		Index := StrIndex(CapitalCase(s), MessageLevelStr);
		if (Index >= SG(Low(Result))) and (Index <= SG(High(Result))) then
			Result := TMessageLevel(Index);
	end;
end;

function MainLogWrite(const AMessageLevel: TMessageLevel): BG;
begin
	Result := False;
  if Assigned(MainLog) then
    if (AMessageLevel >= MainLog.LoggingLevel) then
    begin
      MainLog.FLastMessageLevel := AMessageLevel;
      Result := True;
    end;
end;

procedure MainLogAdd(const ALine: string; const AMessageLevel: TMessageLevel);
begin
  if InitializingLog then Exit;

	if Assigned(MainLog) then
  	if AMessageLevel >= MainLog.FLoggingLevel then
	    MainLog.Add(ALine, AMessageLevel);
end;

procedure SetLoggingLevel(const Value: string);
begin
	MainLog.LoggingLevel := GetLoggingLevel(Value);
end;

procedure LogAdd(const AMessage: string);
begin
  if Assigned(MainLog) then
    MainLog.Add(AMessage, MainLog.FLastMessageLevel);
end;

function LogFatalError: BG;
begin
  Result := False;
  if Assigned(MainLog) then
  	if mlFatalError >= MainLog.FLoggingLevel then
    begin
      MainLog.FLastMessageLevel := mlFatalError;
      Result := True;
    end;
end;

function LogError: BG;
begin
  Result := False;
  if Assigned(MainLog) then
  	if mlError >= MainLog.FLoggingLevel then
    begin
      MainLog.FLastMessageLevel := mlError;
      Result := True;
    end;
end;

function LogWarning: BG;
begin
  Result := False;
  if Assigned(MainLog) then
  	if mlWarning >= MainLog.FLoggingLevel then
    begin
      MainLog.FLastMessageLevel := mlWarning;
      Result := True;
    end;
end;

function LogInformation: BG;
begin
  Result := False;
  if Assigned(MainLog) then
  	if mlInformation >= MainLog.FLoggingLevel then
    begin
      MainLog.FLastMessageLevel := mlInformation;
      Result := True;
    end;
end;

function LogDebug: BG;
begin
  Result := False;
  if Assigned(MainLog) then
  	if mlDebug >= MainLog.FLoggingLevel then
    begin
      MainLog.FLastMessageLevel := mlDebug;
      Result := True;
    end;
end;

function LogConfirmation: BG;
begin
  Result := False;
  if Assigned(MainLog) then
  	if mlConfirmation >= MainLog.FLoggingLevel then
    begin
      MainLog.FLastMessageLevel := mlConfirmation;
      Result := True;
    end;
end;

procedure InitializeLog;
var
	i: SG;
	s: string;
begin
  InitializingLog := True;
	s := '';
	for i := 0 to Length(MessageLevelStr) - 1 do
	begin
		s := s + MessageLevelStr[TMessageLevel(i)] + '|';
	end;
	s := DelLastChar(s);
	RegisterParam('Log', 'Logging level: Log[' + s + ']', SetLoggingLevel);

  InitPaths;
	CreateDirEx(ExtractFilePath(MainLogFileName));
	MainLog := TLog.Create(MainLogFileName, mlInformation, True, 16); //, GetLoggingLevel(GetParamValue('LOG')), True);
  InitializingLog := False;
end;

initialization

finalization
{$IFNDEF NoFinalization}
	FreeAndNil(MainLog);
{$ENDIF NoFinalization}
end.
