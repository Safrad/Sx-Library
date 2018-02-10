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
		procedure WriteLine(const Line: string);
    procedure UpdateLoggingLevelFromEnvironmentVariable;
	public
		constructor Create(const FileName: TFileName);
		destructor Destroy; override;

		procedure Add(const Line: string; const MessageLevel: TMessageLevel); override;
		procedure Add(const LogTime: TDateTime; const Line: string; const MessageLevel: TMessageLevel); override;

    function FatalError: BG;
    function Error: BG;
    function Warning: BG;
    function Information: BG;
    function Debug: BG;
    function Confirmation: BG;

    procedure LogEnter(const AName: string);
    procedure LogLeave(const AName: string);
    procedure LogException(const E: Exception);

		procedure Flush;

		property FileName: TFileName read FFileName;
		property LoggingLevel: TMessageLevel read FLoggingLevel write FLoggingLevel;
	end;


	procedure MainLogAdd(const ALine: string; const AMessageLevel: TMessageLevel);

  // Optimization purposes only
	function MainLogWrite(const AMessageLevel: TMessageLevel): BG;
  function LogFatalError: BG;
  function LogError: BG;
  function LogWarning: BG;
  function LogInformation: BG;
  function LogDebug: BG;
  function LogConfirmation: BG;

  procedure LogException(const E: Exception);

	procedure InitializeLog;

var
	MainLog: TLog;

implementation

uses
	uParams, uFiles, uCharset,
	uOutputFormat, uEscape, uStrings, uProjectInfo,
	Windows, TypInfo;

const
  MinLogFileSize = 4 * MB;
  MaxLogFileSize = 16 * MB; // > MinLogFileSize

var
	FCriticalSection: TRTLCriticalSection;

procedure TLog.WriteLine(const Line: string);
var
	LLog: TLog;
	LineA: AnsiString;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FInWrite then
    begin
      Exit;
    end;

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
  finally
    FInWrite := False;
    LeaveCriticalSection(FCriticalSection);
  end;
end;

const
	IdLine = ';Local Date Time	Type	Message' + FileSep;

constructor TLog.Create(const FileName: TFileName);

  function GetLogWritableFileName: TFileName;
  var
    Instance: SG;
    NewFileName: TFileName;
  begin
    Result := '';
    NewFileName := FileName;
    for Instance := 1 to 9 do
    begin
      if IsFileWritable(NewFileName) then Break;

      if Instance = 9 then Exit;
      NewFileName := DelFileExt(FileName) +  IntToStr(Instance) + ExtractFileExt(FileName);
    end;
    Result := NewFileName;
  end;

var
	NewFileName: TFileName;
//	DeleteOptions: TDeleteOptions;
begin
	inherited Create;
	FFileName := GetLogWritableFileName;
	FDirectWrite := True;
  if IsDebug then
    FLoggingLevel := mlDebug
  else
    FLoggingLevel := mlInformation;
  UpdateLoggingLevelFromEnvironmentVariable;

	if GetFileSizeU(FFileName) >= MinLogFileSize then
	begin
		NewFileName := DelFileExt(FFileName) + '_' + DateToS(FileTimeToDateTime(GetFileModified(FFileName)), ofIO) + ExtractFileExt(FFileName);
		if FileExists(NewFileName) = False then
		begin
			RenameFileEx(FFileName, NewFileName);
//      if MaxLogFiles > 0 then
//      begin
//				DeleteOptions.Mask := '*.log';
//        DeleteOptions.MaxDirs := MaxLogFiles;
//        DeleteOptions.SelectionType := stOld;
//        DeleteOptions.AcceptFiles := True;
//        DeleteOptions.Test := False;
//        DeleteOptions.DisableLog := True;
//      	SxDeleteDirs(ExtractFilePath(FFileName), DeleteOptions);
//			end;
		end;
	end;

	FFile := TFile.Create;
  if not FFile.Open(FFileName, fmAppend) then Exit;

	if FFile.FileSize = 0 then
		WriteLine({';' + ExtractFileName(FFileName) + FileSep + }IdLine); // First line
	Add('Started Version ' + GetProjectInfo(piFileVersion), mlInformation);
end;

var
  InitializingLog: BG; // or DeinitializingLog

destructor TLog.Destroy;
begin
//  InitializingLog := True;
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

function TLog.FatalError: BG;
begin
  Result := False;
  if mlFatalError >= MainLog.FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLog.Error: BG;
begin
  Result := False;
  if mlError >= MainLog.FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLog.Warning: BG;
begin
  Result := False;
  if mlWarning >= MainLog.FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLog.Information: BG;
begin
  Result := False;
  if mlInformation >= MainLog.FLoggingLevel then
  begin
    Result := True;
  end;
end;

procedure TLog.LogEnter(const AName: string);
const
  EnterPrefix = '>';
begin
  Add(EnterPrefix + AName, mlDebug);
end;

procedure TLog.LogLeave(const AName: string);
const
  LeavePrefix = '<';
begin
  Add(LeavePrefix + AName, mlDebug);
end;

procedure TLog.LogException(const E: Exception);
begin
  Add(E.Message + ' (' + E.ClassName + ')', mlFatalError);
end;

function TLog.Debug: BG;
begin
  Result := False;
  if mlDebug >= MainLog.FLoggingLevel then
  begin
    Result := True;
  end;
end;

function TLog.Confirmation: BG;
begin
  Result := False;
  if mlConfirmation >= MainLog.FLoggingLevel then
  begin
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
	Result := mlDebug;
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

function MainLogWrite(const AMessageLevel: TMessageLevel): BG;
begin
	Result := Assigned(MainLog) and (MainLog.LoggingLevel <= AMessageLevel);
end;

function LogFatalError: BG;
begin
  Result := Assigned(MainLog) and (MainLog.FLoggingLevel <= mlFatalError);
end;

function LogError: BG;
begin
  Result := Assigned(MainLog) and (MainLog.FLoggingLevel <= mlError);
end;

function LogWarning: BG;
begin
  Result := Assigned(MainLog) and (MainLog.FLoggingLevel <= mlWarning);
end;

function LogInformation: BG;
begin
  Result := Assigned(MainLog) and (MainLog.FLoggingLevel <= mlInformation);
end;

function LogDebug: BG;
begin
  Result := Assigned(MainLog) and (MainLog.FLoggingLevel <= mlDebug);
end;

function LogConfirmation: BG;
begin
  Result := Assigned(MainLog) and (MainLog.FLoggingLevel <= mlConfirmation);
end;

procedure LogException(const E: Exception);
begin
  if Assigned(MainLog) and (MainLog.FLoggingLevel <= mlFatalError) then
    MainLog.LogException(E);
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
	MainLog := TLog.Create(MainLogFileName);
  InitializingLog := False;
end;

procedure TLog.UpdateLoggingLevelFromEnvironmentVariable;
var
  Value: string;
begin
  Value := GetEnvironmentVariable('LoggingLevel');
  if Value <> '' then
    FLoggingLevel := GetLoggingLevel(Value);
end;

initialization
{$IFNDEF NoInitialization}
	InitializeCriticalSection(FCriticalSection);
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
	FreeAndNil(MainLog);
	DeleteCriticalSection(FCriticalSection);
{$ENDIF NoFinalization}
end.
