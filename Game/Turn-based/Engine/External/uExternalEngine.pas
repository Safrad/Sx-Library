unit uExternalEngine;

interface

uses
  SysUtils,
  SyncObjs,

  uTypes,
  uStopwatch,
  uLogger,
  uCommonEngine,
  uCustomParser,
  uCustomEngineWriter,
  uPipedExternalApplication;

const
  NoTimeOut = 0;

type
  TExternalEngine = class(TCommonEngine)
  private
    FNoOutputStopwatch: TStopwatch;
    FLogger: TLogger;
		FFileName: TFileName;
		FExternalApp: TPipedExternalApplication;
    FPaused: BG;
    FParser: ICustomParser;
    FWriter: TCustomEngineWriter;
    FProtocolError: string;
    FCalculationDoneEvent: TEvent;

    procedure OnReadLine(const AText: string);

    /// <summary>Required for:
    /// 1) engine process freezes and command quit do not terminate process
    /// 2) termination take long time (freeing swapped memory)
    ///  </summary>
    procedure ForceClose;

    // Property setters
    procedure SetFileName(const Value: TFileName);
    procedure SetLogger(const Value: TLogger);
    procedure SetParser(const Value: ICustomParser);
    procedure SetWriter(const Value: TCustomEngineWriter);
    procedure SetProtocolError(const Value: string);
    function GetAllocatedMemory: U8;
  protected
    procedure DoMove(const AMove: string); override;
    procedure SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG); override;
  public
    constructor Create;
    destructor Destroy; override;

		procedure Initialize; override;
    procedure AutodetectProtocol;
		procedure InitializeUCI;
		procedure InitializeXBoard;
    procedure WaitForReady; override;
		procedure Finalize; override;

    procedure SetStartPos; override;
    procedure GetPerft(const ADepth: SG); override;

    procedure NewGame; override;
    procedure Start; override;
    procedure Stop; override;

    procedure Pause; override;

    /// <summary>Use const NoTimeOut for no time out</summary>
    procedure WaitForCalculationDone(const ATimeOutInMilliseconds: U8); override;

    // Input
		property FileName: TFileName read FFileName write SetFileName;
    property Parser: ICustomParser read FParser write SetParser;
    property Writer: TCustomEngineWriter read FWriter write SetWriter;

    // Process
		procedure SendCommand(const ACommand: string);
    procedure CalculationDone(Sender: TObject);

    // Output
    property Logger: TLogger read FLogger write SetLogger;
    property ProtocolError: string read FProtocolError write SetProtocolError;
    property AllocatedMemory: U8 read GetAllocatedMemory;
  end;

implementation

uses
  Math,
  Winapi.Windows,

  uChar,
  uStrings,
  uFiles,
  uMainLog,
  uStartupWindowState,
  uETimeOutException,

  uExternalEngineParser,
  uPerftParser,
  uUCIReaderInit,
  uXBoardReaderInit,
  uAutodetectEngineProtocol,
  uStartWriter,
  uUCIWriter,
  uXBoardWriter;

{ TExternalEngine }

procedure TExternalEngine.AutodetectProtocol;
begin
  FParser := TAutodetectEngineProtocol.Create;
  TExternalEngineParser(FParser).Engine := Self;
	// prefer xboard if both protocols are supported
  SendCommand('uci');
	SendCommand('xboard');
	SendCommand('protover 2');
end;

procedure TExternalEngine.CalculationDone;
begin
  FCalculationDoneEvent.SetEvent;
end;

constructor TExternalEngine.Create;
begin
  inherited;

  FNoOutputStopwatch := TStopwatch.Create;
  Options.OwnsObjects := True;
  FCalculationDoneEvent := TEvent.Create;
end;

destructor TExternalEngine.Destroy;
begin
  try
    try
      if Assigned(FExternalApp) then
      begin
        FreeAndNil(FExternalApp);
      end;
    finally
      FParser := nil;
    end;

    Assert(FWriter = nil, 'Forget to call Finalize');
    FreeAndNil(FWriter);
    FNoOutputStopwatch.Free;
    FCalculationDoneEvent.Free;
  finally
    inherited;
  end;
end;

procedure TExternalEngine.DoMove(const AMove: string);
begin
  inherited;

  FWriter.DoMove(AMove);
end;

procedure TExternalEngine.Finalize;
begin
  inherited;

  if FExternalApp <> nil then
  begin
    if FWriter <> nil then
    begin
  // Piranha quit sequence
  //    Sleep(100);
  //    FWriter.Quit;
      FWriter.Quit;
      FreeAndNil(FWriter);
    end;

    FExternalApp.WaitForTimeOut.Milliseconds := 250;
    try
      FExternalApp.WaitFor;
    except
      on E: Exception do
      begin
        ForceClose;
      end;
    end;
    FreeAndNil(FExternalApp);
  end;

  Terminated := True;
end;

procedure TExternalEngine.ForceClose;
begin
  if Mainlog.IsLoggerFor(mlError) then
    MainLog.Add('Forcing close ' + FFileName, mlError);
  FExternalApp.Terminate;
end;

function TExternalEngine.GetAllocatedMemory: U8;
begin
  if FExternalApp = nil then
    Result := 0
  else
    Result := FExternalApp.AllocatedMemory;
end;

procedure TExternalEngine.GetPerft(const ADepth: SG);
var
  LastParser: ICustomParser;
  PerftParser: TPerftParser;
begin
  LastParser := FParser;
  PerftParser := TPerftParser.Create;
  try
    PerftParser.OnDone := CalculationDone;
    FParser := PerftParser;
    if StartStr('Stochfish', Title) then
      SendCommand('go perft ' + IntToStr(ADepth))
    else
      SendCommand('perft ' + IntToStr(ADepth));

    FCalculationDoneEvent.WaitFor;
  finally
    FParser := LastParser;
    PerftParser.Free;
  end;
end;

procedure TExternalEngine.Initialize;
var
  StartupWindowState: TStartupWindowState;
begin
  inherited;
  IsReady := False;

	RaiseExceptionIfFileNotExists(FFileName);

	if FExternalApp = nil then
	begin
		FExternalApp := TPipedExternalApplication.Create;
		FExternalApp.FileName := ExpandDir(FFileName);
    FExternalApp.CurrentDirectory :=  ExtractFileDir(FFileName);
    StartupWindowState.WindowState  := hwsHidden;
    StartupWindowState.Active := False;
    FExternalApp.StartupWindowState := StartupWindowState;
    FExternalApp.OnReadLine := OnReadLine;
    FWriter := TStartWriter.Create;
    FWriter.Engine := Self;
	end;

	if not FExternalApp.Running then
  	FExternalApp.Execute;
end;

procedure TExternalEngine.InitializeUCI;
begin
  FreeAndNil(FWriter);

  FWriter := TUCIWriter.Create;
  FWriter.Engine := Self;

  FParser := TUCIReaderInit.Create;
  TExternalEngineParser(FParser).Engine := Self;

  SendCommand('uci');
  FWriter.IsReady;
end;

procedure TExternalEngine.InitializeXBoard;
begin
  FreeAndNil(FWriter);

  FWriter := TXBoardWriter.Create;
  FWriter.Engine := Self;

  FParser := TXBoardReaderInit.Create;
  TExternalEngineParser(FParser).Engine := Self;
  TXBoardReaderInit(FParser).XBoardWriter := TXBoardWriter(FWriter);

  SendCommand('xboard');
  SendCommand('protover 2');
end;

procedure TExternalEngine.NewGame;
begin
  inherited;

  FWriter.NewGame;
end;

procedure TExternalEngine.Pause;
begin
	if Assigned(FExternalApp) then
	begin
		if not FPaused then
			SuspendThread(FExternalApp.ProcessInformation.hThread)
		else
			ResumeThread(FExternalApp.ProcessInformation.hThread);
		FPaused := not FPaused;
	end;
	inherited;
end;

procedure TExternalEngine.OnReadLine(const AText: string);
begin
  FNoOutputStopwatch.Restart;

  if Assigned(FLogger) then
    FLogger.Add('Engine->: ' + AText, mlInformation);

  try
    if Assigned(FParser) then
      FParser.Parse(AText);
  except
    on E: Exception do
    begin
      FProtocolError := E.Message;
      if Assigned(FLogger) then
        FLogger.Add(E.Message, mlFatalError);
    end;
  end;
end;

procedure TExternalEngine.SendCommand(const ACommand: string);
begin
	if Assigned(FExternalApp) then
  begin
    FExternalApp.WriteLine(ACommand);
    if Assigned(FLogger) then
      FLogger.Add('->Engine: ' + ACommand, mlInformation);
  end;
end;

procedure TExternalEngine.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  Title := DelFileExt(ExtractFileName(Value));
end;

procedure TExternalEngine.SetLogger(const Value: TLogger);
begin
  FLogger := Value;
end;

procedure TExternalEngine.SetParser(const Value: ICustomParser);
begin
  FParser := Value;
end;

procedure TExternalEngine.SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG);
begin
  inherited;

  AnalysisInfo.StartPosition := AString;
  FWriter.SetPositionFromStringSpecific(AString, ASideToMove);
  if Pos(' b ', AString) <> 0 then
    ASideToMove := 1
  else
    ASideToMove := 0;
end;

procedure TExternalEngine.SetProtocolError(const Value: string);
begin
  FProtocolError := Value;
end;

procedure TExternalEngine.SetStartPos;
begin
  inherited;

  FWriter.SetStartPos;
end;

procedure TExternalEngine.SetWriter(const Value: TCustomEngineWriter);
begin
  FWriter := Value;
end;

procedure TExternalEngine.Start;
begin
  inherited;

  FWriter.Start;
end;

procedure TExternalEngine.Stop;
begin
  inherited;

  FWriter.Stop;

  AnalysisInfo.ElapsedTime.Stop;
end;

procedure TExternalEngine.WaitForCalculationDone(const ATimeOutInMilliseconds: U8);
var
  ElapsedTime: TStopwatch;
begin
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogEnter('WaitForBestMove');
  ElapsedTime := TStopwatch.Create;
  try
    ElapsedTime.Start;
    while True do
    begin
      if AnalysisInfo.ElapsedTime.IsRunning = False then
        Break;

      Sleep(10);
    end;

//    FCalculationDoneEvent.WaitFor(ATimeOutInMilliseconds);
//    FCalculationDoneEvent.ResetEvent;

    if not AnalysisInfo.ElapsedTime.IsRunning then
    begin
      if MainLog.IsLoggerFor(mlDebug) then
        MainLog.Add('Abort waiting for best move, it is found.', mlDebug);
    end
    else if Terminated then
    begin
      if MainLog.IsLoggerFor(mlWarning) then
        MainLog.Add('Abort waiting for best move, external engine has terminated.', mlWarning);
    end
    else if not FExternalApp.Running then
    begin
      if MainLog.IsLoggerFor(mlError) then
        MainLog.Add('Abort waiting for best move, external application has terminated.', mlError);
    end
    else
      ElapsedTime.Stop;
  finally
    ElapsedTime.Free;
  end;
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogLeave('WaitForBestMove');
end;

procedure TExternalEngine.WaitForReady;
const
  TotalTimeOut = 3000;
  NoOutputTimeOut = 500;
var
  TotalStopwatch: TStopwatch;
begin
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogEnter('WaitForReady');
  inherited;

  // TODO : Use TEvent
  TotalStopwatch := TStopwatch.Create;
  try
    TotalStopwatch.Start;
    FNoOutputStopwatch.Restart;
    // Required for first test correct time (initialization requires long time)
    while True do
    begin
      if IsReady then
      begin
        if MainLog.IsLoggerFor(mlDebug) then
          MainLog.Add('Is ready.', mlDebug);
        Break;
      end;
      if Terminated then
      begin
        if MainLog.IsLoggerFor(mlWarning) then
          MainLog.Add('Abort waiting for best move, external engine has terminated.', mlWarning);
        Break;
      end;
      if FNoOutputStopwatch.Elapsed.Milliseconds > NoOutputTimeOut then
        raise ETimeOutException.Create('No output time out (' + IntToStr(NoOutputTimeOut) + ' ms), no "uciok" or "feature done=1" response found.');
      if TotalStopwatch.Elapsed.Milliseconds > TotalTimeOut then
        raise ETimeOutException.Create('Total time out (' + IntToStr(TotalTimeOut) + ' ms), no "uciok" or "feature done=1" response found.');
      Sleep(LoopSleepTime);
    end;
    FNoOutputStopwatch.Stop;
  finally
    TotalStopwatch.Free;
  end;
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogLeave('WaitForReady');
end;

end.

