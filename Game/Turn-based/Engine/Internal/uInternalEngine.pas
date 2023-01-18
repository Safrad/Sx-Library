unit uInternalEngine;

interface

uses
  uTypes,
  uSxThreadTimer,
  uPowerRequest,

  uTimeSpan,
  uGameVariant,
  uCommonEngine,
  uCommonEngineOptions,
  uEngineOutput,
  uStopManager,
  uDrawManager,
  uAnalysis,
  uOpponent;

type
	TInternalEngine = class(TCommonEngine)
	private
    FGameVariant: TGameVariant;
    FDoMoveAfterStop: BG;
    FOneSecondTimer: TSxThreadTimer;
    FCommonOptions: TCommonEngineOptions;
    FStopManager: TStopManager;
    FDrawManager: TDrawManager;
    FOutput: TEngineOutput;
    FOpponent: TOpponent;

    procedure SetDoMoveAfterStop(const Value: BG);
    procedure SetOpponent(const Value: TOpponent);
    procedure AverageMoveOverheadOnChange(Sender: TObject);
    procedure MaximalMoveOverheadOnChange(Sender: TObject);
    procedure AddOnChangeToOptions;
    procedure CreateCommonOptions;
    procedure ManageDrawOffer(var Analysis: PAnalysis);
  protected
    procedure SetOutput(const Value: TEngineOutput); virtual;
    function GetRemainMoveCount: SG; virtual; abstract;
    procedure SetGameVariant(const Value: TGameVariant); virtual;
	public
    constructor Create;
    destructor Destroy; override;

    property GameVariant: TGameVariant read FGameVariant write SetGameVariant;

    property Output: TEngineOutput read FOutput write SetOutput;
    property Opponent: TOpponent read FOpponent write SetOpponent;
    procedure OpponentRequireDraw;

    procedure Start; override;
    procedure DoBestMove; virtual;
    procedure Stop; override;

    procedure Undo; virtual;

    procedure EngineStateToConsole; virtual;
    procedure EvalToConsole; virtual; abstract;
    procedure WriteBoardToConsole; virtual; abstract;
    procedure WaitForCalculationDone(const ATimeOutInMilliseconds: U8); override;
    procedure SetHashForMoveTime(const ATimeSpan: TTimeSpan);
    procedure SetNumberOfHashEntries(const AHashEntries: U8); virtual; abstract;


    function GetCurrentLine: string; virtual; abstract;

    property DoMoveAfterStop: BG read FDoMoveAfterStop write SetDoMoveAfterStop;

    property CommonOptions: TCommonEngineOptions read FCommonOptions;
    property StopManager: TStopManager read FStopManager;
	end;

implementation

uses
  SysUtils,
  Math,

  uStrings,
  uProjectInfo,
  uMainLog,
  uStopwatch,

  uScore,
  uTimeLevel,
  uMoveTimeLevel,
  uTimeControlLevel,
  uOneSecondTimer;

{ TInternalEngine }

procedure TInternalEngine.SetDoMoveAfterStop(const Value: BG);
begin
  FDoMoveAfterStop := Value;
end;

procedure TInternalEngine.SetGameVariant(const Value: TGameVariant);
begin
  if FGameVariant <> Value then
  begin
    FGameVariant := Value;
    if Output <> nil then
    begin
      if Value <> nil then
      begin
        Output.TellGUIInfo('Game variant changed to: ' + Value.Names[0])
      end
      else
      begin
        Output.TellGUIInfo('Game variant changed to: ' + NAStr);
      end;
    end;
  end;
end;

procedure TInternalEngine.SetOpponent(const Value: TOpponent);
begin
  FOpponent := Value;
end;

procedure TInternalEngine.SetOutput(const Value: TEngineOutput);
begin
  FOutput := Value;

  FStopManager.EngineOutput := Value;
end;

constructor TInternalEngine.Create;
begin
	inherited;

	Title := GetProjectInfo(piProductName) + ' ' + GetProjectInfo(piProductVersion);

  CreateCommonOptions;

  AddOnChangeToOptions;

  FOneSecondTimer := TOneSecondTimer.Create;
  TOneSecondTimer(FOneSecondTimer).InternalEngine := Self;

  FStopManager := TStopManager.Create;
  FStopManager.LevelManager := LevelManager;
  FStopManager.AnalysisInfo := AnalysisInfo;

  FDrawManager := TDrawManager.Create;
end;

destructor TInternalEngine.Destroy;
begin
  try
    FDrawManager.Free;
    FStopManager.Free;
    FOneSecondTimer.Free;
    FCommonOptions.Free;
    FreeAndNil(FOutput);
  finally
    inherited;
  end;
end;

procedure TInternalEngine.DoBestMove;
var
  Analysis: PAnalysis;
begin
  FOneSecondTimer.Enabled := False;
  AnalysisInfo.ElapsedTime.Stop;
  Output.ShowBestMove(CommonOptions.Ponder.Value);
  if DoMoveAfterStop then
  begin
    Analysis := AnalysisInfo.LastAnalysis;
    ManageDrawOffer(Analysis);
    Output.DoImportMove;
    if Analysis <> nil then
    begin
      DoMove(AnalysisInfo.BestMove);
    end;
  end;
  OnStop(Self);
end;

procedure TInternalEngine.EngineStateToConsole;
begin
  inherited;

  Options.WriteValuesToCommonOutput;
  Output.TellGUIInfo('Analyze Mode: ' + FalseTrue[SG(not FDoMoveAfterStop)]);
  Output.TellGUIInfo('Infinite Analysis: ' + FalseTrue[SG(LevelManager.InfiniteAnalysis)]);
  if LevelManager.MyLevel <> nil then
    Output.TellGUIInfo('Level: ' + LevelManager.MyLevel.GetAsString);
end;

procedure TInternalEngine.AddOnChangeToOptions;
begin
  FCommonOptions.AverageMoveOverhead.OnChange := AverageMoveOverheadOnChange;
  AverageMoveOverheadOnChange(nil);
  FCommonOptions.MaximalMoveOverhead.OnChange := MaximalMoveOverheadOnChange;
  MaximalMoveOverheadOnChange(nil);
end;

procedure TInternalEngine.AverageMoveOverheadOnChange(Sender: TObject);
begin
  AnalysisInfo.AverageMoveOverhead.Milliseconds := FCommonOptions.AverageMoveOverhead.Value;
end;

procedure TInternalEngine.MaximalMoveOverheadOnChange(Sender: TObject);
begin
  AnalysisInfo.MaximalMoveOverhead.Milliseconds := FCommonOptions.MaximalMoveOverhead.Value;
end;

procedure TInternalEngine.OpponentRequireDraw;
begin
  if FDrawManager.AcceptDraw then
    Output.AcceptDraw;
end;

procedure TInternalEngine.SetHashForMoveTime(const ATimeSpan: TTimeSpan);
const
  NodesPerSecond = 4 * 1000 * 1000; // TODO : Calculate
begin
  SetNumberOfHashEntries(Round(ATimeSpan.SecondsAsF * FG(NodesPerSecond)));
end;

procedure TInternalEngine.Start;
begin
  try
    inherited;

    Output.Start;

    StopManager.Start;

    if (LevelManager.InfiniteAnalysis = False) then
    begin
      if (LevelManager.MyLevel = nil) then
        raise Exception.Create('No level is set.');
      if LevelManager.MyLevel is TTimeControlLevel then
      begin
        if TTimeControlLevel(LevelManager.MyLevel).MoveCount = 0 then
          TTimeControlLevel(LevelManager.MyLevel).MoveCount := GetRemainMoveCount;
        TTimeControlLevel(LevelManager.MyLevel).IncrementTime := LevelManager.MyRemainTime;
        TTimeControlLevel(LevelManager.MyLevel).MoveIndex := 0;
        if (LevelManager.OpponentLevel <> nil) and (LevelManager.OpponentLevel is TTimeControlLevel) then
        begin
          TTimeControlLevel(LevelManager.OpponentLevel).IncrementTime := LevelManager.OpponentRemainTime;
          TTimeControlLevel(LevelManager.OpponentLevel).MoveIndex := 0;
        end;
        Output.TellGUIInfo(TTimeLevel(LevelManager.MyLevel).GetAsString);
      end;

      // Set Automatic Hash Size
      if FCommonOptions.AutomaticHashSize.Value = True then
      begin
        if LevelManager.MyLevel is TMoveTimeLevel then
        begin
          SetHashForMoveTime(TMoveTimeLevel(LevelManager.MyLevel).Value);
        end
        else if LevelManager.MyLevel is TTimeLevel then
        begin
          SetHashForMoveTime(TTimeLevel(LevelManager.MyLevel).MaximalTime);
        end
        else
        begin
          FCommonOptions.HashSizeInMB.OnChange(Self);
        end;
      end;
    end;

    AnalysisInfo.ElapsedTime.Restart;
    FOneSecondTimer.Enabled := True;

    if CommonOptions.OwnBook.Value then
      BookMoves;
    if (AnalysisInfo.AnalysisCount > 0) and (LevelManager.InfiniteAnalysis = False) then
    begin
      raise EAbort.Create('Book move found.');
    end;
  except
    on E: Exception do
    begin
      DoBestMove;
      raise;
    end;
  end;
end;

procedure TInternalEngine.Stop;
begin
  StopManager.Abort;

	inherited;
end;

procedure TInternalEngine.Undo;
begin
  RootMoves.Clear;
end;

procedure TInternalEngine.WaitForCalculationDone(const ATimeOutInMilliseconds: U8);
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
    else
      ElapsedTime.Stop;
  finally
    ElapsedTime.Free;
  end;
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogLeave('WaitForBestMove');
end;

procedure TInternalEngine.ManageDrawOffer(var Analysis: PAnalysis);
begin
  if (Analysis <> nil) and (Analysis.Status.ScoreBound in [sbExact, sbLower]) then
  begin
    FDrawManager.ContemptValue := CommonOptions.ContemptValue.Value;
    FDrawManager.Update(AnalysisInfo.LastAnalysis.Status.Score);
    if Output.DebugMode and (FDrawManager.OfferProbability > 0) then
      Output.TellGUIDebug('Draw offer probability: 1 / ' + IntToStr(FDrawManager.OfferProbability));
    if FDrawManager.OfferDraw then
    begin
      // Engine offers a draw
      Output.OfferDraw;
    end;
  end
  else
  begin
    FDrawManager.Clear;
  end;
end;

procedure TInternalEngine.CreateCommonOptions;
begin
  FCommonOptions := TCommonEngineOptions.Create;
  Options.Add(FCommonOptions.Ponder);
  Options.Add(FCommonOptions.OwnBook);
  Options.Add(FCommonOptions.BookFile);
  Options.Add(FCommonOptions.ContemptValue);
  Options.Add(FCommonOptions.RandomPlay);
  Options.Add(FCommonOptions.RandomValue);
  FCommonOptions.AddTimeManagementOptions(Options);
  Options.Add(FCommonOptions.ShowCurrLine);
end;

end.

