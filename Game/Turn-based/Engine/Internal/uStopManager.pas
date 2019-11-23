unit uStopManager;

interface

uses
  uTypes,
  uTimeSpan,

  uStopCause,
  uCustomLevel,
  uAnalysisInfo,
  uEngineOutput,
  uLevelManager;

type
  TStopManager = class
  private
    const
      BreaksPerSecond = 60;
    var
    FCause: TStopCause;
    FAnalysisInfo: TAnalysisInfo;
    FEngineOutput: TEngineOutput;
    FMaxDepth: SG;
    FLevelMaxDepth: SG;
    FLimitMaxDepth: SG;
    FLevelManager: TLevelManager;
    FSleepAfterNodes: U8;
    FLimitNodesPerSecond: FG;
    procedure InternalSetCause(const Value: TStopCause);
    procedure SetAnalysisInfo(const Value: TAnalysisInfo);
    procedure SetEngineOutput(const Value: TEngineOutput);
    procedure SetLevelMaxDepth(const Value: SG);
    procedure SetLimitMaxDepth(const Value: SG);
    procedure SetLevelManager(const Value: TLevelManager);
    procedure SetLimitNodesPerSecond(const Value: FG);
  public
    constructor Create;

    // Input
    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
    property LevelManager: TLevelManager read FLevelManager write SetLevelManager;
    property EngineOutput: TEngineOutput read FEngineOutput write SetEngineOutput;

    property LevelMaxDepth: SG read FLevelMaxDepth write SetLevelMaxDepth;
    property LimitMaxDepth: SG read FLimitMaxDepth write SetLimitMaxDepth;
    property LimitNodesPerSecond: FG read FLimitNodesPerSecond write SetLimitNodesPerSecond;

    // Process
    procedure Start;
    procedure UpdateStopNode;
    procedure UpdateStopMove;
    procedure UpdateStopAnalysis;
    procedure UpdateStopDepth;
    procedure Abort;

    // Output
    property Cause: TStopCause read FCause;
  end;

implementation

uses
  SysUtils,

  uMath,

  uTimeLevel,
  uAnalysis,
  uScore;

{ TStopManager }

procedure TStopManager.Abort;
begin
  if FCause = scNone then
    InternalSetCause(scUserAbort);
end;

procedure TStopManager.Start;
begin
  FCause := scNone;

  FMaxDepth := FLevelMaxDepth;
end;

constructor TStopManager.Create;
const
  MaxDepth = 99;
begin
  inherited;

  FCause := scUserAbort;
  FLevelMaxDepth := MaxDepth;
  FLimitMaxDepth := MaxDepth;
end;

procedure TStopManager.InternalSetCause(const Value: TStopCause);
begin
  FCause := Value;
//  if EngineOutput.DebugMode then
//    EngineOutput.TellGUIDebug('Stop Cause: ' + StopCauseStrings[FCause]);
  if Assigned(EngineOutput) then
    EngineOutput.TellGUIInfo('Stop Cause: ' + StopCauseStrings[FCause]);
end;

procedure TStopManager.UpdateStopAnalysis;
var
  LastAnalysis: PAnalysis;
begin
  LastAnalysis := AnalysisInfo.LastAnalysis;
  if LastAnalysis <> nil then
  begin
    if LastAnalysis.Status.Score >= scWin0 then
    begin
      FMaxDepth := scWin - LastAnalysis.Status.Score - 1;
      EngineOutput.TellGUIInfo('Maximal Depth: ' + IntToStr(FMaxDepth));
      if (FLevelManager.InfiniteAnalysis = False) or (AnalysisInfo.Depth > FMaxDepth) then
      begin
        InternalSetCause(scWinScore);
        Exit;
      end;
    end;
  end;

  if FLevelManager.InfiniteAnalysis then Exit;

  if Assigned(FLevelManager.MyLevel) and FLevelManager.MyLevel.StopAnalysis then
  begin
    InternalSetCause(scLevelOnAnalysis);
    Exit;
  end;
end;

procedure TStopManager.UpdateStopDepth;
begin
  if AnalysisInfo.Depth >= FMaxDepth then
  begin
    InternalSetCause(scLevelOnDepth);
    Exit;
  end;

  if FLevelManager.InfiniteAnalysis then Exit;

  if AnalysisInfo.Depth >= FLimitMaxDepth then
  begin
    InternalSetCause(scLevelOnDepth);
    Exit;
  end;
  if AnalysisInfo.MoveCount = 1 then // Only Depth 0
  begin
    InternalSetCause(scNoChoise);
    Exit;
  end;
  if (AnalysisInfo.LastAnalysis <> nil) and (AnalysisInfo.LastAnalysis.Status.Score <= -scWin0) then
  begin
    InternalSetCause(scLoseScore);
    Exit;
  end;

  if FLevelManager.MyLevel.StopDepth then
  begin
    InternalSetCause(scLevelOnDepth);
    Exit;
  end;
end;

procedure TStopManager.UpdateStopNode;
const
  MaxSleep = 1 / BreaksPerSecond;
var
  SleepTime: TTimeSpan;
  ExpectedElapsedTime: FG;
  ElapsedTime: FG;
begin
  if FSleepAfterNodes <> 0 then
  begin
    if FAnalysisInfo.Nodes mod FSleepAfterNodes = 0 then
    begin
      ElapsedTime := FAnalysisInfo.ElapsedTime.Elapsed.SecondsAsF;
      Assert(ElapsedTime > 0);
      ExpectedElapsedTime := FAnalysisInfo.Nodes / FLimitNodesPerSecond;
      if ExpectedElapsedTime >= ElapsedTime + 0.001 then
      begin
        SleepTime.SecondsAsF := ExpectedElapsedTime - ElapsedTime;
//        Assert(SleepTime.SecondsAsF <= MaxSleep);
        if IsDebug then
          EngineOutput.TellGUIInfo('Sleep [ms]: ' + IntToStr(SleepTime.Milliseconds));

        if SleepTime.SecondsAsF > MaxSleep then
        begin
          SleepTime.SecondsAsF := MaxSleep;
        end;
        while AnalysisInfo.ElapsedTime.Elapsed.SecondsAsF < ExpectedElapsedTime do
        begin
          Sleep(SleepTime.Milliseconds); // Accuracy not required, safe CPU cycles
          //PreciseSleep(SleepTime);
          if (FLevelManager.InfiniteAnalysis = False) and FLevelManager.MyLevel.StopNode then
          begin
            InternalSetCause(scLevelOnNode);
            Exit;
          end;
        end;
      end;
    end;
  end;

  if (FLevelManager.InfiniteAnalysis = False) and FLevelManager.MyLevel.StopNode then
  begin
    InternalSetCause(scLevelOnNode);
    Exit;
  end;

{  if AnalysisInfo.Nodes >= MaximalNodes then
  begin
    InternalSetCause(scMaximalNodes);
  end
  if MyLevel is TTimeLevel then
  begin
    TimeLevel := TTimeLevel(MyLevel);
    if AnalysisInfo.Time >= TimeLevel.MaximalTime then
    begin
      InternalSetCause(scLevel);
    end;
  end;}
end;

procedure TStopManager.UpdateStopMove;
begin
  if FLevelManager.InfiniteAnalysis then Exit;

  if FLevelManager.MyLevel.StopMove then
  begin
    InternalSetCause(scLevelOnMove);
    Exit;
  end;
end;

procedure TStopManager.SetAnalysisInfo(const Value: TAnalysisInfo);
begin
  FAnalysisInfo := Value;
  if FLevelManager.MyLevel <> nil then
    FLevelManager.MyLevel.AnalysisInfo := Value;
  if FLevelManager.OpponentLevel <> nil then
    FLevelManager.OpponentLevel.AnalysisInfo := Value;
end;

procedure TStopManager.SetEngineOutput(const Value: TEngineOutput);
begin
  Assert(Value <> nil);
  FEngineOutput := Value;
end;

procedure TStopManager.SetLevelManager(const Value: TLevelManager);
begin
  FLevelManager := Value;
end;

procedure TStopManager.SetLevelMaxDepth(const Value: SG);
begin
  FLevelMaxDepth := Value;
end;

procedure TStopManager.SetLimitMaxDepth(const Value: SG);
begin
  FLimitMaxDepth := Value;
end;

procedure TStopManager.SetLimitNodesPerSecond(const Value: FG);
begin
  if FLimitNodesPerSecond <> Value then
  begin
    FLimitNodesPerSecond := Value;
    if FLimitNodesPerSecond > 0 then
      FSleepAfterNodes := Trunc(FLimitNodesPerSecond / BreaksPerSecond) + 1
    else
      FSleepAfterNodes := 0;
  end;
end;

end.
