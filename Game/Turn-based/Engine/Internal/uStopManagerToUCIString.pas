unit uStopManagerToUCIString;

interface

uses
  uTypes,
  uStopManager;

function StopManagerToUCIString(const AStopManager: TLevelManager; const ASideToMove: SG): string;

implementation

uses
  SysUtils,
  uStrings,
  uMath,
  uTimeSpan,

  uCustomLevel,

  uInfiniteLevel,
  uMoveTimeLevel,
  uMaximalDepthLevel,
  uNodesLevel,
  uWinInLevel,
  uTimeControlLevel;

function StopManagerToUCIString(const AStopManager: TLevelManager; const ASideToMove: SG): string;
var
  winc, binc, wtime, btime: S8; // ms
begin
  if (AStopManager.InfiniteAnalysis) or (AStopManager.MyLevel = nil) then
  begin
    Result := 'infinite';
    Exit;
  end;

  if AStopManager.MyLevel is TInfiniteLevel then
    Result := 'infinite'
  else if AStopManager.MyLevel is TMoveTimeLevel then
  begin
    Result := 'movetime ' + IntToStr(TMoveTimeLevel(AStopManager.MyLevel).Value.Milliseconds);
    Assert(TMoveTimeLevel(AStopManager.MyLevel).ValueType = lvtEqual);
  end
  else if AStopManager.MyLevel is TMaximalDepthLevel then
    Result := 'depth ' + IntToStr(TMaximalDepthLevel(AStopManager.MyLevel).Value)
  else if AStopManager.MyLevel is TNodesLevel then
    Result := 'nodes ' + IntToStr(TNodesLevel(AStopManager.MyLevel).Value)
  else if AStopManager.MyLevel is TWinInLevel then
    Result := 'mate ' + IntToStr(TWinInLevel(AStopManager.MyLevel).Value)
  else if AStopManager.MyLevel is TTimeControlLevel then
  begin
    Result := 'movestogo ' + IntToStr(TTimeControlLevel(AStopManager.MyLevel).MoveCount);
    winc := TTimeControlLevel(AStopManager.MyLevel).MoveIncrementTime.Milliseconds;
    binc := TTimeControlLevel(AStopManager.OpponentLevel).MoveIncrementTime.Milliseconds;
    if ASideToMove and 1 <> 0 then
      Exchange(winc, binc);

    AppendStr(Result, ' winc ' + IntToStr(winc));
    AppendStr(Result, ' binc ' + IntToStr(binc));

    wtime := AStopManager.MyRemainTime.Milliseconds;
    btime := AStopManager.OpponentRemainTime.Milliseconds;

    if ASideToMove and 1 <> 0 then
      Exchange(wtime, btime);
    AppendStr(Result, ' wtime ' + IntToStr(wtime));
    AppendStr(Result, ' btime ' + IntToStr(btime));
  end
  else
    raise ENotSupportedException.Create('Level not supported.');
end;

end.
