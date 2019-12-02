unit uLevelManagerToUCIString;

interface

uses
  uTypes,
  uLevelManager;

function LevelManagerToUCIString(const ALevelManager: TLevelManager; const ASideToMove: SG): string;

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

function LevelManagerToUCIString(const ALevelManager: TLevelManager; const ASideToMove: SG): string;
var
  winc, binc, wtime, btime: S8; // ms
begin
  if (ALevelManager.InfiniteAnalysis) or (ALevelManager.MyLevel = nil) then
  begin
    Result := 'infinite';
    Exit;
  end;

  if ALevelManager.MyLevel is TInfiniteLevel then
    Result := 'infinite'
  else if ALevelManager.MyLevel is TMoveTimeLevel then
  begin
    Result := 'movetime ' + IntToStr(TMoveTimeLevel(ALevelManager.MyLevel).Value.Milliseconds);
    Assert(TMoveTimeLevel(ALevelManager.MyLevel).ValueType = lvtEqual);
  end
  else if ALevelManager.MyLevel is TMaximalDepthLevel then
    Result := 'depth ' + IntToStr(TMaximalDepthLevel(ALevelManager.MyLevel).Value)
  else if ALevelManager.MyLevel is TNodesLevel then
    Result := 'nodes ' + IntToStr(TNodesLevel(ALevelManager.MyLevel).Value)
  else if ALevelManager.MyLevel is TWinInLevel then
    Result := 'mate ' + IntToStr(TWinInLevel(ALevelManager.MyLevel).Value)
  else if ALevelManager.MyLevel is TTimeControlLevel then
  begin
    Result := 'movestogo ' + IntToStr(TTimeControlLevel(ALevelManager.MyLevel).MoveCount);
    winc := TTimeControlLevel(ALevelManager.MyLevel).MoveIncrementTime.Milliseconds;
    binc := TTimeControlLevel(ALevelManager.OpponentLevel).MoveIncrementTime.Milliseconds;
    if ASideToMove and 1 <> 0 then
      Exchange(winc, binc);

    AppendStr(Result, ' winc ' + IntToStr(winc));
    AppendStr(Result, ' binc ' + IntToStr(binc));

    wtime := ALevelManager.MyRemainTime.Milliseconds;
    btime := ALevelManager.OpponentRemainTime.Milliseconds;

    if ASideToMove and 1 <> 0 then
      Exchange(wtime, btime);
    AppendStr(Result, ' wtime ' + IntToStr(wtime));
    AppendStr(Result, ' btime ' + IntToStr(btime));
  end
  else
    raise ENotSupportedException.Create('Level not supported.');

end;

end.
