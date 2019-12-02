unit uLevelManagerToXBoardString;

interface

uses
  uTypes,
  uLevelManager;

function LevelManagerToXBoardString(const ALevelManager: TLevelManager): string;

implementation

uses
  SysUtils,
  uTimeSpan,
  uStrings,
  uChar,
  uOutputFormat,

  uCustomLevel,

  uInfiniteLevel,
  uMoveTimeLevel,
  uMaximalDepthLevel,
  uNodesLevel,
  uWinInLevel,
  uTimeControlLevel;

function LevelManagerToXBoardString(const ALevelManager: TLevelManager): string;
begin
  if (ALevelManager.InfiniteAnalysis) or (ALevelManager.MyLevel = nil) then
  begin
    Result := 'analyze';
    Exit;
  end;

  if ALevelManager.MyLevel is TInfiniteLevel then
    Result := 'analyze'
  else if ALevelManager.MyLevel is TMoveTimeLevel then
  begin
    Result := 'st ' + TMoveTimeLevel(ALevelManager.MyLevel).Value.ToStringInSeconds;
    Assert(TMoveTimeLevel(ALevelManager.MyLevel).ValueType = lvtEqual);
  end
  else if ALevelManager.MyLevel is TMaximalDepthLevel then
    Result := 'sd ' + IntToStr(TMaximalDepthLevel(ALevelManager.MyLevel).Value)
  else if ALevelManager.MyLevel is TTimeControlLevel then
  begin
    Result := 'level ' +
      IntToStr(TTimeControlLevel(ALevelManager.MyLevel).MoveCount) + CharSpace +
      MsToStr(TTimeControlLevel(ALevelManager.MyLevel).IncrementTime.Milliseconds, diMSD, 0) + CharSpace +
      MsToStr(TTimeControlLevel(ALevelManager.MyLevel).MoveIncrementTime.Milliseconds, diMSD, 0);

    AppendStr(Result, LineSep + 'time ' + IntToStr(ALevelManager.MyRemainTime.Milliseconds div 10));
    AppendStr(Result, LineSep + 'otim ' + IntToStr(ALevelManager.OpponentRemainTime.Milliseconds div 10));
  end
  else
    raise ENotSupportedException.Create('Level not supported.');
end;

end.
