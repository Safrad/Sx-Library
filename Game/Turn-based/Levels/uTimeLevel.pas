unit uTimeLevel;

interface

uses
  uTypes,
  uTimeSpan,
  uCustomLevel;

type
  TTimeLevel = class(TCustomLevel)
  private
    FFixedMoveTime: BG;
    procedure SetFixedMoveTime(const Value: BG);
  protected
    FAverageTime: TTimeSpan;
    FMaximalTime: TTimeSpan;
    FMinimalTime: TTimeSpan;
    function CanStopOnDepth: BG; override;
    function CanStopOnMove: BG; override;
    function CanStopOnNode: BG; override;
  public
    property MinimalTime: TTimeSpan read FMinimalTime;
    property MaximalTime: TTimeSpan read FMaximalTime;
    property AverageTime: TTimeSpan read FAverageTime;

    function GetAsString: string; override;
    property FixedMoveTime: BG read FFixedMoveTime write SetFixedMoveTime;
  end;

implementation

uses
  uAnalysisInfo;

{ TTimeLevel }

const
  MinimalCompleteDepth = 1;

function TTimeLevel.CanStopOnDepth: BG;
const
  AvgFactor = 2;
var
  ElapsedWithAverageMoveOverheadTicks: U8;
begin
  if (AnalysisInfo.Depth >= MinimalCompleteDepth) and (FFixedMoveTime = False) then
  begin
    ElapsedWithAverageMoveOverheadTicks := AnalysisInfo.ElapsedTimeWithAverageMoveOverhead.Ticks;
    Result :=
      (AnalysisInfo.ElapsedTime.Elapsed.Ticks >= MinimalTime.Ticks) and
      (
        (ElapsedWithAverageMoveOverheadTicks * AvgFactor > AverageTime.Ticks) or
        (ElapsedWithAverageMoveOverheadTicks >= MaximalTime.Ticks)
      );
  end
  else
    Result := False;
end;

function TTimeLevel.CanStopOnMove: BG;
const
  AvgFactor = 2;
var
  ElapsedWithAverageMoveOverheadTicks: U8;
begin
  if (AnalysisInfo.Depth >= MinimalCompleteDepth + 1) and
    (AnalysisInfo.ActualBestMoveIsBad = False) and
    (AnalysisInfo.ChangedMove = False) and
    (FFixedMoveTime = False) then
  begin
    ElapsedWithAverageMoveOverheadTicks := AnalysisInfo.ElapsedTimeWithAverageMoveOverhead.Ticks;
    Result :=
      (AnalysisInfo.ElapsedTime.Elapsed.Ticks >= MinimalTime.Ticks) and
      (ElapsedWithAverageMoveOverheadTicks > AvgFactor * AverageTime.Ticks);
  end
  else
    Result := False;
end;

function TTimeLevel.CanStopOnNode: BG;
var
  ElapsedWithAverageMoveOverheadTicks: U8;
begin
  if (AnalysisInfo.Nodes mod 1000 = 0) and (AnalysisInfo.Depth >= 2) then
  begin
    ElapsedWithAverageMoveOverheadTicks := AnalysisInfo.ElapsedTimeWithAverageMoveOverhead.Ticks;
    Result :=
      (AnalysisInfo.ElapsedTime.Elapsed.Ticks >= MinimalTime.Ticks) and
      (
        (ElapsedWithAverageMoveOverheadTicks >= MaximalTime.Ticks) or
        (FFixedMoveTime and (ElapsedWithAverageMoveOverheadTicks >= AverageTime.Ticks))
      );
  end
  else
    Result := False;
end;

function TTimeLevel.GetAsString: string;
begin
	Result := 'Time Limits: ';
		Result := Result + 'Min: ' + FMinimalTime.ToStringInSeconds;
		Result := Result + ' | Avg: ' + FAverageTime.ToStringInSeconds;
		Result := Result + ' | Max: ' + FMaximalTime.ToStringInSeconds;
end;

procedure TTimeLevel.SetFixedMoveTime(const Value: BG);
begin
  FFixedMoveTime := Value;
end;

end.
