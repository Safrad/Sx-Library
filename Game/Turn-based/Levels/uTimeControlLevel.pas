unit uTimeControlLevel;

interface

uses
  uTypes,
  uTimeSpan,
  uTimeLevel;

type
  TTimeControlLevel = class(TTimeLevel)
  private
    FMoveIndex: UG;

    FMoveIncrementTime: TTimeSpan;
    FFreeTime: TTimeSpan;
    FIncrementTime: TTimeSpan;
    FMoveCount: UG;
    FTimeUsage: UG;
    FMaximalMoveOverhead: TTimeSpan;

    procedure InitTimeLimits;

    procedure SetMoveIndex(const Value: UG);
    procedure SetFreeTime(const Value: TTimeSpan);
    procedure SetIncrementTime(const Value: TTimeSpan);
    procedure SetMoveCount(const Value: UG);
    procedure SetMoveIncrementTime(const Value: TTimeSpan);
    procedure SetTimeUsage(const Value: UG);
    procedure SetMaximalMoveOverhead(const Value: TTimeSpan);
  public
    constructor Create;

    function GetAsString: string; override;

    property MoveIndex: UG read FMoveIndex write SetMoveIndex;
    property MoveCount: UG read FMoveCount write SetMoveCount;
    property IncrementTime: TTimeSpan read FIncrementTime write SetIncrementTime;
    property MaximalMoveOverhead: TTimeSpan read FMaximalMoveOverhead write SetMaximalMoveOverhead;
    property MoveIncrementTime: TTimeSpan read FMoveIncrementTime write SetMoveIncrementTime;
    property FreeTime: TTimeSpan read FFreeTime write SetFreeTime;
    property TimeUsage: UG read FTimeUsage write SetTimeUsage;
  end;

implementation

uses
  SysUtils,
  uMath,
  Math;

{ TTimeControlLevel }

constructor TTimeControlLevel.Create;
begin
  inherited;

  FTimeUsage := 100; // %
end;

function TTimeControlLevel.GetAsString: string;
begin
  Result := 'Time Control: ' +
    'Total Time: ' + FIncrementTime.ToStringInSeconds + ', ' +
    'Move Increment: ' + FMoveIncrementTime.ToStringInSeconds + ', ' +
    'Free Time: ' + FFreeTime.ToStringInSeconds + ', ' +
    'Remain Moves: ' + IntToStr(FMoveCount);
end;

procedure TTimeControlLevel.InitTimeLimits;
const
	AvgFactor = 3; // Calibrate
var
	RemainMoves: UG;
  RemainTime: U8;
begin
  RemainMoves := Max(1, RoundDivU8(100 * (FMoveCount - FMoveIndex), FTimeUsage));
  RemainTime := Max(0, S8(FIncrementTime.Ticks) - S8(FMaximalMoveOverhead.Ticks));
  FMaximalTime.Ticks :=
    RoundDivU8(AvgFactor * (RemainTime + U8(RemainMoves - 1) * FMoveIncrementTime.Ticks), RemainMoves);
  FAverageTime.Ticks := FMaximalTime.Ticks div AvgFactor;

  if FMaximalTime.Ticks > RemainTime then
    FMaximalTime.Ticks := RemainTime;
end;

procedure TTimeControlLevel.SetFreeTime(const Value: TTimeSpan);
begin
  FFreeTime := Value;
end;

procedure TTimeControlLevel.SetIncrementTime(const Value: TTimeSpan);
begin
  FIncrementTime := Value;
end;

procedure TTimeControlLevel.SetMaximalMoveOverhead(const Value: TTimeSpan);
begin
  FMaximalMoveOverhead := Value;
end;

procedure TTimeControlLevel.SetMoveCount(const Value: UG);
begin
  FMoveCount := Value;
end;

procedure TTimeControlLevel.SetMoveIncrementTime(const Value: TTimeSpan);
begin
  FMoveIncrementTime := Value;
end;

procedure TTimeControlLevel.SetMoveIndex(const Value: UG);
begin
  FMoveIndex := Value;
  InitTimeLimits;
end;

procedure TTimeControlLevel.SetTimeUsage(const Value: UG);
begin
  FTimeUsage := Value;
end;

end.
