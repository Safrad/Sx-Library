unit uTicks;

interface

uses
  uTypes;

type
  TTicks = class
  private
    FMinimumValue: FG;
    FMaximumValue: FG;

    FTickStep: FG;
    FMinimumTickValue: FG;
    FMaximumTickValue: FG;
    FNoEdge: BG;
    procedure SetMaximumValue(const Value: FG);
    procedure SetMinimumValue(const Value: FG);
    procedure SetNoEdge(const Value: BG);
  public
    property MinimumValue: FG read FMinimumValue write SetMinimumValue;
    property MaximumValue: FG read FMaximumValue write SetMaximumValue;

    procedure UpdateTicks(const AMinimumTicksStep: FG);

    property TickStep: FG read FTickStep;
    property MinimumTickValue: FG read FMinimumTickValue;
    property MaximumTickValue: FG read FMaximumTickValue;
    property NoEdge: BG read FNoEdge write SetNoEdge;
  end;

implementation

uses
  uMath,
  Math;

{ TTicks }

procedure TTicks.UpdateTicks(const AMinimumTicksStep: FG);
const
  // Set best step for the range
  goodNormalizedSteps: array[0..6] of FG = (1, 1.5, 2, 2.5, 5, 7.5, 10 ); // keep the 10 at the end
  // Or use these if you prefer:  { 1, 2, 5, 10 };
  // 1, 1.5, 2, 3, 5, 7, 10
var
  epsilon, max, min, range: FG;
  stepCount: SG;
  roughStep: FG;
  stepPower: FG;
  normalizedStep: FG;
  goodNormalizedStep: FG;
  i: SG;
begin
  max := MaximumValue;
  min := MinimumValue;

  if NoEdge then
  begin
    // Minimal increment to avoid round extreme values to be on the edge of the chart
    epsilon := (max - min) / 1e6;
    Increment(max, epsilon);
    Decrement(min, epsilon);
  end;

  range := max - min;

  if AMinimumTicksStep = 0 then
  begin
    // Target number of values to be displayed (it may be less)
    stepCount := 20;

    // First approximation
    roughStep := range / (stepCount - 1);
  end
  else
    roughStep := AMinimumTicksStep;

  // Normalize rough step to find the normalized one that fits best
  stepPower := Math.Power(10, -Math.Floor(Math.Log10(Abs(roughStep))));
  normalizedStep := roughStep * stepPower;

  goodNormalizedStep := 0;
  for i := 0 to Length(goodNormalizedSteps) - 1 do
  begin
    if goodNormalizedSteps[i] >= normalizedStep then
    begin
      goodNormalizedStep := goodNormalizedSteps[i];
      Break;
    end;
  end;

  FTickStep := goodNormalizedStep / stepPower;

  // Determine the scale limits based on the chosen step.
  FMaximumTickValue := Math.Ceil(max / FTickStep) * FTickStep;
  FMinimumTickValue := Math.Floor(min / FTickStep) * FTickStep;
end;

procedure TTicks.SetMaximumValue(const Value: FG);
begin
  FMaximumValue := Value;
end;

procedure TTicks.SetMinimumValue(const Value: FG);
begin
  FMinimumValue := Value;
end;

procedure TTicks.SetNoEdge(const Value: BG);
begin
  FNoEdge := Value;
end;

end.
