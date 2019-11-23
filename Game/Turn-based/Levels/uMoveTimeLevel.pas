unit uMoveTimeLevel;

interface

uses
  uTypes,
  uCustomLevel,
  uTimeLevel,
  uTimeSpan;

type
  TMoveTimeLevel = class(TTimeLevel)
  private
    FValueType: TLevelValueType;
    FValue: TTimeSpan;
    procedure SetValueType(const Value: TLevelValueType);
    procedure SetValue(const Value: TTimeSpan);
  protected
    function CanStopOnNode: BG; override;
    function CanStopOnDepth: BG; override;
    function CanStopOnMove: BG; override;
  public
    constructor Create;

    property Value: TTimeSpan read FValue write SetValue;
    property ValueType: TLevelValueType read FValueType write SetValueType;

    function GetAsString: string; override;
  end;

implementation

{ TMoveTimeLevel }

function TMoveTimeLevel.CanStopOnDepth: BG;
const
  AvgFactor = 2;
begin
  Result := False;
  if (AnalysisInfo.Depth >= 1) then
  begin
    case ValueType of
    lvtMinimal:
      Result := AnalysisInfo.ElapsedTime.Elapsed.Ticks >= Value.Ticks;
    lvlAverage:
      Result := AnalysisInfo.EstimateNextDepthElapsedTime.Ticks + AnalysisInfo.ElapsedTimeWithAverageMoveOverhead.Ticks > AvgFactor * Value.Ticks;
    lvtMaximal:
      Result := AnalysisInfo.EstimateNextDepthElapsedTime.Ticks + AnalysisInfo.ElapsedTimeWithAverageMoveOverhead.Ticks > Value.Ticks;
    end;
  end;
end;

function TMoveTimeLevel.CanStopOnMove: BG;
begin
  if ValueType = lvtEqual then
    Result := False
  else
    Result := inherited;
end;

function TMoveTimeLevel.CanStopOnNode: BG;
begin
  Result :=
    (ValueType in [lvtEqual, lvtMaximal]) and (AnalysisInfo.ElapsedTimeWithAverageMoveOverhead.Ticks >= Value.Ticks)
end;

constructor TMoveTimeLevel.Create;
begin
  inherited;

  FValueType := lvtEqual;
end;

function TMoveTimeLevel.GetAsString: string;
begin
  Result := 'Move Time: ' + FValue.ToStringInSeconds;
end;

procedure TMoveTimeLevel.SetValue(const Value: TTimeSpan);
begin
  FValue := Value;
end;

procedure TMoveTimeLevel.SetValueType(const Value: TLevelValueType);
begin
  FValueType := Value;
end;

end.
