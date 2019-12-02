unit uNodesLevel;

interface

uses
  uTypes,
  uCustomLevel;

type
  TNodesLevel = class(TCustomLevel)
  private
    FValueType: TLevelValueType;
    FValue: U8;
    procedure SetValue(const Value: U8);
    procedure SetValueType(const Value: TLevelValueType);
  protected
    function CanStopOnNode: BG; override;
    function CanStopOnDepth: BG; override;
  public
    function GetAsString: string; override;
    property Value: U8 read FValue write SetValue;
    property ValueType: TLevelValueType read FValueType write SetValueType;
  end;

implementation

uses
  SysUtils;

{ TNodesLevel }

function TNodesLevel.CanStopOnDepth: BG;
const
  AvgFactor = 2;
begin
  case ValueType of
  lvtMinimal:
    Result := (AnalysisInfo.Nodes >= Value);
  lvlAverage:
    Result := AnalysisInfo.EstimateNextDepthNodes + AnalysisInfo.Nodes > AvgFactor * Value;
  lvtMaximal:
    Result := AnalysisInfo.EstimateNextDepthNodes + AnalysisInfo.Nodes > Value;
  else
    Result := False;
  end;
end;

function TNodesLevel.CanStopOnNode: BG;
begin
  Result := (ValueType in [lvtEqual, lvtMaximal]) and (AnalysisInfo.Nodes >= Value);
end;

function TNodesLevel.GetAsString: string;
begin
  Result := 'Nodes ' + IntToStr(Value);
end;

procedure TNodesLevel.SetValue(const Value: U8);
begin
  FValue := Value;
end;

procedure TNodesLevel.SetValueType(const Value: TLevelValueType);
begin
  FValueType := Value;
end;

end.
