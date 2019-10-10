unit uUnitFormatter;

interface

uses
  uTypes,
  uNumberFormatter;

type
  TUnitFormatter = class(TNumberFormatter)
  private
    FUnitNameSuffix: string;
    procedure SetUnitNameSuffix(const Value: string);
  public
    function Format(const AValue: S8): string; override;
    function Format(const AValue: FG): string; override;

    property UnitNameSuffix: string read FUnitNameSuffix write SetUnitNameSuffix;
  end;

implementation

uses
  uStrings,
  uUnitPrefix,
  uMetricPrefix;

{ TUnitFormatter }

function TUnitFormatter.Format(const AValue: S8): string;
var
  PrefixedResult: TPrefixedValue;
begin
  PrefixedResult := TMetricPrefix.PrefixedValue(AValue);
  Result := inherited Format(PrefixedResult.Value) + CharSpace + PrefixedResult.Prefix + UnitNameSuffix;
end;

function TUnitFormatter.Format(const AValue: FG): string;
var
  PrefixedResult: TPrefixedValue;
begin
  PrefixedResult := TMetricPrefix.PrefixedValue(AValue);
  Result := inherited Format(PrefixedResult.Value) + CharSpace + PrefixedResult.Prefix + UnitNameSuffix;
end;

procedure TUnitFormatter.SetUnitNameSuffix(const Value: string);
begin
  FUnitNameSuffix := Value;
end;

end.
