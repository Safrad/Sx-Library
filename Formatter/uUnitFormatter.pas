unit uUnitFormatter;

interface

uses
  uTypes,
  uNumberFormatter;

type
  TUnitFormatter = class(TNumberFormatter)
  private
    FUnitName: string;
    procedure SetUnitName(const Value: string);
  public
    function Format(const AValue: S8): string; override;
    function Format(const AValue: FG): string; override;

    property UnitName: string read FUnitName write SetUnitName;
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
  Result := inherited Format(PrefixedResult.Value) + CharSpace + PrefixedResult.Prefix + UnitName;
end;

function TUnitFormatter.Format(const AValue: FG): string;
var
  PrefixedResult: TPrefixedValue;
begin
  PrefixedResult := TMetricPrefix.PrefixedValue(AValue);
  Result := inherited Format(PrefixedResult.Value) + CharSpace + PrefixedResult.Prefix + UnitName;
end;

procedure TUnitFormatter.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

end.
