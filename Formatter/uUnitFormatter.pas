unit uUnitFormatter;

interface

uses
  uTypes,
  uNumberFormatter;

type
  TPrefixType = (ptMetric, ptBinary);

  TUnitFormatter = class(TNumberFormatter)
  private
    FUnitName: string;
    FPrefixType: TPrefixType;
    procedure SetUnitName(const Value: string);
    procedure SetPrefixType(const Value: TPrefixType);
  public
    function Format(const AValue: S8): string; override;
    function Format(const AValue: FG): string; override;

    property UnitName: string read FUnitName write SetUnitName;
    property PrefixType: TPrefixType read FPrefixType write SetPrefixType;
  end;

implementation

uses
  uStrings,
  uUnitPrefix;

{ TUnitFormatter }

function TUnitFormatter.Format(const AValue: S8): string;
var
  PrefixedResult: TPrefixedResult;
begin
  PrefixedResult := PrefixedValue(AValue, uUnitPrefix.ptMetric);
  Decimals := 20;
  Result := inherited Format(PrefixedResult.Value) + CharSpace + PrefixedResult.Prefix + UnitName;
end;

function TUnitFormatter.Format(const AValue: FG): string;
begin
  Result := inherited Format(AValue) + CharSpace + UnitName;
end;

procedure TUnitFormatter.SetPrefixType(const Value: TPrefixType);
begin
  FPrefixType := Value;
end;

procedure TUnitFormatter.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

end.
