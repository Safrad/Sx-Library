unit uNumberFormatter;

interface

uses
  uTypes,
  uFormatter;

type
  TNumberFormatter = class(TFormatter)
  private
    FDecimals: SG;
    procedure SetDecimals(const Value: SG);
  public
    function Format(const AValue: S8): string; override;
    function Format(const AValue: FG): string; override;

    property Decimals: SG read FDecimals write SetDecimals;
  end;

implementation

uses
  uOutputFormat;

{ TNumberFormatter }

function TNumberFormatter.Format(const AValue: FG): string;
begin
//  Result := FToS(AValue, Decimals);
  Result := FloatToDecimalString(AValue, 16, Decimals);
end;

function TNumberFormatter.Format(const AValue: S8): string;
begin
  Result := NToS(AValue, Decimals);
end;

procedure TNumberFormatter.SetDecimals(const Value: SG);
begin
  FDecimals := Value;
end;

end.
