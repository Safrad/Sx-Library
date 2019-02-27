unit uBigDecimalConsts;

interface

uses
  Velthuis.BigDecimals;

type
  TBigDecimalConsts = class
  private
    class var CachedNumberE: BigDecimal;
    class var CachedNumberPi: BigDecimal;
  public
    class function NumberE: BigDecimal;

    class function NumberPi: BigDecimal;
  end;

implementation

uses
  uNumberEString,
  uNumberPiString;

class function TBigDecimalConsts.NumberE: BigDecimal;
begin
  if CachedNumberE.IsZero then
  begin
    CachedNumberE := BigDecimal(NumberEString);
  end;
	Result := CachedNumberE;
end;

class function TBigDecimalConsts.NumberPi: BigDecimal;
begin
  if CachedNumberPi.IsZero then
  begin
    CachedNumberPi := BigDecimal(NumberPiString);
  end;
	Result := CachedNumberPi;
end;

end.
