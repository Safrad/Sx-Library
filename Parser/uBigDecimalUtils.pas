unit uBigDecimalUtils;

interface

uses
  Velthuis.BigDecimals,
  Velthuis.BigIntegers;

type
  TBigDecimalUtils = class
  private
    class var FUsedLowPrecisionCalculation: Boolean;
    class procedure SetUsedLowPrecisionCalculation(const Value: Boolean); static;
  public
    class function ToBigInteger(const AValue: BigDecimal): BigInteger;
    class function ToDouble(const AValue: BigDecimal): Double;
    {$ifndef CPUX64}
    class function ToExtended(const AValue: BigDecimal): Extended;
    {$endif}
    class function ToFloat(const AValue: BigDecimal): {$ifndef CPUX64}Extended{$else}Double{$endif};
    class function Exp(const AValue: BigDecimal): BigDecimal;
    class function Ln(const AValue: BigDecimal): BigDecimal; overload;
    class function Sqrt(const AValue: BigDecimal): BigDecimal;
    class function Power(const ABase, AExponent: BigDecimal): BigDecimal;
    class function IntPower(const ABase: BigDecimal; AExponent: Integer): BigDecimal;
    class function Divide(const ADivident, ADivisor: BigDecimal): BigDecimal; overload;
    // Used IEEE 754 FPU
    class property UsedLowPrecisionCalculation: Boolean read FUsedLowPrecisionCalculation write SetUsedLowPrecisionCalculation;
  end;

implementation

uses
  Math,
  uBigDecimalConsts;

{ TBigDecimalUtils }

class function TBigDecimalUtils.ToBigInteger(const AValue: BigDecimal): BigInteger;
var
  x: BigInteger;
begin
  x := BigInteger.Pow(10, System.Abs(AValue.Scale));
  if AValue.Scale > 0 then
	  Result := AValue.UnscaledValue div x
  else
  	Result := AValue.UnscaledValue * x;
end;

class function TBigDecimalUtils.ToDouble(const AValue: BigDecimal): Double;
const
  { i.e. absolute error for float pi constant:
    19: 6e19
    20: 2e19
    21: 0
  }
  MaxExtendedDigists = 21;
var
  Value: BigDecimal;
begin
  FUsedLowPrecisionCalculation := True;

  Value := AValue.RoundToPrecision(MaxExtendedDigists);
  // Power(10, -1) has low precision, i. e. 0.1 -> 0.10000003...
  {$ifdef CPUX64}
  Result := Value.UnscaledValue.AsDouble * Power10(1, -Value.Scale);
  {$else}
  Result := Value.UnscaledValue.AsExtended * Power10(1, -Value.Scale);
  {$endif}
end;

class function TBigDecimalUtils.ToFloat(const AValue: BigDecimal): {$ifndef CPUX64}Extended{$else}Double{$endif};
begin
  {$ifdef CPUX64}
  Result := ToDouble(AValue);
  {$else}
  Result := ToExtended(AValue);
  {$endif}
end;

{$ifndef CPUX64}
class function TBigDecimalUtils.ToExtended(const AValue: BigDecimal): Extended;
const
  { i.e. absolute error for float pi constant:
    19: 6e19
    20: 2e19
    21: 0
  }
  MaxExtendedDigists = 21;
var
  Value: BigDecimal;
begin
  FUsedLowPrecisionCalculation := True;

  Value := AValue.RoundToPrecision(MaxExtendedDigists);
  // Power(10, -1) has low precision, i. e. 0.1 -> 0.10000003...
  Result := Value.UnscaledValue.AsExtended * Power10(1, -Value.Scale);
end;
{$endif}

class function TBigDecimalUtils.Power(const ABase, AExponent: BigDecimal): BigDecimal;
var
  {$ifndef CPUX64}
  Base, Exponent: Extended;
  {$else}
  Base, Exponent: Double;
  {$endif}
begin
  if AExponent.Frac <> 0 then
  begin
    FUsedLowPrecisionCalculation := True;
    Base := ToFloat(ABase);
    Exponent := ToFloat(AExponent);
    if (Base = 10) and (System.Frac(Exponent) = 0) and (Abs(Exponent) <= MaxInt) then
      Result := Power10(1, System.Trunc(Exponent))
    else
      Result := Math.Power(Base, Exponent);
  end
  else if AExponent < 0 then
    Result := IntPower(Divide(1, ABase), -AExponent.Trunc)
  else
    Result := IntPower(ABase, AExponent.Trunc);
end;

class function TBigDecimalUtils.Exp(const AValue: BigDecimal): BigDecimal;
begin
  Result := Power(TBigDecimalConsts.NumberE, AValue);
end;

class function TBigDecimalUtils.IntPower(const ABase: BigDecimal; AExponent: Integer): BigDecimal;
begin
  Result := BigDecimal.IntPower(ABase, AExponent, Math.Max(ABase.Precision, BigDecimal.DefaultPrecision));
end;

class function TBigDecimalUtils.Ln(const AValue: BigDecimal): BigDecimal;
var
  R: Double;
begin
  FUsedLowPrecisionCalculation := True;
	R := BigInteger.Ln(AValue.UnscaledValue);
  if AValue.Scale > 0 then
	  R := R - BigInteger.Ln(BigInteger.Pow(10, AValue.Scale))
  else if AValue.Scale < 0 then
	  R := R + BigInteger.Ln(BigInteger.Pow(10, -AValue.Scale));
  Result := R;
end;

class function TBigDecimalUtils.Divide(const ADivident, ADivisor: BigDecimal): BigDecimal;
begin
  Result := BigDecimal.Divide(ADivident, ADivisor, Math.Max(Math.Max(ADivident.Precision, ADivisor.Precision), BigDecimal.DefaultPrecision));
end;

class function TBigDecimalUtils.Sqrt(const AValue: BigDecimal): BigDecimal;
begin
  Result := BigDecimal.Sqrt(AValue, Max(AValue.Precision, BigDecimal.DefaultPrecision));
end;

class procedure TBigDecimalUtils.SetUsedLowPrecisionCalculation(const Value: Boolean);
begin
  FUsedLowPrecisionCalculation := Value;
end;

end.
