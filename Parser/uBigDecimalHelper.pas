unit uBigDecimalHelper;

interface

uses
  Velthuis.BigDecimals,
  Velthuis.BigIntegers;

type
  TBigDecimalHelper = record helper for BigDecimal
  public
	// Conversion
    function ToBigInteger: BigInteger;
    function ToDouble: Double;
    function ToFloat: {$ifndef CPUX64}Extended{$else}Double{$endif};
    {$ifndef CPUX64}
    function ToExtended: Extended;
    {$endif}

	// Basic functions
    function Power(const AExponent: BigDecimal): BigDecimal;
    function Divide(const ADivisor: BigDecimal): BigDecimal;
    function Exp: BigDecimal;
    function Ln: BigDecimal;

	// Hyperbolic functions
    function ArCosh: BigDecimal;
    function ArCoth: BigDecimal;
    function ArCsch: BigDecimal;
    function ArSech: BigDecimal;
    function ArSinh: BigDecimal;
    function ArTanh: BigDecimal;
    function Cosh: BigDecimal;
    function Coth: BigDecimal;
    function Csch: BigDecimal;
    function Sech: BigDecimal;
    function Sinh: BigDecimal;
    function Tanh: BigDecimal;
  end;

implementation

uses
  uBigDecimalUtils,
  uBigDecimalHyperbolic;

{ TBigDecimalHelper }

function TBigDecimalHelper.ArCosh: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.ArCosh(Self);
end;

function TBigDecimalHelper.ArCoth: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.ArCoth(Self);
end;

function TBigDecimalHelper.ArCsch: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.ArCsch(Self);
end;

function TBigDecimalHelper.ArSech: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.ArSech(Self);
end;

function TBigDecimalHelper.ArSinh: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.ArSinh(Self);
end;

function TBigDecimalHelper.ArTanh: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.ArTanh(Self);
end;

function TBigDecimalHelper.Cosh: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.Cosh(Self);
end;

function TBigDecimalHelper.Coth: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.Coth(Self);
end;

function TBigDecimalHelper.Csch: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.Csch(Self);
end;

function TBigDecimalHelper.Divide(const ADivisor: BigDecimal): BigDecimal;
begin
  Result := TBigDecimalUtils.Divide(Self{Dividend}, ADivisor);
end;

function TBigDecimalHelper.Exp: BigDecimal;
begin
  Result := TBigDecimalUtils.Exp(Self);
end;

function TBigDecimalHelper.Ln: BigDecimal;
begin
  Result := TBigDecimalUtils.Ln(Self);
end;

function TBigDecimalHelper.Power(const AExponent: BigDecimal): BigDecimal;
begin
  Result := TBigDecimalUtils.Power(Self, AExponent);
end;

function TBigDecimalHelper.Sech: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.Sech(Self);
end;

function TBigDecimalHelper.Sinh: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.Sinh(Self);
end;

function TBigDecimalHelper.Tanh: BigDecimal;
begin
  Result := TBigDecimalHyperbolic.Tanh(Self);
end;

function TBigDecimalHelper.ToBigInteger: BigInteger;
begin
  Result := TBigDecimalUtils.ToBigInteger(Self);
end;

function TBigDecimalHelper.ToDouble: Double;
begin
  Result := TBigDecimalUtils.ToDouble(Self);
end;

function TBigDecimalHelper.ToFloat: {$ifndef CPUX64}Extended{$else}Double{$endif};
begin
  Result := TBigDecimalUtils.ToFloat(Self);
end;

{$ifndef CPUX64}
function TBigDecimalHelper.ToExtended: Extended;
begin
  Result := TBigDecimalUtils.ToExtended(Self);
end;
{$endif}

end.
