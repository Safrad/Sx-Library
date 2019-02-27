unit uBigDecimalHyperbolic;

interface

uses
  Velthuis.BigDecimals;

type
  TBigDecimalHyperbolic = class
  public
    // Hyperbolic sinus
    class function Sinh(const AValue: BigDecimal): BigDecimal;

    // Hyperbnoolic cosinus
    class function Cosh(const AValue: BigDecimal): BigDecimal;

    // Hyperbnoolic tangent
    class function Tanh(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic cosecant
    class function Csch(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic secant
    class function Sech(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic cotangent
    class function Coth(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic arcus sinus
    class function ArSinh(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic arcus cosinus
    class function ArCosh(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic arcus tangent
    class function ArTanh(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic arcus cosecant
    class function ArCsch(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic arcus secant
    class function ArSech(const AValue: BigDecimal): BigDecimal;

    // Hyperbolic arcus cotangent
    class function ArCoth(const AValue: BigDecimal): BigDecimal;
  end;

implementation

uses
  uBigDecimalUtils;

{ TBigDecimalHyperbolic }

class function TBigDecimalHyperbolic.ArCosh(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Ln(
      AValue + TBigDecimalUtils.Sqrt(AValue.Sqr - 1)
    );
end;

class function TBigDecimalHyperbolic.ArCoth(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    0.5 * TBigDecimalUtils.Ln(
      TBigDecimalUtils.Divide(
        AValue + 1,
        AValue - 1
      )
    );
end;

class function TBigDecimalHyperbolic.ArCsch(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Ln(
      TBigDecimalUtils.Divide(
        1,
        AValue
      )
      +
      TBigDecimalUtils.Sqrt(
        TBigDecimalUtils.Divide(
          1,
          AValue.Sqr
        ) + 1
      )
    );
end;

class function TBigDecimalHyperbolic.ArSech(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Ln(
      TBigDecimalUtils.Divide(
        1 + TBigDecimalUtils.Sqrt(1 - AValue.Sqr),
        AValue
      )
    );
end;

class function TBigDecimalHyperbolic.ArSinh(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Ln(
      AValue + TBigDecimalUtils.Sqrt(AValue.Sqr + 1)
    );
end;

class function TBigDecimalHyperbolic.ArTanh(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    0.5 * TBigDecimalUtils.Ln(
      TBigDecimalUtils.Divide(
        1 + AValue,
        1 - AValue
      )
    );
end;

class function TBigDecimalHyperbolic.Cosh(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Divide(
      1 + TBigDecimalUtils.Exp(-2 * AValue),
      2 * TBigDecimalUtils.Exp(-AValue)
    );
end;

class function TBigDecimalHyperbolic.Coth(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Divide(
      TBigDecimalUtils.Exp(2 * AValue) + 1,
      TBigDecimalUtils.Exp(2 * AValue) - 1
    );
end;

class function TBigDecimalHyperbolic.Csch(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Divide(
      2 * TBigDecimalUtils.Exp(AValue),
      TBigDecimalUtils.Exp(2 * AValue) - 1
    );
end;

class function TBigDecimalHyperbolic.Sech(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Divide(
      2 * TBigDecimalUtils.Exp(AValue),
      TBigDecimalUtils.Exp(2 * AValue) + 1
    );
end;

class function TBigDecimalHyperbolic.Sinh(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Divide(
      1 - TBigDecimalUtils.Exp(-2 * AValue),
      2 * TBigDecimalUtils.Exp(-AValue)
    );
end;

class function TBigDecimalHyperbolic.Tanh(const AValue: BigDecimal): BigDecimal;
begin
  Result :=
    TBigDecimalUtils.Divide(
      TBigDecimalUtils.Exp(2 * AValue) - 1,
      TBigDecimalUtils.Exp(2 * AValue) + 1
    );
end;

end.
