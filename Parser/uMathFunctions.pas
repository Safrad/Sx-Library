unit uMathFunctions;

interface

uses
	uVector;

const
	ConstC = 299792458; // Speed of the light [m / s]

implementation

uses
  uBigDecimalConsts,
  Velthuis.BigDecimals,
  uNamespace, uStrings;

function IfElse(const X, Y, Z: TVector): TVector;
begin
	if VectorToNum(X) <> 0 then
		Result := Y
	else
		Result := Z;
end;

function Equal(const X, Y: TVector): TVector;
begin
	if BigDecimal.Compare(VectorToNum(X), VectorToNum(Y)) = 0 then
		Result := NumToVector(1)
	else
		Result := NumToVector(0);
end;

function Greater(const X, Y: TVector): TVector;
begin
	if VectorToNum(X) > VectorToNum(Y) then
		Result := NumToVector(1)
	else
		Result := NumToVector(0);
end;

function Less(const X, Y: TVector): TVector;
begin
	if VectorToNum(X) < VectorToNum(Y) then
		Result := NumToVector(1)
	else
		Result := NumToVector(0);
end;

function GreaterOrEqual(const X, Y: TVector): TVector;
begin
	if VectorToNum(X) >= VectorToNum(Y) then
		Result := NumToVector(1)
	else
		Result := NumToVector(0);
end;

function LessOrEqual(const X, Y: TVector): TVector;
begin
	if VectorToNum(X) <= VectorToNum(Y) then
		Result := NumToVector(1)
	else
		Result := NumToVector(0);
end;

function E: TVector;
begin
  Result := NumToVector(TBigDecimalConsts.NumberE);
end;

function C: TVector;
begin
	Result := NumToVector(ConstC);
end;

function Neg(const X: TVector): TVector;
begin
	Result := NegVector(X);
end;

function Plus(const X, Y: TVector): TVector;
begin
	Result := PlusVector(X, Y);
end;

function Minus(const X, Y: TVector): TVector;
begin
	Result := PlusVector(X, NegVector(Y));
end;

function Multiply(const X, Y: TVector): TVector;
begin
	Result := MultiplyVector(X, Y);
end;

function Divide(const X, Y: TVector): TVector;
begin
	Result := DivideVector(X, Y);
end;

function Modulo(const X, Y: TVector): TVector;
begin
	Result := ModuloVector(X, Y);
end;

function Power(const X, Y: TVector): TVector;
begin
	Result := PowerVector(X, Y);
end;

function Log(const X, Y: TVector): TVector;
begin
	Result := LogVector(X, Y);
end;

function Ln(const X: TVector): TVector;
begin
	Result := LnVector(X);
end;

function Sqr(const X: TVector): TVector;
begin
	Result := SqrVector(X);
end;

function Sqrt(const X: TVector): TVector;
begin
	Result := SqrtVector(X);
end;

function Floor(const X: TVector): TVector;
begin
	Result := FloorVector(X);
end;

function Ceil(const X: TVector): TVector;
begin
	Result := CeilVector(X);
end;

function Trunc(const X: TVector): TVector;
begin
	Result := TruncVector(X);
end;

function RoundTowardsZero(const X: TVector): TVector;
begin
	Result := RoundTowardsZeroVector(X);
end;

function RoundAwayFromZero(const X: TVector): TVector;
begin
	Result := RoundAwayFromZeroVector(X);
end;

function RoundHalfCeil(const X: TVector): TVector;
begin
	Result := RoundHalfCeilVector(X);
end;

function RoundHalfFloor(const X: TVector): TVector;
begin
	Result := RoundHalfFloorVector(X);
end;

function RoundHalfTowardsZero(const X: TVector): TVector;
begin
	Result := RoundHalfTowardsZeroVector(X);
end;

function RoundHalfAwayFromZero(const X: TVector): TVector;
begin
	Result := RoundHalfAwayFromZeroVector(X);
end;

function RoundHalfEven(const X: TVector): TVector;
begin
	Result := RoundHalfEvenVector(X);
end;

function Round(const X: TVector): TVector;
begin
	Result := RoundVector(X);
end;

function Frac(const X: TVector): TVector;
begin
	Result := FracVector(X);
end;

function Abs(const X: TVector): TVector;
begin
	Result := AbsVector(X);
end;

function _Not(const X: TVector): TVector;
begin
	Result := NotVector(X);
end;

function Inv(const X: TVector): TVector;
begin
	Result := InvVector(X);
end;

function Inc(const X: TVector): TVector;
begin
	Result := PlusVector(X, NumToVector(1));
end;

function Dec(const X: TVector): TVector;
begin
	Result := PlusVector(X, NumToVector(-1));
end;

function Exp(const X: TVector): TVector;
begin
	Result := ExpVector(X);
end;

function Fact(const X: TVector): TVector;
begin
	Result := FactVector(X);
end;

function Gamma(const X: TVector): TVector;
begin
	Result := GammaVector(X);
end;

function GCD(const AData: array of TVector): TVector;
begin
  Result := GCDVector(AData);
end;

function LCM(const AData: array of TVector): TVector;
begin
  Result := LCMVector(AData);
end;

initialization
{$IFNDEF NoInitialization}
	AddFunction('Conditional Tests', 'IfElse', IfElse, WikipediaURLPrefix + 'Conditional_(programming)');
	AddFunction('Arithmetic', 'Equal', Equal, WikipediaURLPrefix + 'Conditional_(programming)');
	AddFunction('Arithmetic', 'Greater', Greater, WikipediaURLPrefix + 'Inequality');
	AddFunction('Arithmetic', 'Less', Less, WikipediaURLPrefix + 'Inequality');
	AddFunction('Arithmetic', 'GreaterOrEqual', GreaterOrEqual, WikipediaURLPrefix + 'Inequality');
	AddFunction('Arithmetic', 'LessOrEqual', LessOrEqual, WikipediaURLPrefix + 'Inequality');
	AddFunction('Arithmetic', 'E', E, WikipediaURLPrefix + 'Eulers_number');
	AddFunction('Physics', 'C', C, WikipediaURLPrefix + 'Speed_of_light');
	AddFunction('Arithmetic', 'neg', Neg, WikipediaURLPrefix + 'Additive_inverse');
	AddFunction('Arithmetic', 'inv', Inv, WikipediaURLPrefix + 'Multiplicative_inverse');
	AddFunction('Arithmetic', 'plus', Plus, WikipediaURLPrefix + 'Addition');
	AddFunction('Arithmetic', 'minus', Minus, WikipediaURLPrefix + 'Subtraction');
	AddFunction('Arithmetic', 'mul', Multiply, WikipediaURLPrefix + 'Multiply');
	AddFunction('Arithmetic', 'div', Divide, '(Dividend;Divisor) ' + WikipediaURLPrefix + 'Division_(mathematics)');
	AddFunction('Arithmetic', 'mod', Modulo, '(Dividend;Divisor) ' + WikipediaURLPrefix + 'Modular_arithmetic');
	AddFunction('Arithmetic', 'power', Power, '(Base;Exponent) ' + WikipediaURLPrefix + 'Exponentiation');
	AddFunction('Arithmetic', 'log', Log, '(Base;Value) ' + WikipediaURLPrefix + 'Logarithm');
	AddFunction('Arithmetic', 'ln', Ln, WikipediaURLPrefix + 'Natural_logarithm');
	AddFunction('Arithmetic', 'sqr', Sqr, WikipediaURLPrefix + 'Squared');
	AddFunction('Arithmetic', 'sqrt', Sqrt, WikipediaURLPrefix + 'Square_root');

	AddFunction('Arithmetic', 'floor', Floor, WikipediaURLPrefix + 'Rounding#Rounding_down'); // Floor_function
	AddFunction('Arithmetic', 'ceil', Ceil, WikipediaURLPrefix + 'Rounding#Rounding_up'); // Ceil_function
	AddFunction('Arithmetic', 'trunc', Trunc, WikipediaURLPrefix + '/Rounding#Rounding_towards_zero'); // Truncation
	AddFunction('Arithmetic', 'RoundTowardsZero', RoundTowardsZero, WikipediaURLPrefix + 'Rounding#Rounding_towards_zero');
	AddFunction('Arithmetic', 'RoundAwayFromZero', RoundAwayFromZero, WikipediaURLPrefix + 'Rounding#Rounding_away_from_zero');

	AddFunction('Arithmetic', 'RoundHalfCeil', RoundHalfCeil, WikipediaURLPrefix + 'Rounding#Round_half_down');
	AddFunction('Arithmetic', 'RoundHalfFloor', RoundHalfFloor, WikipediaURLPrefix + 'Rounding#Round_half_up');
	AddFunction('Arithmetic', 'RoundHalfTowardsZero', RoundHalfTowardsZero, WikipediaURLPrefix + 'Rounding#Round_half_towards_zero');
	AddFunction('Arithmetic', 'RoundHalfAwayFromZero', RoundHalfAwayFromZero, WikipediaURLPrefix + 'Rounding#Round_half_away_from_zero');
	AddFunction('Arithmetic', 'RoundHalfToEven', RoundHalfEven, WikipediaURLPrefix + 'Rounding#Round_half_to_even');
	AddFunction('Arithmetic', 'round', Round, WikipediaURLPrefix + 'Rounding#Round_half_to_even');

	AddFunction('Arithmetic', 'frac', Frac, WikipediaURLPrefix + 'Fractional_part');
	AddFunction('Arithmetic', 'abs', Abs, WikipediaURLPrefix + 'Absolute_value');
	AddFunction('Arithmetic', 'not', _Not, WikipediaURLPrefix + 'Logical_negation');
	AddFunction('Arithmetic', 'inc', Inc, WikipediaURLPrefix + 'Increment');
	AddFunction('Arithmetic', 'dec', Dec, WikipediaURLPrefix + 'Decrement');
	AddFunction('Arithmetic', 'exp', Exp, WikipediaURLPrefix + 'Exponential_function');
	AddFunction('Arithmetic', 'fact', Fact, WikipediaURLPrefix + 'Factorial');
	AddFunction('Arithmetic', 'Gamma', Gamma, WikipediaURLPrefix + 'Gamma_function');
	AddFunction('Arithmetic', 'Γ', Gamma, WikipediaURLPrefix + 'Gamma_function');
	AddFunction('Arithmetic', 'GCD', GCD, WikipediaURLPrefix + 'Greatest_common_divisor');
	AddFunction('Arithmetic', 'LCM', LCM, WikipediaURLPrefix + 'Least_common_multiple');
{$ENDIF NoInitialization}
end.

