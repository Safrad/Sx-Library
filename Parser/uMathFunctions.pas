unit uMathFunctions;

interface

uses
	uVector;

const
	ConstE = 2.7182818284590452353602874713527; // Euler's number
	ConstC = 299792458; // Speed of the light [m / s]
var
	VariableX: TVector;

implementation

uses
  uNamespace, uStrings;

function X: TVector;
begin
	Result := VariableX;
end;

function IfElse(const X, Y, Z: TVector): TVector;
begin
	if VectorToNum(X) <> 0 then
		Result := Y
	else
		Result := Z;
end;

function Equal(const X, Y: TVector): TVector;
begin
	if VectorToNum(X)= VectorToNum(Y) then // TODO: Epsilon
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
	Result := NumToVector(ConstE);
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

function Trunc(const X: TVector): TVector;
begin
	Result := TruncVector(X);
end;

function Floor(const X: TVector): TVector;
begin
	Result := FloorVector(X);
end;

function Round(const X: TVector): TVector;
begin
	Result := RoundVector(X);
end;

function Ceil(const X: TVector): TVector;
begin
	Result := CeilVector(X);
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

(* TODO :
	opGCD: // Greatest Common Measure (Divident)
	begin
		if Node.ArgCount = 1 then
			Result := RoundN(Calc(Node.Args[0]))
		else if Node.ArgCount >= 2 then
		begin
			Result := RoundN(Calc(Node.Args[0]));

			for i := 1 to Node.ArgCount - 1 do
			begin
				e := RoundN(Calc(Node.Args[i]));

				while Result <> e do
				begin
					if Result > e then
						Result := Result - e
					else
						e := e - Result;
				end;
			end;

		end
		else
			Result := 0;
	end;
	opLCM: // Less Common Multipicator
	begin
		if Node.ArgCount = 1 then
			Result := RoundN(Calc(Node.Args[0]))
		else if Node.ArgCount >= 2 then
		begin
			e0 := RoundN(Calc(Node.Args[0]));

			Result := e0;

			for i := 1 to Node.ArgCount - 1 do
			begin
				e1 := RoundN(Calc(Node.Args[i]));
				e := e1;
				while Result <> e do
				begin
					if Result > e then
						Result := Result - e
					else
						e := e - Result;
				end;
				if Result <> 0 then
					Result := e0 * e1 / Result
				else
					Result := Infinity;
				e0 := Result;
			end;
			{$ifopt c+}
			for i := 0 to Node.ArgCount - 1 do
				Assert(Frac(Result / RoundN(Calc(Node.Args[i]))) = 0);
			{$endif}
		end
		else
			Result := 0;
	end; *)

initialization
{$IFNDEF NoInitialization}
	AddFunction('Variable', 'x', X, 'Variable X.');
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
	AddFunction('Arithmetic', 'log', Log, '(Base;Exponent) ' + WikipediaURLPrefix + 'Logarithm');
	AddFunction('Arithmetic', 'ln', Ln, WikipediaURLPrefix + 'Natural_logarithm');
	AddFunction('Arithmetic', 'sqr', Sqr, WikipediaURLPrefix + 'Squared');
	AddFunction('Arithmetic', 'sqrt', Sqrt, WikipediaURLPrefix + 'Square_root');
	AddFunction('Arithmetic', 'trunc', Trunc, WikipediaURLPrefix + 'Truncation');
	AddFunction('Arithmetic', 'floor', Floor, WikipediaURLPrefix + 'Floor_function');
	AddFunction('Arithmetic', 'round', Round, WikipediaURLPrefix + 'Round_number');
	AddFunction('Arithmetic', 'ceil', Ceil, WikipediaURLPrefix + 'Ceil_function');
	AddFunction('Arithmetic', 'frac', Frac, WikipediaURLPrefix + 'Fractional_part');
	AddFunction('Arithmetic', 'abs', Abs, WikipediaURLPrefix + 'Absolute_value');
	AddFunction('Arithmetic', 'not', _Not, WikipediaURLPrefix + 'Logical_negation');
	AddFunction('Arithmetic', 'inc', Inc, WikipediaURLPrefix + 'Increment');
	AddFunction('Arithmetic', 'dec', Dec, WikipediaURLPrefix + 'Decrement');
	AddFunction('Arithmetic', 'exp', Exp, WikipediaURLPrefix + 'Exponential_function');
	AddFunction('Arithmetic', 'fact', Fact, WikipediaURLPrefix + 'Factorial');
	AddFunction('Arithmetic', 'gamma', Gamma, WikipediaURLPrefix + 'Gamma_function');
{$ENDIF NoInitialization}
end.

