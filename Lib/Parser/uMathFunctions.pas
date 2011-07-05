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

uses uNamespace;

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
	AddFunction('Variable', 'x', X, 'Variable X.');
	AddFunction('Conditional Tests', 'IfElse', IfElse, 'http://en.wikipedia.org/wiki/Conditional_(programming)');
	AddFunction('Arithmetic', 'Equal', Equal, 'http://en.wikipedia.org/wiki/Conditional_(programming)');
	AddFunction('Arithmetic', 'Greater', Greater, 'http://en.wikipedia.org/wiki/Inequality');
	AddFunction('Arithmetic', 'Less', Less, 'http://en.wikipedia.org/wiki/Inequality');
	AddFunction('Arithmetic', 'GreaterOrEqual', GreaterOrEqual, 'http://en.wikipedia.org/wiki/Inequality');
	AddFunction('Arithmetic', 'LessOrEqual', LessOrEqual, 'http://en.wikipedia.org/wiki/Inequality');
	AddFunction('Arithmetic', 'E', E, 'http://en.wikipedia.org/wiki/Eulers_number');
	AddFunction('Physics', 'C', C, 'http://en.wikipedia.org/wiki/Speed_of_light');
	AddFunction('Arithmetic', 'neg', Neg, 'http://en.wikipedia.org/wiki/Additive_inverse');
	AddFunction('Arithmetic', 'inv', Inv, 'http://en.wikipedia.org/wiki/Multiplicative_inverse');
	AddFunction('Arithmetic', 'plus', Plus, 'http://en.wikipedia.org/wiki/Addition');
	AddFunction('Arithmetic', 'minus', Minus, 'http://en.wikipedia.org/wiki/Subtraction');
	AddFunction('Arithmetic', 'mul', Multiply, 'http://en.wikipedia.org/wiki/Multiply');
	AddFunction('Arithmetic', 'div', Divide, '(Dividend;Divisor) http://en.wikipedia.org/wiki/Division_(mathematics)');
	AddFunction('Arithmetic', 'mod', Modulo, '(Dividend;Divisor) http://en.wikipedia.org/wiki/Modular_arithmetic');           
	AddFunction('Arithmetic', 'power', Power, '(Base;Exponent) http://en.wikipedia.org/wiki/Exponentiation');
	AddFunction('Arithmetic', 'log', Log, '(Base;Exponent) http://en.wikipedia.org/wiki/Logarithm');
	AddFunction('Arithmetic', 'ln', Ln, 'http://en.wikipedia.org/wiki/Natural_logarithm');
	AddFunction('Arithmetic', 'sqr', Sqr, 'http://en.wikipedia.org/wiki/Squared');
	AddFunction('Arithmetic', 'sqrt', Sqrt, 'http://en.wikipedia.org/wiki/Square_root');
	AddFunction('Arithmetic', 'trunc', Trunc, 'http://en.wikipedia.org/wiki/Truncation');
	AddFunction('Arithmetic', 'floor', Floor, 'http://en.wikipedia.org/wiki/Floor_function');
	AddFunction('Arithmetic', 'round', Round, 'http://en.wikipedia.org/wiki/Round_number');
	AddFunction('Arithmetic', 'ceil', Ceil, 'http://en.wikipedia.org/wiki/Ceil_function');
	AddFunction('Arithmetic', 'frac', Frac, 'http://en.wikipedia.org/wiki/Fractional_part');
	AddFunction('Arithmetic', 'abs', Abs, 'http://en.wikipedia.org/wiki/Absolute_value');
	AddFunction('Arithmetic', 'not', _Not, 'http://en.wikipedia.org/wiki/Logical_negation');
	AddFunction('Arithmetic', 'inc', Inc, 'http://en.wikipedia.org/wiki/Increment');
	AddFunction('Arithmetic', 'dec', Dec, 'http://en.wikipedia.org/wiki/Decrement');
	AddFunction('Arithmetic', 'exp', Exp, 'http://en.wikipedia.org/wiki/Exponential_function');
	AddFunction('Arithmetic', 'fact', Fact, 'http://en.wikipedia.org/wiki/Factorial');
	AddFunction('Arithmetic', 'gamma', Gamma, 'http://en.wikipedia.org/wiki/Gamma_function');
end.

