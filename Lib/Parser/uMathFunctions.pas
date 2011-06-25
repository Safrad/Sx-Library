//* File:     Lib\Parser\uMathFunctions.pas
//* Created:  2004-03-07
//* Modified: 2007-05-27
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

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

(*	opPlus, opMinus:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				V := Calc(Node.Args[i]);
				if Node.Operation = opMinus then
					V := NegVector(Calc(Node.Args[i]));
				Result := PlusVector(Result, V);
			end;
		end;
	end;
	opMul:
	begin
		Result := NumToVector(1);
		for i := 0 to Node.ArgCount - 1 do
		begin
			if Node.Args[i] <> nil then
				Result := MultiplyVector(Result, Calc(Node.Args[i]));
		end;
	end;
	opDiv:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				if Node.Args[i] <> nil then
				begin
					Result := DivideVector(Result, Calc(Node.Args[i]));
{					e := Calc(Node.Args[i]);
					if (e = 0) then
					begin
						if Result > 0 then
							Result := Infinity
						else if Result < 0 then
							Result := NegInfinity;
					end
					else
						Result := Result / e;}
				end;
			end;
		end;
	end;
	opMod:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := Calc(Node.Args[0]);
			for i := 1 to Node.ArgCount - 1 do
			begin
				if Node.Args[i] <> nil then
				begin
					Result := ModuloVector(Result, Calc(Node.Args[i]));
{				e := Calc(Node.Args[i]);
				if e = 0 then
				else
					Result := ModE(Result, e);}
				end;
			end;
		end;
	end;
	// Unary
	opNeg, opTrunc, opRound, opAbs, opNot, opInc, opDec, opExp, opLn, opSqr, opSqrt:
	begin
		if Node.ArgCount > 0 then
		begin
			Result := UnaryOperation(Calc(Node.Args[0]), Node.Operation);
			for i := 1 to Node.ArgCount - 1 do
			begin
				if Node.Args[i] <> nil then
				begin
					Result := PlusVector(Result, UnaryOperation(Calc(Node.Args[i]), Node.Operation));
				end;
			end;
		end;
	end;

(*	opInv:
	begin
		Result := 1;
		for i := 0 to Node.ArgCount - 1 do
		begin
			Result := Result / Calc(Node.Args[i]);
		end;
	end;
	opFact:
	begin
		Result := 1;
		for i := 0 to Node.ArgCount - 1 do
		begin
			e := Round(Calc(Node.Args[i])); // TODO : Factor 1.5 =
			if e < 0 then
			begin
//				ShowError('Input -infinity..2000 for Fact')
			end
			else if e <= 1754 then
			begin
				for j := 2 to Round(e) do
					Result := Result * j;
			end
			else
			begin
				if e > 1754 then Result := Infinity;
			end;
		end;
	end;
	opGCD: // Greatest Common Measure (Divident)
	begin
		if Node.ArgCount = 1 then
			Result := Round(Calc(Node.Args[0]))
		else if Node.ArgCount >= 2 then
		begin
			Result := Round(Calc(Node.Args[0]));

			for i := 1 to Node.ArgCount - 1 do
			begin
				e := Round(Calc(Node.Args[i]));

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
			Result := Round(Calc(Node.Args[0]))
		else if Node.ArgCount >= 2 then
		begin
			e0 := Round(Calc(Node.Args[0]));

			Result := e0;

			for i := 1 to Node.ArgCount - 1 do
			begin
				e1 := Round(Calc(Node.Args[i]));
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
				Assert(Frac(Result / Round(Calc(Node.Args[i]))) = 0);
			{$endif}
		end
		else
			Result := 0;
	end; *)

initialization
	AddFunction('Math', 'X', X, 'Variable X.');
	AddFunction('Math', 'E', E, 'Euler''s number.');
	AddFunction('Math', 'C', C, 'Speed of light.');
	AddFunction('Math', 'neg', Neg, '');
	AddFunction('Math', 'plus', Plus, '');
	AddFunction('Math', 'minus', Minus, '');
	AddFunction('Math', 'mul', Multiply, '');
	AddFunction('Math', 'div', Divide, '');
	AddFunction('Math', 'mod', Modulo, '');
	AddFunction('Math', 'power', Power, '');
	AddFunction('Math', 'log', Log, '');
	AddFunction('Math', 'ln', Ln, '');
end.

