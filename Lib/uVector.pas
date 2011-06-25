//* File:     Lib\uVector.pas
//* Created:  2006-05-03
//* Modified: 2007-05-27
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uVector;

interface

uses
	uTypes,
	uOutputFormat;

type
(*
	TOperator = (opNone, opNumber, opIdent,
//		opUnarMinus - implemented as opMinus with firts argument nil
		// Arithmetic
		opPlus, opMinus, opMul, opDiv, opMod,
		opFact, opPower,
		// Logic
		opNot, opShl, opShr, opAnd, opOr, opXor, opXnor,
		// Single
		opRound, opTrunc, opAbs, opNeg, opInv, opInc, opDec, opGCD, opLCM,
		// Exponencial
		opExp, opLn, opLog, opSqr, opSqrt,
		// Goniometric
		opLength,
		opSin, opCos, opTan,
		opArcSin, opArcCos, opArcTan,
		opSinh, opCosh, opTanh,
		opArcSinh, opArcCosh, opArcTanh
		{
		b	a	| 0 and or xor xnor 1
		0	0	  0  0  0   0   1   1
		0	1   0  0  1   1   0   1
		1	0   0  0  1   1   0   1
		1	1   0  1  1   0   1   1
		}
); *)

	TVector = array of FA;

function NullVector: TVector;
function NegVector(const V: TVector): TVector;
function PlusVector(const V1, V2: TVector): TVector;
function MinusVector(const V1, V2: TVector): TVector;
function SumVector(const V: TVector): FA;
function MultiplyVector(const V1, V2: TVector): TVector;
function DivideVector(const V1, V2: TVector): TVector;
function ModuloVector(const V1, V2: TVector): TVector;
function SqrVector(const V: TVector): TVector;
function SqrtVector(const V: TVector): TVector;
function PowerVector(const V1, V2: TVector): TVector;
function LogVector(const V1, V2: TVector): TVector;
function LnVector(const V: TVector): TVector;
function CompareVector(const V1, V2: TVector): FA;

function NumToVector(const Num: FA): TVector;
function VectorToNum(const V: TVector): FA;
function VectorToStr(const V: TVector; const OutputFormat: TOutputFormat): string;

implementation

uses
	Math,
	uMath;

function NullVector: TVector;
begin
	SetLength(Result, 0);
end;

function NegVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := -V[i];
end;

function PlusVector(const V1, V2: TVector): TVector;
var
	L1, L2: SG;
	i: SG;
begin
	L1 := Length(V1);
	L2 := Length(V2);
	SetLength(Result, Max(L1, L2));
	for i := 0 to Length(Result) - 1 do
	begin
		Result[i] := 0;
		if i < L1 then
			Result[i] := Result[i] + V1[i];
		if i < L2 then
			Result[i] := Result[i] + V2[i];
	end;
end;

function MinusVector(const V1, V2: TVector): TVector;
begin
	Result := PlusVector(V1, NegVector(V2));
end;

function SumVector(const V: TVector): FA;
var i: SG;
begin
	Result := 0;
	for i := 0 to Length(V) - 1 do
		Result := Result + V[i];
end;

function MultiplyVector(const V1, V2: TVector): TVector;
var
	i: SG;
	M: FA;
begin
	M := SumVector(V2);
	SetLength(Result, Length(V1));
	for i := 0 to Length(Result) - 1 do
	begin
		Result[i] := V1[i] * M;
	end;
end;

function DivideVector(const V1, V2: TVector): TVector;
var
	i: SG;
	M: FA;
begin
	M := SumVector(V2);
	SetLength(Result, Length(V1));
	for i := 0 to Length(Result) - 1 do
	begin
		if M = 0 then
		begin
			if Result[i] > 0 then
				Result[i] := Infinity
			else if Result[i] < 0 then
				Result[i] := NegInfinity
			else
				Result[i] := 0;
		end
		else
		begin
			Result[i] := V1[i] / M;
		end;
	end;
end;

function ModuloVector(const V1, V2: TVector): TVector;
var
	i: SG;
	M: FA;
begin
	M := SumVector(V2);
	SetLength(Result, Length(V1));
	for i := 0 to Length(Result) - 1 do
	begin
		if M = 0 then
		begin
			if Result[i] > 0 then
				Result[i] := Infinity
			else if Result[i] < 0 then
				Result[i] := NegInfinity
			else
				Result[i] := 0;
		end
		else
		begin
			Result[i] := ModE(V1[i], M);
		end;
	end;
end;

function SqrVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := Sqr(V[i]);
end;

function SqrtVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := Sqrt(V[i]);
end;

function VectorLength(const V: TVector): FA;
var i: SG;
begin
	Result := 0;
	for i := 0 to Length(V) - 1 do
		Result := Result + {Sqr-remove sign!}(V[i]);
end;

function PowerVector(const V1, V2: TVector): TVector;
begin
	Result := NumToVector(Math.Power(VectorToNum(V1), VectorToNum(V2)));
end;

function LogVector(const V1, V2: TVector): TVector;
begin
	Result := NumToVector(Math.LogN(VectorToNum(V1), VectorToNum(V2)));
end;

function LnVector(const V: TVector): TVector;
begin
	Result := NumToVector(Ln(VectorToNum(V)));
end;

function CompareVector(const V1, V2: TVector): FA;
begin
	Result := VectorLength(V1) - VectorLength(V2);
end;

(*
function UnaryOperation(const V: TVector; const Operation: TOperator; const GonFormat: TGoniometricFormat = gfRad): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
	begin
			case GonFormat of
			gfGrad: V[i] := GradToRad(V[i]);
			gfDeg: V[i] := DegToRad(V[i]);
			gfCycle: V[i] := CycleToRad(V[i]);
			end;
		case Operation of
		opNeg: Result[i] := -V[i];
		opTrunc: Result[i] := Trunc(V[i]);
		opRound: Result[i] := Round(V[i]);
		opAbs: Result[i] := Abs(V[i]);
		opNot: Result[i] := not Round(V[i]);
		opInc: Result[i] := V[i] + 1;
		opDec: Result[i] := V[i] - 1;
		opExp: Result[i] := Exp(V[i]);
		opLn:
		begin
			if V[i] > 0 then
				Result[i] := Ln(V[i])
			else
				Result[i] := NegInfinity;
//					ShowError('Input 0..infinity for Ln');}
		end;
		opSqr: Result[i] := Sqr(V[i]);
		opSqrt: Result[i] := Sqrt(V[i]);
		// Goniometric
		opSin: Result[i] := Sin(V[i]);
		opCos: Result[i] := Cos(V[i]);
		opTan: Result[i] := Tan(V[i]);
		opArcSin: Result[i] := ArcSin(V[i]);
		opArcCos: Result[i] := ArcCos(V[i]);
		opArcTan: Result[i] := ArcTan(V[i]);

		opSinH: Result[i] := Sinh(V[i]);
		opCosH: Result[i] := Cosh(V[i]);
		opTanH: Result[i] := Tanh(V[i]);
		opArcSinH: Result[i] := ArcSinh(V[i]);
		opArcCosH: Result[i] := ArcCosh(V[i]);
		opArcTanH: Result[i] := ArcTanh(V[i]);

		else Assert(False);
		end;
	end;
end; *)

function NumToVector(const Num: FA): TVector;
begin
	SetLength(Result, 1);
	Result[0] := Num;
end;

function VectorToNum(const V: TVector): FA;
begin
//	Assert(Length(Vector) = 1);
	if Length(V) = 0 then
		Result := 0
	else
		Result := V[0];
end;

function VectorToStr(const V: TVector; const OutputFormat: TOutputFormat): string;
var i: SG;
begin
	case Length(V) of
	0: Result := '';
	1: Result := FToS(V[0], OutputFormat);
	else
	begin
		Result := '(';
		for i := 0 to Length(V) - 1 do
			Result := Result + FToS(V[i], OutputFormat);

		Result := Result + ')';
	end;
	end;
end;

end.

