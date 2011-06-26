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
function TruncVector(const V: TVector): TVector;
function FloorVector(const V: TVector): TVector;
function RoundVector(const V: TVector): TVector;
function CeilVector(const V: TVector): TVector;
function FracVector(const V: TVector): TVector;
function AbsVector(const V: TVector): TVector;
function NotVector(const V: TVector): TVector;
function InvVector(const V: TVector): TVector;
function ExpVector(const V: TVector): TVector;
function FactVector(const V: TVector): TVector;
function GammaVector(const V: TVector): TVector;
function CompareVector(const V1, V2: TVector): FA;
function ShlVector(const V1, V2: TVector): TVector;
function ShrVector(const V1, V2: TVector): TVector;
function AndVector(const V1, V2: TVector): TVector;
function OrVector(const V1, V2: TVector): TVector;
function XorVector(const V1, V2: TVector): TVector;
function XnorVector(const V1, V2: TVector): TVector;

function NumToVector(const Num: FA): TVector;
function VectorToNum(const V: TVector): FA;
function VectorToStr(const V: TVector; const OutputFormat: TOutputFormat): string;

implementation

uses
	Math, GammaF,
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

function TruncVector(const V: TVector): TVector;
begin
	Result := NumToVector(Trunc(VectorToNum(V)));
end;

function FloorVector(const V: TVector): TVector;
begin
	Result := NumToVector(Floor(VectorToNum(V)));
end;

function RoundVector(const V: TVector): TVector;
begin
	Result := NumToVector(Round(VectorToNum(V)));
end;

function CeilVector(const V: TVector): TVector;
begin
	Result := NumToVector(Ceil(VectorToNum(V)));
end;

function FracVector(const V: TVector): TVector;
begin
	Result := NumToVector(Frac(VectorToNum(V)));
end;

function AbsVector(const V: TVector): TVector;
begin
	Result := NumToVector(Abs(VectorToNum(V)));
end;

function NotVector(const V: TVector): TVector;
begin
	Result := NumToVector(not Round(VectorToNum(V)));
end;

function InvVector(const V: TVector): TVector;
begin
	Result := DivideVector(NumToVector(1), V);
end;

function ExpVector(const V: TVector): TVector;
begin
	Result := NumToVector(Exp(VectorToNum(V)));
end;

function FactVector(const V: TVector): TVector;
var
	i, j: SG;
	e, x: FA;
begin
	x := 1;
	for i := 0 to 0 do
	begin
		e := VectorToNum(V);
		if Frac(e) = 0 then
		begin
			if e < 0 then
			begin
			//				ShowError('Input -infinity..2000 for Fact')
			end
			else if e <= 1754 then
			begin
				for j := 2 to Round(e) do
					x := x * j;
			end
			else
			begin
				if e > 1754 then x := Infinity;
			end;
		end
		else
		begin
			x := Gamma(e + 1);
		end;
	end;
	Result := NumToVector(x);
end;

function GammaVector(const V: TVector): TVector;
var
	e, x: FA;
begin
	e := VectorToNum(V);
	x := Gamma(e);
	Result := NumToVector(x);
end;

function CompareVector(const V1, V2: TVector): FA;
begin
	Result := VectorLength(V1) - VectorLength(V2);
end;

function ShlVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := Round(V1[i]) shl Round(VectorToNum(V2));
end;

function ShrVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := Round(V1[i]) shr Round(VectorToNum(V2));
end;

function AndVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := Round(V1[i]) and Round(VectorToNum(V2));
end;

function OrVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := Round(V1[i]) or Round(VectorToNum(V2));
end;

function XorVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := Round(V1[i]) xor Round(VectorToNum(V2));
end;

function XnorVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := not (Round(V1[i]) xor Round(VectorToNum(V2)));
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

