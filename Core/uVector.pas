unit uVector;

interface

uses
  Velthuis.BigDecimals,
	uTypes,
	uOutputFormat;

type
	TVector = array of BigDecimal;

function NullVector: TVector;
function NegVector(const V: TVector): TVector;
function PlusVector(const V1, V2: TVector): TVector;
function MinusVector(const V1, V2: TVector): TVector;
function SumVector(const V: TVector): BigDecimal;
function MultiplyVector(const V1, V2: TVector): TVector;
function DivideVector(const V1, V2: TVector): TVector;
function ModuloVector(const V1, V2: TVector): TVector;
function SqrVector(const V: TVector): TVector;
function SqrtVector(const V: TVector): TVector;
function PowerVector(const V1, V2: TVector): TVector;
function LogVector(const V1, V2: TVector): TVector;
function LnVector(const V: TVector): TVector;

function CeilVector(const V: TVector): TVector;
function FloorVector(const V: TVector): TVector;

function TruncVector(const V: TVector): TVector; // Same as RoundTowardZeroVector
function RoundTowardsZeroVector(const V: TVector): TVector; // Same as TruncVector
function RoundAwayFromZeroVector(const V: TVector): TVector;

function RoundHalfCeilVector(const V: TVector): TVector;
function RoundHalfFloorVector(const V: TVector): TVector;
function RoundHalfTowardsZeroVector(const V: TVector): TVector;
function RoundHalfAwayFromZeroVector(const V: TVector): TVector;
function RoundHalfEvenVector(const V: TVector): TVector; // Same as RoundVector
function RoundVector(const V: TVector): TVector; // Same as RoundHalfEvenVector

function FracVector(const V: TVector): TVector; // Original = Ceil + Frac
function AbsVector(const V: TVector): TVector;
function NotVector(const V: TVector): TVector;
function InvVector(const V: TVector): TVector;
function ExpVector(const V: TVector): TVector;
function FactVector(const V: TVector): TVector;
function GammaVector(const V: TVector): TVector;
function GCDVector(const AData: array of TVector): TVector;
function LCMVector(const AData: array of TVector): TVector;

function CompareVector(const V1, V2: TVector): BigDecimal;
function ShlVector(const V1, V2: TVector): TVector;
function ShrVector(const V1, V2: TVector): TVector;

(*
  b a | 0  and or  xor xnor 1
  –––––––––––––––––––––––––––
  0 0 | 0   0   0   0   1   1
  0 1 | 0   0   1   1   0   1
  1 0 | 0   0   1   1   0   1
  1 1 | 0   1   1   0   1   1
*)
function AndVector(const V1, V2: TVector): TVector;
function OrVector(const V1, V2: TVector): TVector;
function XorVector(const V1, V2: TVector): TVector;
function XnorVector(const V1, V2: TVector): TVector;

function BigDecimalToString(const ABigDeciomal: BigDecimal): string;
function NumToVector(const Num: BigDecimal): TVector;
function VectorToNum(const V: TVector): BigDecimal;
function VectorToStr(const V: TVector; const OutputFormat: TOutputFormat): string;

implementation

uses
	SysUtils,
  Math,
  GammaF,

  Velthuis.BigIntegers,
  uChar,
  uBigDecimalHelper,
  uStrings,
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

function SumVector(const V: TVector): BigDecimal;
var i: SG;
begin
	Result := 0;
	for i := 0 to Length(V) - 1 do
		Result := Result + V[i];
end;

function MultiplyVector(const V1, V2: TVector): TVector;
var
	i: SG;
	M: BigDecimal;
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
	M: BigDecimal;
begin
	M := SumVector(V2);
	SetLength(Result, Length(V1));
	for i := 0 to Length(Result) - 1 do
	begin
{		if M = 0 then
		begin
			if Result[i] > 0 then
				Result[i] := Infinity
			else if Result[i] < 0 then
				Result[i] := NegInfinity
			else
				Result[i] := 0;
		end
		else
		begin}
			Result[i] := V1[i].Divide(M);
//		end;
	end;
end;

function ModuloVector(const V1, V2: TVector): TVector;
var
	i: SG;
	M: BigDecimal;
begin
	M := SumVector(V2);
	SetLength(Result, Length(V1));
	for i := 0 to Length(Result) - 1 do
	begin
{		if M = 0 then
		begin
			if Result[i] > 0 then
				Result[i] := Infinity
			else if Result[i] < 0 then
				Result[i] := NegInfinity
			else
				Result[i] := 0;
		end
		else
		begin}
    	Result[i] := V1[i] - BigDecimal.Round(V1[i].Divide(M), rmFloor) * M;
//		end;
	end;
end;

function SqrVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Sqr(V[i]);
end;

function SqrtVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Sqrt(V[i], Max(BigDecimal.DefaultPrecision, V[i].Precision));
end;

function VectorLength(const V: TVector): BigDecimal;
var i: SG;
begin
	Result := 0;
	for i := 0 to Length(V) - 1 do
		Result := Result + {Sqr-remove sign!}(V[i]);
end;

function PowerVector(const V1, V2: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(V1).Power(VectorToNum(V2)));
end;

function LogVector(const V1, V2: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(V2).Ln.Divide(VectorToNum(V1).Ln));
end;

function LnVector(const V: TVector): TVector;
begin
  Result := NumToVector(VectorToNum(V).Ln);
end;

function RoundFloorVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], rmFloor);
end;

function RoundDown(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], rmCeiling);
end;

function RoundUp(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], rmNearestEven);
end;

function RoundEvenVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], rmNearestEven);
end;

function CeilVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], rmCeiling);
end;

function FloorVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], rmFloor);
end;

function TruncVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmDown);
end;

function RoundTowardsZeroVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmDown);
end;

function RoundAwayFromZeroVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmUp);
end;

function RoundHalfCeilVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
  begin
    if V[i] <= 0 then
  		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestDown)
    else
  		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestUp);
  end;
end;

function RoundHalfFloorVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
  begin
    if V[i] <= 0 then
  		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestUp)
    else
  		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestDown);
  end;
end;

function RoundHalfTowardsZeroVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestDown);
end;

function RoundHalfAwayFromZeroVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestUp);
end;

function RoundHalfEvenVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Round(V[i], BigDecimal.RoundingMode.rmNearestEven);
end;

function RoundVector(const V: TVector): TVector;
begin
  Result := RoundHalfEvenVector(V);
end;

function FracVector(const V: TVector): TVector;
var
  i: SG;
  A: BigDecimal;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
  begin
    A := BigDecimal.Abs(V[i]);
		Result[i] := A - BigDecimal.Round(A, rmFloor);
  end;
end;

function AbsVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Abs(V[i]);
end;

function NotVector(const V: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V));
	for i := 0 to Length(V) - 1 do
		Result[i] := BigDecimal.Negate(V[i]) - 1;
end;

function InvVector(const V: TVector): TVector;
begin
	Result := DivideVector(NumToVector(1), V);
end;

function ExpVector(const V: TVector): TVector;
begin
	Result := NumToVector(VectorToNum(V).Exp);
end;

function FactVector(const V: TVector): TVector;
var
	j: SG;
	e, x: BigDecimal;
begin
  e := VectorToNum(V);
  if e.Frac = 0 then
  begin
    if e < 0 then
    begin
      raise EArgumentException.Create('Argument of factorial expect non-negative integer.');
    end
    else
    begin
      x := 1;
      for j := 2 to BigDecimal.Round(e) do
        x := x * j;
    end;
  	Result := NumToVector(x);
  end
  else
  begin
    Result := GammaVector(NumToVector(e + 1));
  end;
end;

function GammaVector(const V: TVector): TVector;
var
	e, x: Extended;
begin
	e := VectorToNum(V).ToFloat;
	x := Gamma(e);
	Result := NumToVector(x);
end;

function GCDVector2(const V1, V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
    Result[i] := BigInteger.GreatestCommonDivisor(V1[i].UnscaledValue, V2[i].UnscaledValue);
end;

function GCDVector(const AData: array of TVector): TVector;
var
  i: Integer;
begin
  if Length(AData) = 0 then
    Exit;

  Result := AData[0];
  for i := 1 to Length(AData) - 1 do
  begin
    Result := GCDVector2(Result, AData[i]);
  end;
end;

function LCMVector(const AData: array of TVector): TVector;
begin
  if Length(AData) = 0 then
  else if Length(AData) = 1 then
    Result := AData[0]
  else if Length(AData) = 2 then
    Result := DivideVector(MultiplyVector(AData[0], AData[1]), GCDVector2(AData[0], AData[1]))
  else
    raise ENotImplemented.Create('LCM expect 2 or less arguments.');
  // TODO else if Length(AData) > 2 then
end;

function CompareVector(const V1, V2: TVector): BigDecimal;
begin
	Result := VectorLength(V1) - VectorLength(V2);
end;

function ShlVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := V1[i] * BigDecimal(2).Power(V2[i]);
end;

function ShrVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := V1[i].Divide(BigDecimal(2).Power(V2[i]));
end;

function AndVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := V1[i].UnscaledValue and V2[i].UnscaledValue;
end;

function OrVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := V1[i].UnscaledValue or V2[i].UnscaledValue;
end;

function XorVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := V1[i].UnscaledValue xor V2[i].UnscaledValue;
end;

function XnorVector(const V1: TVector; const V2: TVector): TVector;
var i: SG;
begin
	SetLength(Result, Length(V1));
	for i := 0 to Length(V1) - 1 do
		Result[i] := not (V1[i].UnscaledValue xor VectorToNum(V2).UnscaledValue);
end;

function NumToVector(const Num: BigDecimal): TVector;
begin
	SetLength(Result, 1);
	Result[0] := Num;
end;

function VectorToNum(const V: TVector): BigDecimal;
begin
	Assert(Length(V) <= 1);
	if Length(V) = 0 then
		Result := 0
	else
		Result := V[0];
end;

function BigDecimalToString(const ABigDeciomal: BigDecimal): string;
begin
  Result := ABigDeciomal.RemoveTrailingZeros(0).ToString;
end;

function VectorToStr(const V: TVector; const OutputFormat: TOutputFormat): string;
var i: SG;
begin
	case Length(V) of
    0: Result := '';
    1: Result := BigDecimalToString(V[0]);
    else
    begin
      Result := '(';
      for i := 0 to Length(V) - 1 do
        Result := Result + BigDecimalToString(V[i]);

      Result := Result + ')';
    end;
	end;
  Replace(Result, '-', CharMinus);
end;

end.
