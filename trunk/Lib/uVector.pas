unit uVector;

interface

uses
	uTypes,
	uFormat;

type
	TVector = array of FA;

function NullVector: TVector;
function NegVector(const V: TVector): TVector;
function PlusVector(const V1, V2: TVector): TVector;
function SumVector(const V: TVector): FA;
function MultiplyVector(const V1, V2: TVector): TVector;
function DivideVector(const V1, V2: TVector): TVector;
function ModuloVector(const V1, V2: TVector): TVector;

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

