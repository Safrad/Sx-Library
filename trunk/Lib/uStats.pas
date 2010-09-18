unit uStats;

interface

uses uTypes;
// Statistics

function CountData(const Data: array of FG): FG;
function Minimum(const Data: array of FG): FG;
function Maximum(const Data: array of FG): FG;
function Sum(const Data: array of FG): FG;
function Sumx(const Data: array of FG; x: SG): FG;
function Avg(const Data: array of FG): FG;
function _mx(const Data: array of FG; x: SG): FG;
function ux(const Data: array of FG; x: SG): FG;
function Variance0(const Data: array of FG): FG;
function VarianceCoef(const Data: array of FG): FG;
function Skew(const Data: array of FG): FG;

implementation

uses Math;

// Statistics

function CountData(const Data: array of FG): FG;
begin
	Result := Length(Data);
end;

function Minimum(const Data: array of FG): FG;
var i: SG;
begin
	if Length(Data) <= 0 then
		Result := NaN
	else
		Result := MaxDouble;
	for i := 0 to Length(Data) - 1 do
	begin
		if Result > Data[i] then
			Result := Data[i];
	end;
end;

function Maximum(const Data: array of FG): FG;
var i: SG;
begin
	if Length(Data) <= 0 then
		Result := NaN
	else
		Result := -MaxDouble;
	for i := 0 to Length(Data) - 1 do
	begin
		if Result < Data[i] then
			Result := Data[i];
	end;
end;

function Sum(const Data: array of FG): FG;
var i: SG;
begin
	Result := 0;
	for i := 0 to Length(Data) - 1 do
	begin
		Result := Result + Data[i];
	end;
end;

function Sumx(const Data: array of FG; x: SG): FG;
var i: SG;
begin
	Result := 0;
	for i := 0 to Length(Data) - 1 do
	begin
		Result := Result + Power(Data[i], x);
	end;
end;

// m1 = EX - Average Value (støední hodnota)
function Avg(const Data: array of FG): FG;
begin
	if Length(Data) <= 0 then
		Result := NaN
	else
		Result := Sum(Data) / Length(Data);
end;

// mx
function _mx(const Data: array of FG; x: SG): FG;
begin
	if Length(Data) <= 0 then
		Result := NaN
	else
		Result := Sumx(Data, x) / Length(Data);
end;

// ux
function ux(const Data: array of FG; x: SG): FG;
begin
	if Length(Data) <= 0 then
		Result := NaN
	else
		Result := _mx(Data, x) - Sqr(_mx(Data, x - 1));
end;

// u2 = m2 - m1^2 = var (variance) X = DX = "o^2" = E(X - EX)^2 - rozptyl = E(X^2) - (EX)^2
// o = Sqrt(var X) smerodatna odchylka
function Variance0(const Data: array of FG): FG;
var
	i: SG;
	A: FG;
begin
	if Length(Data) <= 1 then
		Result := 0
	else
	begin
{		Result := 0;
		A := Avg(Data);
		for i := 0 to Length(Data) - 1 do
		begin
			Result := Result + Sqr(Data[i]);
		end;
		Result := Result / Length(Data) - Sqr(A);}


		Result := 0;
		A := Avg(Data);
		for i := 0 to Length(Data) - 1 do
		begin
			Result := Result + Sqr(Data[i] - A);
		end;
		Result := Result / Length(Data);
	end;
end;

function VarianceCoef(const Data: array of FG): FG;
begin
	Result := Sqrt(Variance0(Data) / Avg(Data));
end;

function Skew(const Data: array of FG): FG;
begin
	if Length(Data) <= 0 then
		Result := NaN
	else
		Result := ux(Data, 3) / Power(Sqrt(Variance0(Data)), 3);
end;

end.

