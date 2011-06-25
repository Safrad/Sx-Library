// * File:     Lib\Parser\uStatisticsFunctions.pas
// * Created:  2004-03-07
// * Modified: 2009-08-30
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uStatisticsFunctions;

interface

uses
	uTypes,
	uVector;

function CountData(const Data: array of TVector): TVector;
function Minimum(const Data: array of TVector): TVector;
function Maximum(const Data: array of TVector): TVector;
function Sum(const Data: array of TVector): TVector;
function Sumx(const Data: array of TVector; x: TVector): TVector;
function Avg(const Data: array of TVector): TVector;
function _mx(const Data: array of TVector; x: TVector): TVector;
function ux(const Data: array of TVector; x: TVector): TVector;
function Variance0(const Data: array of TVector): TVector;
function VarianceCoef(const Data: array of TVector): TVector;
function Skew(const Data: array of TVector): TVector;

implementation

uses
	uNamespace,
	Math;

function CountData(const Data: array of TVector): TVector;
begin
	Result := NumToVector(Length(Data));
end;

function _Length(const Data: array of TVector): TVector;
var
	i: SG;
begin
	Result := nil;
//	if Length(Data) > 0 then
	begin
		Result := nil;
		for i := 0 to Length(Data) - 1 do
		begin
			Result := PlusVector(Result, SqrVector(Data[i]));
		end;
		Result := SqrtVector(Result);
	end;
{		Result := 0;
		for i := 0 to Node.ArgCount - 1 do
			Result := Result + Sqr(Calc(Node.Args[i]));
		Result := Sqrt(Result);}
end;

function Minimum(const Data: array of TVector): TVector;
var i: SG;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
	begin
		Result := Data[0];
		for i := 1 to Length(Data) - 1 do
		begin
			if CompareVector(Result, Data[i]) > 0 then
				Result := Data[i];
		end;
	end;
end;

function Maximum(const Data: array of TVector): TVector;
var i: SG;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
	begin
		Result := Data[0];
		for i := 1 to Length(Data) - 1 do
		begin
			if CompareVector(Result, Data[i]) < 0 then
				Result := Data[i];
		end;
	end;
end;

function Sum(const Data: array of TVector): TVector;
var i: SG;
begin
	Result := nil;
	for i := 0 to Length(Data) - 1 do
	begin
		Result := PlusVector(Result, Data[i]);
	end;
end;

function Sumx(const Data: array of TVector; x: TVector): TVector;
var i: SG;
begin
	Result := nil;
	for i := 0 to Length(Data) - 1 do
	begin
		Result := PlusVector(Result, PowerVector(Data[i], x));
	end;
end;

// m1 = EX - Average Value (stedn hodnota)
function Avg(const Data: array of TVector): TVector;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
		Result := DivideVector(Sum(Data), NumToVector(Length(Data)));
end;

function GeometricMean(const Data: array of TVector): TVector;
var
	i: SG;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
	begin
		Result := Data[0];
		for i := 1 to Length(Data) - 1 do
		begin
			Result := MultiplyVector(Result, Data[i]);
		end;
		Result := PowerVector(Result, NumToVector(1 / Length(Data)));
	end;
end;

function HarmonicMean(const Data: array of TVector): TVector;
var
	i: SG;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
	begin
		Result := DivideVector(NumToVector(1), Data[0]);
		for i := 1 to Length(Data) - 1 do
		begin
			Result := PlusVector(Result, DivideVector(NumToVector(1), Data[i]));
		end;
		Result := DivideVector(NumToVector(Length(Data)), Result);
	end;
end;

// mx
function _mx(const Data: array of TVector; x: TVector): TVector;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
		Result := DivideVector(Sumx(Data, x), NumToVector(Length(Data)));
end;

// ux
function ux(const Data: array of TVector; x: TVector): TVector;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
		Result := MinusVector(_mx(Data, x), SqrVector(_mx(Data, MinusVector(x, NumToVector(1)))));
end;

function AbsoluteDeviation(const Data: array of TVector): TVector;
var
	i: SG;
	A: TVector;
begin
	if Length(Data) <= 1 then
		Result := nil
	else
	begin
{		Result := 0;
		A := Avg(Data);
		for i := 0 to Length(Data) - 1 do
		begin
			Result := Result + Sqr(Data[i]);
		end;
		Result := Result / Length(Data) - Sqr(A);}


		Result := NumToVector(0);
		A := Avg(Data);
		for i := 0 to Length(Data) - 1 do
		begin
			Result := PlusVector(Result, AbsVector(MinusVector(Data[i], A)));
		end;
		Result := DivideVector(Result, NumToVector(Length(Data)));
	end;
end;

function StandardDeviation(const Data: array of TVector): TVector;
begin
	Result := SqrtVector(Variance0(Data));
end;

// u2 = m2 - m1^2 = var (variance) X = DX = "o^2" = E(X - EX)^2 - rozptyl = E(X^2) - (EX)^2
// o = Sqrt(var X) smerodatna odchylka
function Variance0(const Data: array of TVector): TVector;
var
	i: SG;
	A: TVector;
begin
	if Length(Data) <= 1 then
		Result := nil
	else
	begin
{		Result := 0;
		A := Avg(Data);
		for i := 0 to Length(Data) - 1 do
		begin
			Result := Result + Sqr(Data[i]);
		end;
		Result := Result / Length(Data) - Sqr(A);}


		Result := nil;
		A := Avg(Data);
		for i := 0 to Length(Data) - 1 do
		begin
			Result := PlusVector(Result, SqrVector(MinusVector(Data[i], A)));
		end;
		Result := DivideVector(Result, NumToVector(Length(Data)));
	end;
end;

function VarianceCoef(const Data: array of TVector): TVector;
begin
	Result := SqrtVector(DivideVector(Variance0(Data), Avg(Data)));
end;

function Skew(const Data: array of TVector): TVector;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
		Result := DivideVector(ux(Data, NumToVector(3)), PowerVector(SqrtVector(Variance0(Data)), NumToVector(3)));
end;

function RandomFunction(const Data: array of TVector): TVector;
begin
	if Length(Data) = 0 then
		Result := NumToVector(Random(MaxInt) / MaxInt)
	else //if Length(Data) = 1 then
		Result := NumToVector(Random(Round(VectorToNum(Data[0]))));
{	else if Length(Data) >= 2 then
	begin
		e := Calc(Node.Args[0]);
		Result := e + Random(Round(Calc(Node.Args[1]) - e))
	end;}
end;

initialization
	AddFunction('Statistics', 'Avg', Avg, 'http://en.wikipedia.org/wiki/Arithmetic_mean');
	AddFunction('Statistics', 'AvgG', GeometricMean, 'http://en.wikipedia.org/wiki/Geometric_mean');
	AddFunction('Statistics', 'AvgH', HarmonicMean, 'http://en.wikipedia.org/wiki/Harmonic_mean');
	// http://en.wikipedia.org/wiki/Arithmetic-geometric_mean
	AddFunction('Statistics', 'Count', CountData, 'http://en.wikipedia.org/wiki/Count_data');
	AddFunction('Statistics', 'Min', Minimum, 'http://en.wikipedia.org/wiki/Minimum');
	AddFunction('Statistics', 'Max', Maximum, 'http://en.wikipedia.org/wiki/Maximum');
	AddFunction('Statistics', 'Skew', Skew, 'http://en.wikipedia.org/wiki/Skewness');
	AddFunction('Statistics', 'Sum', Sum, 'http://en.wikipedia.org/wiki/Sum');
//	AddFunction('Statistics', 'Sumx', Sumx, '');
//	AddFunction('Statistics', 'ux', ux, '');
//	AddFunction('Statistics', 'mx', _mx, '');
	AddFunction('Statistics', 'dx', AbsoluteDeviation, 'http://en.wikipedia.org/wiki/Absolute_deviation');
	AddFunction('Statistics', 'Variance0', Variance0, 'http://en.wikipedia.org/wiki/Variance');
	AddFunction('Statistics', 'ex', StandardDeviation, 'http://en.wikipedia.org/wiki/Standard_deviation');
	AddFunction('Statistics', 'VarianceCoef', VarianceCoef, '');


	AddFunction('Statistics', 'Random', RandomFunction, 'http://en.wikipedia.org/wiki/Random_function');
	// http://en.wikipedia.org/wiki/Median
end.



