//* File:     Lib\Parser\uStatisticsFunctions.pas
//* Created:  2004-03-07
//* Modified: 2007-09-27
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

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

// m1 = EX - Average Value (støední hodnota)
function Avg(const Data: array of TVector): TVector;
begin
	if Length(Data) <= 0 then
		Result := nil
	else
		Result := DivideVector(Sum(Data), NumToVector(Length(Data)));
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
	AddFunction('Statistics', 'Avg', Avg, 'Average value of set.');
	AddFunction('Statistics', 'Count', CountData, '');
	AddFunction('Statistics', 'Min', Minimum, '');
	AddFunction('Statistics', 'Max', Maximum, '');
	AddFunction('Statistics', 'Skew', Skew, '');
	AddFunction('Statistics', 'Sum', Sum, '');
//	AddFunction('Statistics', 'Sumx', Sumx, '');
//	AddFunction('Statistics', 'ux', ux, '');
	AddFunction('Statistics', 'Variance0', Variance0, '');
	AddFunction('Statistics', 'VarianceCoef', VarianceCoef, '');
//	AddFunction('Statistics', 'mx', _mx, '');
	AddFunction('Statistics', 'Random', RandomFunction, '');
end.



