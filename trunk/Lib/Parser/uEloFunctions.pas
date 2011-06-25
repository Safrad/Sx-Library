//* File:     Lib\Parser\uEloFunctions.pas
//* Created:  2004-03-07
//* Modified: 2007-05-20
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uEloFunctions;

interface

uses uTypes, uVector, uOutputFormat;

const
	ScoreOne = 100;
	MinimalELO = 1250;

function GetElo(Fruitfulness: FA): SG;
function GetArcElo(EloDifference: FA): FA;
function ExpectedRes(Elo, OpElo: SG; Bonus: SG; Rounded: BG): SG;

function ScoreToS(Score: UG; HTML: BG = True): string;
function EloToStr(Elo: U2; const OutputFormat: TOutputFormat): string;

implementation

uses
	SysUtils,
	uNamespace, uMath, uHTML;

var
	EloTable: array[0..50] of SG = (
		765{6}, 677, 589, 538, 501, 470, 444, 422, 401, 383, 368,
		351, 336, 322, 309, 296, 284, 273, 262, 251, 240,
		230, 220, 211, 202, 193, 184, 175, 166, 158, 149,
		141, 133, 125, 117, 110, 102, 95, 87, 80,
		72, 65, 57, 50, 43, 36, 29, 21, 14, 7, 0);
// 0,5 + 1,4217 * 10^(-3) * x - 2,4336 * 10^(-7) * x * Abs(x) - 2,5140 * 10^(-9) * x * Abs(x)^2 + 1,9910 * 10^(-12) * x * Abs(x)^3

function GetElo(Fruitfulness: FA): SG;
begin
	if Fruitfulness <= 0 then
		Result := -EloTable[0]
	else if Fruitfulness >= 1 then
		Result := EloTable[0]
	else if Fruitfulness < 0.5 then
		Result := -EloTable[Round(100 * Fruitfulness)]
	else
		Result := EloTable[100 - Round(100 * Fruitfulness)];
end;

function GetArcElo(EloDifference: FA): FA;
var
	i: SG;
	e0, e1: FA;
begin
	e0 := MaxInt;
	Result := 0;
	for i := 0 to 100 do
	begin
		e1 := Abs(GetElo(i / 100) - EloDifference);
		if e1 < e0 then
		begin
			e0 := e1;
			Result := i / 100;
		end;
	end;
end;

function ExpectedRes(Elo, OpElo: SG; Bonus: SG; Rounded: BG): SG;
const // Calibrate
	WhitePlus = 193;
	StartElo = 1350;
	WhiteBonus = 59;
begin
	if Elo = 0 then Elo := StartElo;
	if OpElo = 0 then OpElo := StartElo;
	if Bonus < 0 then
		Inc(OpElo, WhiteBonus)
	else if Bonus > 0 then
		Inc(Elo, WhiteBonus);

	if Rounded then
	begin
		if Elo >= OpElo + WhitePlus then
		begin
			Result := ScoreOne
		end
		else if Elo + WhitePlus <= OpElo then
		begin
			Result := 0
		end
		else
			Result := ScoreOne div 2;
	end
	else
	begin
		Result := Round(ScoreOne * GetArcElo(Elo - OpElo));
	end;
end;

function Elo(const Args: array of TVector): TVector;
var
	i: SG;
	ArgCount: SG;
	e: FA;
begin
	ArgCount := Length(Args);
	if ArgCount = 0 then
//		Result := 0
	else if ArgCount = 1 then
		Result := NumToVector(GetELO(Args[0][0]))
	else if ArgCount >= 2 then
	begin
		e := 0;
		for i := 0 to ArgCount - 2 do
		begin
			e := e + VectorToNum(Args[i]);
		end;
		Result := NumToVector(e / (ArgCount - 1) +
			GetELO(VectorToNum(Args[ArgCount - 1])) / (ArgCount - 1));
	end;
end;

function ArcElo(const Args: array of TVector): TVector;
var
	i: SG;
	ArgCount: SG;
	e: FA;
begin
	ArgCount := Length(Args);
	if ArgCount = 0 then
		e := 0
	else if ArgCount = 1 then
		e := VectorToNum(Args[0])
	else
	begin
		e := 0;
		for i := 0 to ArgCount - 1 do
		begin
			e := e + VectorToNum(Args[i]);
		end;
	end;
	Result := NumToVector(GetArcElo(e));
end;

function EloC(const Args: array of TVector): TVector;
var
	i, j: SG;
	ArgCount: SG;
	e, e0, e1, MyElo: FA;
begin
	ArgCount := Length(Args);
	if ArgCount < 3 then
//		Result := 0
	else
	begin // Delta / Performance
(*
		e0 := Calc(Node.Args[1]
		e := Calc(Node.Args[0]);
		Result := 0;
		for i := 1 to Node.ArgCount div 2 - 1 do
		begin
			e1 := ArcElo(e0) - Calc(Node.Args[2 * i]));
			Result := Result + (1 * e * (Calc(Node.Args[2 * i + 1]) - e1)) / 1;
		end;*)

		if ArgCount and 1 = 0 then
			MyElo := VectorToNum(Args[1])
		else
			MyElo := 300;
//			Calc(Node.Args[0]); // Skip your elo

		e := 0; // Suma Elo
		e1 := 0; // Suma Score
		j := 0;
		for i := 1 to (ArgCount + 1) div 2 - 1 do
		begin
			e0 := VectorToNum(Args[2 * i - (ArgCount and 1)]);
			if e0 > 0 then
			begin
				if ArgCount and 1 = 0 then
					if e0 < MyElo - 300 then e0 := MyElo - 300; // New for year 2005
				e := e + e0;
				e1 := e1 + VectorToNum(Args[2 * i + 1 - (ArgCount and 1)]); // Score
				Inc(j);
			end;
		end;
		if j > 0 then
		begin
			e0 := Round(e / j); // Avg opponets elo
			if ArgCount and 1 = 0 then
				Result := NumToVector(Round(VectorToNum(Args[0])){Delta} * (e1 - Round(100 * j * GetArcElo(MyElo - e0)) / 100))
			else
				Result := NumToVector(e0{Avg opponets elo} + GetELO(e1 / j));
		end;
	end;
end;

function ScoreToS(Score: UG; HTML: BG = True): string;
var
	Res, Rem: U2;
begin
	DivModU4(Score, ScoreOne, Res, Rem);
	if (Res > 0) or (Rem = 0) then
		Result := NToS(Res)
	else
		Result := '';
	if (Rem <> 0) or (HTML = False) then
	begin
		if HTML then
		begin
			if Rem = ScoreOne div 2 then
				Result := Result + '&frac12;'
			else if Rem = ScoreOne div 4 then
				Result := Result + '&frac14;'
			else if Rem = 3 * ScoreOne div 4 then
				Result := Result + '&frac34;'
			else
				Result := Result + ',' + NToS(Rem, '00');
		end
		else
			Result := Result + ',' + NToS(Rem, '00');
	end;
end;

function EloToStr(Elo: U2; const OutputFormat: TOutputFormat): string;
begin
	if Elo = 0 then
	begin
		case OutputFormat of
		ofHTML: Result := nbsp;
		ofIO: Result := '0';
		else Result := '';
		end;
	end
	else
	begin
		Result := IntToStr(Elo);
	end;
end;

initialization
	AddFunction('Chess', 'Elo', Elo, '...');
	AddFunction('Chess', 'ArcElo', ArcElo, '...');
	AddFunction('Chess', 'EloC', EloC, '...');
end.
