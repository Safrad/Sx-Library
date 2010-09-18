//* File:     Lib\uDNumber.pas
//* Created:  2001-03-01
//* Modified: 2004-04-28
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDNumber;

interface

uses uAdd;

type
	TMainNumber = U1;
	TMNumber = array of TMainNumber;

	TDNumber = class(TObject)
	private
		// Numerator / Denominator
		Number: TMNumber;
		NumberCount: SG;
		Nume: TMNumber;
		NumeCount: SG;
		Deno: TMNumber;
		DenoCount: SG;
		Negative: Boolean; // False: -, True: +
		procedure Shortest;
	protected
		constructor Create;
		destructor Free;

	public
		procedure Resize(V: SG);

		procedure Assign(V: SG); overload;
		procedure Assign(V: Extended); overload;
		procedure Assign(V: TDNumber); overload;

		function Compare(V: TDNumber): SG; // 0: =, 1: V > n, -1: V < n

		procedure Add(V: SG); overload;
		procedure Add(V: TDNumber); overload;

		procedure Sub(V: SG); overload;
		procedure Sub(V: TDNumber); overload;

		procedure MoveLeft(V: SG);
		procedure MoveRight(V: SG);

		procedure Multiply(V: SG); overload;
		procedure Multiply(V: TDNumber); overload;

		procedure DivideBy(V: SG); overload;
		procedure DivideBy(V: Extended); overload;
		procedure DivideBy(V: TDNumber); overload;

		procedure Exp(V: SG); overload;

		procedure Sin(V: TDNumber);

		procedure Sqr;
		procedure Sqrt;

		function ToInt: SG;
		function ToFloat: Extended;
		function ToDec: string;
		function ToHex: string;
	end;

var Reminder: TDNumber;

implementation

uses SysUtils, Math;

const
	MaxNum = 9; //High(TMainNumber);

procedure Cut(Num: TMNumber; var C: SG);
var i: SG;
begin
	for i := C - 1 downto 0 do
	begin
		if Num[i] <> 0 then
			Break;
//		SetLength(Number, i);
		C := i;
	end;
end;

procedure TDNumber.Shortest;
begin
	Cut(Nume, NumeCount);
	Cut(Deno, DenoCount);
end;

procedure TDNumber.Resize(V: SG);
var
	i, NewSize: SG;
begin
	NewSize := V;
//	if AllocByEx(Length(Number), NewSize, DefMemBuffer) then
	SetLength(Number, NewSize);
	for i := NumberCount to V - 1 do
		Number[i] := 0;
	NumberCount := V;
end;

procedure TDNumber.Assign(V: SG);
var i: SG;
begin
	if V = 0 then
		Resize(0)
	else
	begin
		Resize(4);
		Negative := V < 0;
		for i := 0 to 3 do
		begin
			Number[i] := V mod (MaxNum + 1);
			V := V div (MaxNum + 1);
		end;
		Shortest;
	end;
end;

procedure TDNumber.Assign(V: Extended);
var i: SG;
begin
	if V = 0 then
		Resize(0)
	else
	begin
		Resize(4);
		Negative := V < 0;
		for i := 0 to 3 do
		begin
			Number[i] := Round(V) mod (MaxNum + 1);
			V := V / (MaxNum + 1);
		end;
		Shortest;
	end;
end;

procedure TDNumber.Assign(V: TDNumber);
var i: SG;
begin
	Negative := V.Negative;
	Resize(V.NumberCount);
	for i := 0 to NumberCount - 1 do
	begin
		Number[i] := V.Number[i];
	end;
end;

function TDNumber.Compare(V: TDNumber): SG; // 0: =, 1: V > n, -1: V < n
var i: SG;
begin
	if V.NumberCount > NumberCount then
	begin
		Result := -1;
	end
	else if V.NumberCount < NumberCount then
	begin
		Result := 1;
	end
	else
	begin
		Result := 0;
		for i := NumberCount - 1 downto 0 do
		begin
			if V.Number[i] > Number[i] then
			begin
				Result := -1;
				Break;
			end
			else if V.Number[i] < Number[i] then
			begin
				Result := 1;
				Break;
			end;
		end;
	end;
end;

procedure TDNumber.Add(V: SG);
var Num: TDNumber;
begin
	Num := TDNumber.Create;
	Num.Assign(V);
	Add(Num);
	Num.Free;
end;

procedure TDNumber.Add(V: TDNumber);
var
	i, LastInc, n, MaxN: SG;

begin
	MaxN := Max(NumberCount, V.NumberCount);
	Resize(MaxN + 1);
	V.Resize(MaxN + 1);

	if Negative = V.Negative then
	begin
		LastInc := 0;
		for i := 0 to MaxN - 1 do
		begin
			if Number[i] + V.Number[i] + LastInc > MaxNum then
				LastInc := 1 else LastInc := 0;

			Inc(Number[i], V.Number[i] + LastInc);
		end;
		if LastInc = 1 then Number[MaxN] := 1;
	end
	else
	begin
		LastInc := 0;
		for i := 0 to MaxN - 1 do
		begin
			if Number[i] + V.Number[i] + LastInc > MaxNum then
				LastInc := 1 else LastInc := 0;
			n := Number[i] - V.Number[i] - LastInc;
			if n < 0 then
			begin
				LastInc := 1;
				Number[i] := MaxNum + 1 + n;
			end
			else
			begin
				LastInc := 0;
				Number[i] := n;
			end;
		end;
		if LastInc = 1 then Negative := not Negative;
	end;
	Shortest;
end;

procedure TDNumber.Sub(V: SG);
var Num: TDNumber;
begin
	Num := TDNumber.Create;
	Num.Assign(V);
	Sub(Num);
	Num.Free;
end;

procedure TDNumber.Sub(V: TDNumber);
var
	Num: TDNumber;
begin
	Num := TDNumber.Create;
	Num.Assign(V);
	Num.Negative := not Num.Negative;
	Add(Num);
	Num.Free;
end;

procedure TDNumber.MoveLeft(V: SG);
var i: SG;
begin
	if V = 0 then Exit;
	Resize(NumberCount + V);

	for i := NumberCount - 1 downto V do
	begin
		Number[i] := Number[i - V];
	end;
	for i := 0 to V - 1 do
		Number[i] := 0;
end;

procedure TDNumber.MoveRight(V: SG);
var i: SG;
begin
	if V = 0 then Exit;

	for i := 0 to NumberCount - V - 1 do
	begin
		Number[i] := Number[i + V];
	end;
	Resize(NumberCount - V);
end;

procedure TDNumber.Multiply(V: SG);
var
	i: SG;
	Num1, Num2: TDNumber;
begin
	if (NumberCount <> 0) and (V <> 0) then
	begin
		Num1 := TDNumber.Create;
		Num2 := TDNumber.Create;
		for i := 0 to NumberCount - 1 do
		begin
			Num2.Assign(Number[i] * V);
			Num2.MoveLeft(i);
			Num1.Add(Num2);
		end;
		Assign(Num1);
		Num2.Free;
		Num1.Free;
	end;
end;

procedure TDNumber.Multiply(V: TDNumber);
var
	i, j: SG;
	Num1, Num2: TDNumber;
begin
	if (NumberCount <> 0) and (V.NumberCount <> 0) then
	begin
		Num1 := TDNumber.Create;
		Num2 := TDNumber.Create;
		for j := V.NumberCount - 1 downto 0 do
		begin
			Num1.MoveLeft(1);
			for i := 0 to NumberCount - 1 do
			begin
				Num2.Assign(Number[i] * V.Number[j]);
				Num2.MoveLeft(i);
				Num1.Add(Num2);
			end;
		end;
		Assign(Num1);
		Num2.Free;
		Num1.Free;
	end;
end;

procedure TDNumber.DivideBy(V: SG);
var
//	i, Rem: SG;
	Num: TDNumber;

begin
	Num := TDNumber.Create;
	Num.Assign(V);
	DivideBy(Num);
	Num.Free;
{	Num := TDNumber.Create;
	Num.Resize(NumberCount);
	Rem := 0;
	for i := NumberCount - 1 downto 0 do
	begin
		if (Number[i] + Rem) < V then
		begin
			Num.Number[i] := 0;
			Rem := Rem * (MaxNum + 1) + (Number[i] mod V);
		end
		else
		begin
			Num.Number[i] := (Number[i] + Rem) div V;
			Rem := (Number[i] mod V);
		end;
	end;
	Assign(Num);
	Reminder.Assign(Rem);
	Num.Free;}
end;

procedure TDNumber.DivideBy(V: Extended);
var
//	i, Rem: SG;
	Num: TDNumber;

begin
	Num := TDNumber.Create;
	Num.Assign(V);
	DivideBy(Num);
	Num.Free;
{	Num := TDNumber.Create;
	Num.Resize(NumberCount);
	Rem := 0;
	for i := NumberCount - 1 downto 0 do
	begin
		if (Number[i] + Rem) < V then
		begin
			Num.Number[i] := 0;
			Rem := Rem * (MaxNum + 1) + (Number[i] mod V);
		end
		else
		begin
			Num.Number[i] := (Number[i] + Rem) div V;
			Rem := (Number[i] mod V);
		end;
	end;
	Assign(Num);
	Reminder.Assign(Rem);
	Num.Free;}
end;

procedure TDNumber.DivideBy(V: TDNumber);
var
	i, j, k: SG;
	Num, Num2: TDNumber;
begin
	Num := TDNumber.Create;
	Num.Resize(NumberCount);
	Num2 := TDNumber.Create;
	Reminder.Assign(Self);
	{
	NLP dvojk only: A/B
	C=A
	1. C=C-B
	2.(C>0 or C<0)->result
	3.C shl 1
	4.C := C(- or +)B
	goto 1

	// NLP: A/B
	C = A
	1. C=C-B
	2.(C>0 or C<0)->result
	4.C := C(- or +)B
	5. C=C shl 1
	6.goto 1
	}

	for i := NumberCount - 1 downto 0 do
	begin
		Number[i] := 0;
		k := Reminder.NumberCount;
		while True do
		begin
			Num2.Assign(Reminder);
			Num2.MoveRight(k);
			if Num2.Compare(V) > -1 then Break;
			Dec(k); if k < 0 then Break;
		end;

		for j := MaxNum downto 0 do
		begin
			Num.Assign(V);
			Num.Multiply(j);

			if Num2.Compare(Num) > -1 then
			begin // V <= n
				Number[i - k] := j;
				Num.MoveLeft(k);
				Reminder.Sub(Num);
				Break;
			end;
		end;
	end;
	Num2.Free;
	Num.Free;
end;

procedure TDNumber.Exp(V: SG);
var
	i: SG;
	Num: TDNumber;
begin
	Num := TDNumber.Create;
	Num.Assign(Self);
	for i := 0 to V - 1 do
		Multiply(Num);
	Num.Free;
end;
{
procedure TDNumber.Log(V: SG);
begin

end;}

procedure TDNumber.Sqr;
begin
	Exp(2);
end;

procedure TDNumber.Sqrt;
var N: TDNumber;
begin
{	N := TDNumber.Create;
	N.Assign(0.5);
	N.Exp(N);
	N.Free;}
end;


procedure TDNumber.Sin(V: TDNumber);
begin
	DivideBy(2 * pi);
	Assign(System.Sin(Reminder.ToFloat));
end;

function TDNumber.ToHex: string;
const
	HexToStr: array[0..15] of Char = (
		'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'A', 'B', 'C', 'D', 'E', 'F');
var i: SG;
begin
	if NumberCount = 0 then
		Result := '0'
	else
	begin
		Result := '';
		for i := NumberCount - 1 downto 0 do
		begin
			Result := Result + HexToStr[Number[i] shr 4];
			Result := Result + HexToStr[Number[i] and $f];
		end;
	end;
end;

function TDNumber.ToInt: SG;
var i: SG;
begin
	Result := 0;
	for i := 0 to Min(NumberCount - 1, 4) do
		Result := Result + Number[i] shl (i * 8);
	if Negative then Result := -Result;
end;

function TDNumber.ToFloat: Extended;
var i: SG;
begin
	Result := 0;
	for i := 0 to Min(NumberCount - 1, 4) do
		Result := Result + Number[i] shl (i * 8);
	if Negative then Result := -Result;
end;

function TDNumber.ToDec: string;
var
	i: SG;
	Num: TDNumber;
begin
	Num := TDNumber.Create;
	Num.Assign(Self);
	for i := 0 to NumberCount - 1 do
	begin
		Num.DivideBy(10);
		Result := Char(Ord('0') + Reminder.ToInt) + Result;
	end;
	Num.Free;
end;

constructor TDNumber.Create;
begin
	Assign(0);
end;

destructor TDNumber.Free;
begin
	Assign(0);
end;

initialization
	Reminder := TDNumber.Create;
finalization
	Reminder.Free; Reminder := nil;
end.
