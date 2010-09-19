//* File:     Lib\uFormat.pas
//* Created:  2004-03-07
//* Modified: 2005-11-26
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uFormat;

interface

uses
	uTypes;

// Format functions

// Number Format
var
	NativeSymbols: string[10];

	DecimalSeparator: string[3]; // Decimal symbol
	DigitsAfterDecimal: SG; // No. of digits after decimal
	ThousandSeparator: string[3]; // Digit grouping symbol
	UseThousandSeparator: BG = True; // Custom
	ThousandGroup: SG; // Digit grouping
	FractionGroup: SG;
	NegSymbol: string[4]; // Negatove sing symbol
	PosSymbol: string[4]; // Negatove sing symbol
	NegFormat: SG; // Negative number format
	LeadingZero: SG; // Display leading zeros
	ListSeparator: string[3]; // List separator

// Time Format
	TimeSeparator: string[3];
	ICentury: SG;

{
NToS(S8, ...); <-> StrToValI(SG,UG), StrToValS8, U1(..., False, ...);

FToS(F10, ...) <-> StrToValExt StrToE(..., False);

UseWinFormat:
False: 2,102,454,545.45644; Disk File Input/Output
True: 2t102t454t545d4564; User Input, Graphics Output

IntToStr	StrToInt ; 2102454545;  Windows Registry, IE

}
{
	Decimals:
	-2	2.2	Maximum decimals
	+2	2.20	Fixed decimals
}
{
	UseWinFormat:
	False:
		20030923
		2003-09-23
		2003-9-23
		09/23/2003
		9/23/2003
		23.09.2003
		23.9.2003
}
// Data To Str
var
	NumericBase: U1 = 10;
const
	MaxNumericBase = 36;
	NumberTable: array[0..MaxNumericBase - 1] of Char = (
		'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
		'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
		'U', 'V', 'W', 'X', 'Y', 'Z'{, 'a', 'b', 'c', 'd',
		'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
		'y', 'z', '-', '*'});

var UseNA: BG;

function NToS(const Num: Int64): string; overload;
function NToS(const Num: Int64; const Decimals: SG): string; overload;
function NToS(const Num: Int64; const UseFormat: string): string; overload;
function NToS(const Num: Int64; const UseWinFormat: BG): string; overload;
function NToS(const Num: Int64; const UseWinFormat: BG; const Decimals: SG): string; overload;
//function NToHS(Num: S8): string;

//function NumToStr(Num: S8; const Base: SG): string;

function FToS(Num: Extended): string; overload;
function FToS(Num: Extended; const UseWinFormat: BG): string; overload;

function BToStr(const B: S4): string; overload;
function BToStr(const B: S8): string; overload;
function NodesToS(const Value: U8): string;

procedure msToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
type
	TDisplay = (diDHMSD, diHHMSD, diHMSD, diMSD, diSD);

{Decimals
-3: 0:34.34
3: 0:34.340
}

// function SToMs(const Str: string): SG; // MsToStr<-

function StrToF4(Str: string): F4;
function StrToF8(Str: string): F8;
function StrToFA(Str: string): FA;
function StrToSG(Str: string): SG;
function SToTime(Str: string): TDateTime;


function MsToStr(DT: Int64;
	Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string; overload;
function MsToStr(DT: Int64; const UseWinFormat: BG;
	Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string; overload;

function DateToS(var Year, Month, Day: U2): string; overload;
function DateToS(D: TDateTime): string; overload;
function TimeToS(T: TDateTime): string;
function DateTimeToS(DT: TDateTime): string;
function DTToStr(DT: TDateTime): string; // UseWinFormat = True
function PhoneToStr(Phone: U8): string;

function RemoveEscape(s: string): string;
function AddEscape(s: string): string; // 2.8x larger for random data

implementation

uses
	SysUtils, Windows,
	uMath, uStrings;

procedure AddMinusStr(var Result: string; const UseWinFormat: BG);
begin
	if UseWinFormat then
	begin
		case NegFormat of
		0: Result := '(' + Result + ')';
		1: Result := NegSymbol + Result;
		2: Result := NegSymbol + ' ' + Result;
		3: Result := Result + NegSymbol;
		4: Result := Result + ' ' + NegSymbol;
		else Result := NegSymbol + Result;
		end;
	end
	else
		Result := '-' + Result;
end;

function NumToStr(Num: S8; const Base: SG): string;
var
	M: SG;
//	Minus: BG;
begin
	Result := '';
	if Num < 0 then
	begin
		Num := -Num;
//		Minus := True;
	end;
{	else
		Minus := False;}
	Assert((Base >= 2) and (Base <= MaxNumericBase));
	while True do
	begin
//		DivModS64(Num, Base, D, M);
		M := Num mod Base;
		Num := Num div Base;
		Result := NumberTable[M] + Result;
		if Num = 0 then Break;
	end;
//	if Minus then AddMinusStr(Result);
end;

function NToS(const Num: Int64; const UseFormat: string): string;
var
	Nums: string;
	i, j: SG;
	PointPos: SG;
	NumFound: BG;
begin
	Result := '';
	if (Num = Low(Num)) or (Num = High(Num)) then
	begin
		Result := 'Out of 64-bit range';
		Exit;
	end;

	if (UseNA) and (Num <= 0) then
	begin
		Result := 'N/A';
		Exit;
	end;

	if NumericBase = 10 then
		Nums := IntToStr(Abs(Num))
	else
		Nums := NumToStr(Abs(Num), NumericBase);
	j := Length(Nums);
	PointPos := Pos('.', UseFormat);
	if PointPos = 0 then PointPos := High(PointPos);
	NumFound := False;
	for i := Length(UseFormat) downto 1 do
	begin
		if (UseFormat[i] = '0') or (UseFormat[i] = '#') then
		begin
			if j >= 1 then
			begin
				if Nums[j] <> '0' then
				begin
					NumFound := True;
					Result := Nums[j] + Result;
				end
				else
				begin
					if (UseFormat[i] = '#') and (((i > PointPos) and (NumFound = False))) then
						Result := ' ' + Result
					else
					begin
						NumFound := True;
						Result := '0' + Result;
					end;
				end;
			end
			else
			begin
				if (UseFormat[i] = '#') and ((i < PointPos) or (NumFound = False)) then
					Result := ' ' + Result
				else
				begin
					NumFound := True;
					Result := '0' + Result;
				end;
			end;
			Dec(j);
		end
		else if (UseFormat[i] = '.') then
		begin
			if NumFound then
			begin
				Result := '.' + Result
			end
			else
				Result := ' ' + Result;
		end
		else if (UseFormat[i] = ',') then
		begin
			if j > 0 then
			begin
				Result := UseFormat[i] + Result
			end
			else
				Result := ' ' + Result;
		end
		else if UseFormat[i] = ' ' then
		begin
			Result := ' ' + Result;
		end
		else if UseFormat[i] = '+' then
		begin
			if Num < 0 then
				Result := '-' + Result
			else
				Result := '+' + Result;

		end
		else if UseFormat[i] = '-' then
		begin
			if Num < 0 then
				Result := '-' + Result
			else
				Result := ' ' + Result;
		end
		{$ifopt d+}
		else
			Assert(False, 'Unknown char in format string'){$endif};
	end;
end;

function NToS(const Num: Int64): string;
begin
	Result := NToS(Num, True, 0);
end;

function NToS(const Num: Int64; const Decimals: SG): string;
begin
	Result := NToS(Num, True, Decimals);
end;

function NToS(const Num: Int64; const UseWinFormat: BG): string;
begin
	Result := NToS(Num, UseWinFormat, 0);
end;

// 454,545,455.456465; 0.045
function NToS(const Num: Int64; const UseWinFormat: BG; const Decimals: SG): string;
var
	DecimalSep, ThousandSep: string[3];
	ThousandGr, FractionGr: SG;

	Nums: string;
	i, M: SG;
	FirstNotZero: BG;
	c: Char;
begin
	Result := '';
	if (Num = Low(Num)) or (Num = High(Num)) then
	begin
		Result := 'Out of 64-bit range';
		Exit;
	end;
	if (UseNA) and (Num <= 0) then
	begin
		Result := 'N/A';
		Exit;
	end;

	if UseWinFormat then
	begin
		DecimalSep := DecimalSeparator;
		ThousandSep := ThousandSeparator;
		ThousandGr := ThousandGroup;
		FractionGr := FractionGroup;
	end
	else
	begin
		DecimalSep := '.';
		ThousandSep := ',';
		ThousandGr := 3;
		FractionGr := 3;
	end;
	if UseThousandSeparator = False then ThousandSep := '';

	if Num = 0 then
		Nums := ''
	else if NumericBase = 10 then
		Nums := IntToStr(Abs(Num))
	else
		Nums := NumToStr(Abs(Num), NumericBase);

	M := -Abs(Decimals);
	i := Length(Nums);
	FirstNotZero := Decimals >= 0;
	while True do
	begin
		if i > 0 then
		begin
			c := Nums[i];
		end
		else
			c := '0';

		if c = '0' then
		begin
			if FirstNotZero then
				Result := '0' + Result;
		end
		else
		begin
			FirstNotZero := True;
			Result := c + Result
		end;

		Dec(i);
		Inc(M);

		if (i < 1) and (M > 0) then Break;

		if M = 0 then
		begin
			if FirstNotZero then
				Result := DecimalSep + Result;
			FirstNotZero := True;
			if i < 1 then
			begin
				if UseWinFormat then
				begin
					if LeadingZero = 1 then
						Result := '0' + Result;
				end
				else
				begin
					Result := '0' + Result;
				end;
				Break;
			end;
		end
		else if (M < 0) then
		begin
			if (FractionGr > 0) and (FirstNotZero) then
				if Abs(M) mod FractionGr = 0 then
				begin
					Result := ThousandSep + Result
				end;
		end
		else if (M > 0) then
		begin
			if ThousandGr > 0 then
				if Abs(M) mod ThousandGr = 0 then
				begin
					Result := ThousandSep + Result
				end;
		end;
	end;

	if Num < 0 then
	begin
		AddMinusStr(Result, UseWinFormat);
	end;
end;

{
function NToHS(Num: S8): string;
begin
	Result := '';
	repeat
		case Num and $f of
		0..9: Result := Chr(Ord('0') + (Num and $f)) + Result;
		else Result := Chr(Ord('A') + (Num and $f) - $a) + Result;
		end;
		Num := Num shr 4;
	until Num = 0;
end;}

function FToS(Num: Extended; const UseWinFormat: BG): string;
var
	D: SG;
	Nu, eps: Extended;
begin
	D := 0;
	Nu := Num;
	eps := 5.6e-18; // D???
	while True do
	begin
		if Abs(Frac(Nu)) <= eps{MinExtended} then Break;
		if Abs(Nu) < High(S8) div NumericBase then
		begin
			Nu := Nu * NumericBase;
			eps := eps * NumericBase;
		end
		else
		begin
{			Result := FloatToStr(Num);
			Exit;}
			Break;
		end;
		Inc(D);
	end;

	Result := NToS(Round(Nu), UseWinFormat, D);
end;

function FToS(Num: Extended): string;
begin
	Result := FToS(Num, True);
end;

{
function Using(const Typ: string; const Num: Int64): string;
label LExit;
var
	inp: string;
	inpP: Integer;
	FixedSign: Boolean;
	Poin: Integer;
	DelSpace: Boolean;
	i: Integer;
	Fra: Boolean;
begin
	Result := '';
	if UseFormat = '' then
	begin
		Exit;
	end;

	Poin := Pos('.', UseFormat);
	if UseFormat[Length(UseFormat)] = '~' then DelSpace := True else DelSpace := False;

	if Num = 0 then inp := '' else inp := IntToStr(Abs(Num));
	inpP := Length(inp);

	FixedSign := False;
	for i := 1 to Length(UseFormat) do
	begin
		if (UseFormat[i] = '+') or (UseFormat[i] = '-') then
		begin
			FixedSign := True;
			Break;
		end;
	end;

	Fra := False;
	for i := Length(UseFormat) downto 1 do
	begin
		case UseFormat[i] of
		'#':
		begin
			if inpP > 0 then
			begin
				if (inp[inpP] <> '0') or (Fra = True) then
				begin
					Result := inp[inpP] + Result;
					Fra := True;
				end
				else
					if DelSpace = False then Result := ' ' + Result;
				Dec(inpP);
			end
			else
			begin
				if (i < Poin) and (FixedSign = False) and (num < 0) then
				begin
					FixedSign := True;
					Result := '-' + Result;
				end
				else
				begin
					if (Fra = True) and ((i > Poin) and (Poin <> 0)) then
					begin
						Result := '0' + Result;
						Fra := True;
					end
					else
						if DelSpace = False then Result := ' ' + Result;
				end;
			end;
		end;
		'0':
		begin
			if inpP > 0 then
			begin
				Result := inp[inpP] + Result;
				Fra := True;
				Dec(inpP);
			end
			else
			begin
				Result := '0' + Result;
				Fra := True;
			end;
		end;
		'~': 
		begin
			if i = 1 then
			begin
				while Length(Result) > 0 do
				begin
					if Result[1] = ' ' then Delete(Result, 1, 1) else goto LExit;
				end;
				goto LExit;
			end;
		end;
		'+':
		begin
			if num = 0 then
				Result := ' ' + Result
			else
			if num > 0 then
				Result := '+' + Result
			else
				Result := '-' + Result;
		end;
		'-':
		begin
			if num >= 0 then
				Result := ' ' + Result
			else
				Result := '-' + Result;
		end;
		'.':
		begin
			if Fra = False then
			begin
				if DelSpace = False then Result := Result + ' ';
				fra := True;
			end
			else
			begin
				if UseWinFormat then
					Result := DecimalSeparator + Result
				else
					Result := '.' + Result;
			end;
			if UseFormat[1] = '~' then DelSpace := True else DelSpace := False;
		end;
		' ':
		begin
			Result := ' ' + Result;
		end;
		',':
		begin
				if (inpP > 0) then
				begin
					if UseWinFormat then
						Result := ThousandSeparator + Result
					else
						Result := ',' + Result;
				end
				else
				begin
					if DelSpace = False then Result := ' ' + Result;
				end;
		end;
		end;
	end;
	LExit:
end;

{function StrToI(s: string): SG;
begin
	Result := StrToI(s, 0);
{var
	i: SG;
	Minus: Boolean;
begin
	Result := 0;
	Minus := False;
	for i := 1 to Length(s) do
	begin
		case s[i] of
		'-': Minus := not Minus;
		'0'..'9':
		begin
			Result := Result * 10;
			if Minus then
				Result := Result - (Ord(s[i]) - Ord('0'))
			else
				Result := Result + (Ord(s[i]) - Ord('0'));
		end;
		'.': Break;
		end;
	end;
end;
}

const Sep = ' ';

function BToStr(const B: Integer): string;
label LExit;
begin
	if B < 1024 then //2^10 ($400)
	begin
		Result := NToS(B, 0) + Sep + 'B';
		goto LExit;
	end;
	if B < 10240 then
	begin
		Result := NToS((100 * B) div 1024, -2) + Sep + 'KB';
		goto LExit;
	end;
	if B < 102400 then
	begin
		Result := NToS((10 * B) div 1024, -1) + Sep + 'KB';
		goto LExit;
	end;
	if B < 1048576 then //2^20 ($100 000)
	begin
		Result := NToS(B div 1024, 0) + Sep + 'KB';
		goto LExit;
	end;
	if B < 10485760 then
	begin
		Result := NToS((100 * B) div 1048576, -2) + Sep + 'MB';
		goto LExit;
	end;
	if B < 104857600 then
	begin
		Result := NToS((10 * B) div 1048576, -1) + Sep + 'MB';
		goto LExit;
	end;
	if B < 1073741824 then //2^30 ($40 000 000)
	begin
		Result := NToS(B div 1048576, 0) + Sep + 'MB';
		goto LExit;
	end;
	//if B<10737418240 then
	Result := NToS((100 * (B div 128)) div (1073741824 div 128), -2) + Sep + 'GB';
	LExit:
//	if B < 0 then Result := '-' + Result;
end;

function BToStr(const B: Int64): string;
label LExit;
begin
	if B < 1024 then //2^10 ($400)
	begin
		Result := NToS(B, 0) + Sep + 'B';
		goto LExit;
	end;
	if B < 10240 then
	begin
		Result := NToS((100 * B) div 1024, -2) + Sep + 'KB'; //Kilo
		goto LExit;
	end;
	if B < 102400 then
	begin
		Result := NToS((10 * B) div 1024, -1) + Sep + 'KB';
		goto LExit;
	end;
	if B < 1048576 then //2^20 ($100 000)
	begin
		Result := NToS(B div 1024, 0) + Sep + 'KB';
		goto LExit;
	end;
	if B < 10485760 then
	begin
		Result := NToS((100 * B) div 1048576, -2) + Sep + 'MB'; //Mega
		goto LExit;
	end;
	if B < 104857600 then
	begin
		Result := NToS((10 * B) div 1048576, -1) + Sep + 'MB';
		goto LExit;
	end;
	if B < 1073741824 then //2^30 ($40 000 000)
	begin
		Result := NToS(B div 1048576, 0) + Sep + 'MB';
		goto LExit;
	end;
	if B < 10737418240 then
	begin
		Result := NToS((100 * B) div 1073741824, -2) + Sep + 'GB'; //Giga
		goto LExit;
	end;
	if B < 107374182400 then
	begin
		Result := NToS((10 * B) div 1073741824, -1) + Sep + 'GB';
		goto LExit;
	end;
	if B < 1099511627776 then //2^40 ($10 000 000 000)
	begin
		Result := NToS(B div 1073741824, 0) + Sep + 'GB';
		goto LExit;
	end;
	if B < 10995116277760 then
	begin
		Result := NToS((100 * B) div 1099511627776, -2) + Sep + 'TB'; //Tera
		goto LExit;
	end;
	if B < 109951162777600 then
	begin
		Result := NToS((10 * B) div 1099511627776, -1) + Sep + 'TB';
		goto LExit;
	end;
	if B < 1125899906842624 then //2^50 ($4 000 000 000 000)
	begin
		Result := NToS(B div 1099511627776, 0) + Sep + 'TB';
		goto LExit;
	end;
	if B < 11258999068426240 then
	begin;
		Result := NToS((100 * B) div 1125899906842624, -2) + Sep + 'PB'; //Peta
		goto LExit;
	end;
	if B < 112589990684262400 then
	begin
		Result := NToS((10 * B) div 1125899906842624, -1) + Sep + 'PB';
		goto LExit;
	end;
	if B < 1152921504606846976 then //2^60 ($1 000 000 000 000 000)
	begin
		Result := NToS(B div 1125899906842624, 0) + Sep + 'PB';
		goto LExit;
	end;
	//if B<11529215046068469760 then
	Result := NToS((100 * (B div 128)) div (1152921504606846976 div 128), -2) + Sep + 'EB'; //Exa
	LExit:
//	if B < 0 then Result := '-' + Result;
end;

function NodesToS(const Value: U8): string;
const
	K = 1000;
	M = 1000 * K;
	G = 1000 * M;
	T = 1000 * U8(G);
	E = 1000 * T;
begin
	if Value < 0 then
		Result := '> 9E18'
	else if Value < M then
		Result := NToS(Value)
	else if Value < G then
		Result := NToS(Value div K) + ' k'
	else if Value < T then
		Result := NToS(Value div M) + ' M'
	else if Value < E then
		Result := NToS(Value div G) + ' G'
	else
		Result := NToS(Value div T) + ' T';
end;

procedure MsToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
var
	DW: LongWord;
begin
	if Abs(T) >= 1000 * Int64(High(LongWord)) then
	begin
		GH := 999;
		GM := 59;
		GS := 59;
		GD := 99;
	end
	else
	begin
		DivModU8(Abs(T), 1000, DW, GD);
		DivModU8(DW, 60, DW, GS);
		DivModU8(DW, 60, GH, GM);
	end;
end;

function SToMs(const Str: string): SG;
var
	V: LongInt;
	Mul: LongInt;
	W: Byte;
	F: Byte;
	DP: Byte;
begin
	V := 0;
	if Length(Str) > 0 then
	begin
		F := 0;
		for W := Length(Str) - 1 downto 1 do
		begin
			if Str[W] = '.' then
			begin
				F := W;
				Break;
			end;
		end;
		Mul := 1000 div 10;
		if F > 0 then
		for W := F + 1 to Length(Str) do
		begin
			case Str[W] of
			'0'..'9':
			begin
				V := V + Mul * (Ord(Str[W]) - Ord('0'));
				Mul := Mul div 10;
			end;
			end;
		end;
		Mul := 1000;
		DP := 0;
		if F = 0 then F := Length(Str) + 1;
		for W := F - 1 downto 1 do
		begin
			case Str[W] of
			'0'..'9':
			begin
				V := V + Mul * (Ord(Str[W]) - Ord('0'));
				if V > 100000000 then
				begin
					Result := V;
					Exit;
				end;
				if Mul < 100000000 then Mul := Mul * 10;
			end;
			':':
			begin
				case DP of
				0: Mul := Minute;
				1: Mul := Hour;
				2: Mul := Hour;
				end;
				Inc(DP);
			end;
			end;
		end;
	end;
	Result := V;
end;

function StrToF4(Str: string): F4;
var
	E: Integer;
begin
	Val(Str, Result, E);
end;

function StrToF8(Str: string): F8;
var
	E: Integer;
begin
	Val(Str, Result, E);
end;

function StrToFA(Str: string): FA;
var
	E: Integer;
begin
	Val(Str, Result, E);
end;

function StrToSG(Str: string): SG;
var
	E: Integer;
begin
	Val(Str, Result, E);
end;

function SToTime(Str: string): TDateTime;
begin
	Result := SToMs(Str) / MSecsPerDay;
end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
	Result := False;
	DayTable := @MonthDays[IsLeapYear(Year)];
	if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
		(Day >= 1) and (Day <= DayTable^[Month]) then
	begin
		for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
		I := Year - 1;
		Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
		Result := True;
	end;
end;

function MsToStr(DT: Int64;
	Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string;
begin
	Result := MsToStr(DT, True, Display, Decimals, FixedWidth);
end;

function MsToStr(DT: Int64; const UseWinFormat: BG;
	Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string;
var
	h, m, s, d: LongWord;
	Day: SG;
	Res, Rem: U1;

	TimeSep, DecimalSep, ListSep: string[3];
begin
	if UseWinFormat then
	begin
		TimeSep := TimeSeparator;
		DecimalSep := DecimalSeparator;
		ListSep := ListSeparator;
	end
	else
	begin
		TimeSep := ':';
		DecimalSep := '.';
		ListSep := '; ';
	end;

{	case Abs(Decimals) of
	0:
	begin
		DT := 1000 * ((DT + 500) div 1000);
	end;
	1:
	begin
		DT := 100 * ((DT + 50) div 100);
	end;
	2:
	begin
		DT := 10 * ((DT + 5) div 10);
	end;
	end;}
	if DT < 0 then
	begin
		DT := -DT;
		Result := '-'
	end
	else
		Result := '';
	MsToHMSD(DT, h, m, s, d);
	if (DT >= Minute) and (Display = diSD) then
		Display := diMSD;

	if Display = diDHMSD then
	begin
		if DT >= MSecsPerDay then
		begin
			Day := DT div MSecsPerDay;
			Result := Result + IntToStr(Day) + ' day';
			if Day > 1 then Result := Result + 's';
			Result := Result + ListSep;
			h := h mod 24;
		end;
	end;

	case Display of
	diHHMSD, diHMSD, diDHMSD:
	begin
		if (h < 10) and (Display <> diHHMSD) then
		begin
			if (DT >= 0) and FixedWidth then Result := Result + ' ';
			Result := Result + Chr(h + Ord('0')) + TimeSep;
		end
		else if h < 100 then
		begin
			DivModU2(h, 10, Res, Rem);
			Result := Result + Chr(Res + Ord('0')) + Chr(Rem + Ord('0')) + TimeSep
		end
		else
			Result := Result + IntToStr(h) + TimeSep;
	end;
	diMSD:
	begin
		if h = 0 then
		begin
			if FixedWidth then Result := Result + '   '
		end
		else if h < 10 then
		begin
			if (DT >= 0) and FixedWidth then Result := Result + ' ';
			Result := Result + Chr(h + Ord('0')) + TimeSep
		end
		else if h < 100 then
		begin
			DivModU2(h, 10, Res, Rem);
			Result := Result + Chr(Res + Ord('0')) + Chr(Rem + Ord('0')) + TimeSep
		end
		else
			Result := Result + IntToStr(h) + TimeSep;
	end;
	end;

	if Display <> diSD then
		if m < 10 then
		begin
			if (h = 0) and (not (Display in [diHHMSD, diHMSD, diDHMSD])) then
			begin
				if FixedWidth then Result := Result + ' ';
				Result := Result + Chr(m + Ord('0')) + TimeSep
			end
			else
				Result := Result + '0' + Chr(m + Ord('0')) + TimeSep;
		end
		else
		begin
			DivModU2(m, 10, Res, Rem);
			Result := Result + Chr(Res + Ord('0')) + Chr(Rem + Ord('0')) + TimeSep;
		end;

	if Display = diSD then
	begin
		Result := Result + IntToStr(3600 * h + 60 * m + s);
	end
	else
		if s < 10 then
		begin
			Result := Result + '0' + Chr(s + Ord('0'));
		end
		else
		begin
			DivModU2(s, 10, Res, Rem);
			Result := Result + Chr(Res + Ord('0')) + Chr(Rem + Ord('0'));
		end;

	case Abs(Decimals) of
	1:
	begin
		d := d div 100;
		if (Decimals > 0) or (d <> 0) then
			Result := Result + DecimalSep + Chr(d + Ord('0'));
	end;
	2:
	begin
		d := d div 10;
		if (Decimals > 0) then
			Result := Result + DecimalSep + NToS(d, '00')
		else
		begin
			Result := Result + NToS(d, '.##');
			if FixedWidth = False then DelBESpace(Result);
		end;
	end;
	3:
	begin
		if (Decimals > 0) then
			Result := Result + DecimalSep + NToS(d, '000')
		else
		begin
			Result := Result + NToS(d, '.###');
			if FixedWidth = False then DelBESpace(Result);
		end;
	end;
	end;
end;

function DateToS(var Year, Month, Day: U2): string;
begin
	Result := NToS(Year, '0000') + '-' + NToS(Month, '00') + '-' + NToS(Day, '00');
end;

function DateToS(D: TDateTime): string;
var Year, Month, Day: U2;
begin
	if D = 0 then
		Result := ''
	else
	begin
		DecodeDate(D, Year, Month, Day);
		Result := DateToS(Year, Month, Day);
//		Result := NToS(Year, '0000') + '-' + NToS(Month, '00') + '-' + NToS(Day, '00');
	end;
end;

function TimeToS(T: TDateTime): string;
begin
	Result := MsToStr(Round(T * MSecsPerDay), False, diHHMSD, 0, False);
end;

function DateTimeToS(DT: TDateTime): string;
begin
	if DT = 0 then
		Result := ''
	else
	begin
// DateTimeToStr(s, 'YYYY-MM-DD HH:MM:SS', Now);
		Result := DateToS(Trunc(DT)) + ' ' + TimeToS(Frac(DT));
	end;
end;

{function DTToStr(DT: TDateTime): string;
begin
	if DT = 0 then
		Result := 'Never'
	else
		Result := DateToStr(DT) + ' ' + TimeToStr(DT);
end;}

function DTToStr(DT: TDateTime): string;
begin
	if DT = 0 then
		Result := 'Never'
	else
	begin
		try
			Result := DateTimeToStr(DT);
		except
			Result := 'Unknown';
		end;
	end;
end;

{function DateToStr6(D: TDate): string; // Disk
var
	Year, Month, Day: Word;
begin
	DecodeDate(D, Year, Month, Day);
	Result := DateToStr6(Year, Month, Day);
end;

function DateToStr6(Year, Month, Day: Word): string; // Disk
begin
	Result := NToS(Year, '00') + NToS(Month, '00') + NToS(Day, '00')
end;}

function PhoneToStr(Phone: U8): string;
begin
	if Phone = 0 then
	begin
		Result := '';
		Exit;
	end;
	if Phone div 1000000000 = 0 then
		Result := '+420-'
	else
		Result := '+' + NToS(Phone div 1000000000, '000') + '-';
	Result := Result + NToS(Phone mod 1000000000, '000000000');
end;

{
// Standard Escape Sequences:
\b       backspace
\f       formfeed
\n       new line
\r       carriage return
\t       horizontal tab
\'       single quote
\0       null


Sequence	Value	Char	What it does
\a	0x07	BEL	Audible bell
\b	0x08	BS	Backspace
\f	0x0C	FF	Formfeed
\n	0x0A	LF	Newline (linefeed)
\r	0x0D	CR	Carriage return
\t	0x09	HT	Tab (horizontal)
\v	0x0B	VT	Vertical tab
\\	0x5c	\	Backslash
\'	0x27	'	Single quote (apostrophe)
\"	0x22	"	Double quote
\?	0x3F	?	Question mark
\O		any	O=a string of up to three octal digits
\xH		any	H=a string of hex digits
\XH		any	H=a string of hex digits
}

function RemoveEscape(s: string): string;
var
	i, j: SG;
	x, v: U1;
	Special: BG;
begin
	Result := '';
	i := 1;
	Special := False;
	while i <= Length(s) do
	begin
		if s[i] = '\' then
		begin
			if Special then
			begin
				Result := Result + s[i];
				Special := False;
			end
			else
				Special := True;
		end
		else
			if Special then
			begin
				case s[i] of
				'a': Result := Result + CharBell;
				'b': Result := Result + CharBackspace;
				'e', 'E': Result := Result + #$1B;
				'f': Result := Result + CharFormfeed;
				'n': Result := Result + CharLF;
				'r': Result := Result + CharCR;
				't': Result := Result + CharHT;
{				'u', 'U':
				begin
				end;}
				'v': 	Result := Result + CharVT;
				'x':
				begin
					Inc(i);
					x := 0;
					while True do
					begin
						if i <= Length(s) then
							v := HexValue[s[i]]
						else
							v := 16;
						if (v < 16) then
						begin
							x := (x shl 4) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
					end;
				end;
				'''': Result := Result + '''';
				'"': Result := Result + '"';
				'?': Result := Result + '?';
				'0'..'7': //Result := Result + Char(Ord(s[i]) - Ord('0'));//CharNull;
				begin
					x := 0;
					j := 0;
					while True do
					begin
						if (i <= Length(s)) and (j < 3) then
							v := HexValue[s[i]]
						else
							v := 8;
						if (v < 8) then
						begin
							x := (x shl 3) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
						Inc(j);
					end;
				end;
				else
					Result := Result + s[i];
				end;
				Special := False;
			end
			else
				Result := Result + s[i];
		Inc(i);
	end;

end;

function AddEscape(s: string): string;
var i: SG;
begin
	Result := '';
	i := 1;
	while i <= Length(s) do
	begin
		case s[i] of
		'\': Result := Result + '\\';
		CharBell: Result := Result + '\a';
		CharBackspace: Result := Result + '\b';
		#$1B: Result := Result + '\e'; // 'E'
		CharFormfeed: Result := Result + '\f';
		CharLF: Result := Result + '\n';
		CharCR: Result := Result + '\r';
		CharHT: Result := Result + '\t';
		CharVT: Result := Result + '\v';
//		'''': Result := Result + '\''';
//		#0..#6: Result := Result + '\' + Char(Ord(s[i]) + Ord('0'));//CharNull;
		#$20..#$5B, #$5D..#$7F: // ASCII
			Result := Result + s[i];
		else
		begin
			NumericBase := 8;
			Result := Result + '\' + NToS(Ord(s[i]), '000');  // NumToStr(Ord(s[i]), 8);
			NumericBase := 10;
		end;
		end;
		Inc(i);
	end;
end;

procedure GetLocale;
{$ifndef LINUX}
var
	s: string;
	InLineIndex: SG;
{$endif}
begin
	{$ifndef LINUX}
	{$WARNINGS OFF}
	NativeSymbols := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SNATIVEDIGITS, '0123456789');

	DecimalSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SDECIMAL, '.');
	DigitsAfterDecimal := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_IDIGITS, '0'), 0);
	ThousandSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_STHOUSAND, ',');

	s := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SGROUPING, '0');
	InLineIndex := 1;
	ThousandGroup := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 3);
	FractionGroup := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 0);

	NegSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SNEGATIVESIGN, '-');
	PosSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SPOSITIVESIGN, '+');
{	PosSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGSIGNPOSN, '1');
	PosSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGSIGNPOSN, '1');}
	NegFormat := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGNUMBER, '1'), 1);
	LeadingZero := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_ILZERO, '2'), 2);
	ListSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SLIST, ';');

// Time Format
	TimeSeparator := GetLocaleStr(SysLocale.DefaultLCID{ GetThreadLocale}, LOCALE_STIME, ':');
{	s := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_ICENTURY, '0');
	InLineIndex := 1;
	ICentury := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 3);}
	ICentury := 30; // D???
	{$WARNINGS ON}
	{$ELSE}
	NativeSymbols := '0123456789';

	DecimalSeparator := '.';
	DigitsAfterDecimal := 0;
	ThousandSeparator := ',';

	ThousandGroup := 3;
	FractionGroup := 0;

	NegSymbol := '-';
	PosSymbol := '+';
	NegFormat := 1;
	LeadingZero := 2;
	ListSeparator := ';';

// Time Format
	TimeSeparator := ':';

	{$ENDIF}
end;

initialization
	GetLocale;
end.
