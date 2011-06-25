//* File:     Lib\uOutputFormat.pas
//* Created:  2004-03-07
//* Modified: 2007-11-25
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uOutputFormat;

interface

uses
	uTypes;

type
	TOutputFormat = (ofIO{Disk File Input/Output}, ofHTML{Disk HTML}, ofDisplay{Windows Locale});

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

{
NToS(S8, ...); <-> StrToValI(SG,UG), StrToValS8, U1(..., False, ...);

FToS(F10, ...) <-> StrToValExt StrToE(..., False);

2,102,454,545.45644;

IntToStr	StrToInt ; 2102454545;  Windows Registry, IE

}
{
	Decimals:
	-2	2.2	Maximum decimals
	+2	2.20	Fixed decimals
}
{
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

function NToS(const Num: Int64; const UseFormat: string): string; overload;
function NToS(const Num: Int64; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function NToS(const Num: Int64; const OutputFormat: TOutputFormat): string; overload;

function FToS(Num: Extended; const OutputFormat: TOutputFormat = ofDisplay): string;

function BToStr(const B: S4; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function BToStr(const B: S8; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function NodesToS(const Value: U8; const OutputFormat: TOutputFormat): string;

procedure msToHMSD(const T: Int64; out GH, GM, GS, GD: U4);
type
	TDisplay = (diDHMSD, diHHMSD, diHMSD, diMSD, diSD);

{Decimals
-3: 0:34.34
3: 0:34.340
}


function MsToStr(DT: Int64; Display: TDisplay;
	const Decimals: SG = 0; const FixedWidth: Boolean = False; const OutputFormat: TOutputFormat = ofDisplay): string;

function DateToS(const Year, Month, Day: U2; const OutputFormat: TOutputFormat): string; overload;
function DateToS(const D: TDateTime; const OutputFormat: TOutputFormat): string; overload;
function TimeToS(const T: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
function DateTimeToS(const DT: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
function PhoneToStr(const Phone: U8): string;

implementation

uses
	SysUtils, Windows, Math,
	uMath, uStrings;

procedure AddMinusStr(var Result: string; const OutputFormat: TOutputFormat);
begin
	if OutputFormat = ofDisplay then
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

// 454,545,455.456465; 0.045
function NToS(const Num: Int64; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
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

	case OutputFormat of
	ofDisplay:
	begin
		DecimalSep := DecimalSeparator;
		ThousandSep := ThousandSeparator;
		ThousandGr := ThousandGroup;
		FractionGr := FractionGroup;
	end;
	else {ofIO, ofHTML:}
	begin
		DecimalSep := '.';
		ThousandSep := ',';
		ThousandGr := 3;
		FractionGr := 3;
	end;
	end;
	if UseThousandSeparator = False then ThousandSep := '';

	if Num = 0 then
	begin
//		if OutputFormat = ofHTML then Nums := nbsp else Nums := ''
		Nums := '';
	end
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
				if OutputFormat = ofDisplay then
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
		AddMinusStr(Result, OutputFormat);
	end;
end;

function NToS(const Num: Int64; const OutputFormat: TOutputFormat): string; overload;
begin
	Result := NToS(Num, 0, OutputFormat);
end;

function FToS(Num: Extended; const OutputFormat: TOutputFormat = ofDisplay): string;
var
	D: SG;
	Nu, eps: Extended;
	LastThousandSeparator: string;
begin
{	if Num = NaN then
		Result := 'Not a number'
	else}
	begin
		LastThousandSeparator := ThousandSeparator;
		if UseThousandSeparator = False then ThousandSeparator := '';

		D := 0;
		Nu := Num;
		eps := 5.6e-18; // TODO : Round
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

		Result := NToS(Round(Nu), D, OutputFormat);
		ThousandSeparator := LastThousandSeparator;
	end;
end;

{
function Using(const Typ: string; const Num: Int64): string;
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
	if LastChar(UseFormat) = '~' then DelSpace := True else DelSpace := False;

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

function BToStr(const B: S4; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
	Assert(B >= 0);
	if B < KB then // 2^10 ($400)
	begin
		Result := NToS(B, 0, OutputFormat) + Sep + 'byte' + Plural(B);
	end
	else if B < 10 * KB then
	begin
		Result := NToS((100 * B + KB div 2) div KB, -2, OutputFormat) + Sep + 'KB';
	end
	else if B < 100 * KB then
	begin
		Result := NToS((10 * B + KB div 2) div KB, -1, OutputFormat) + Sep + 'KB';
	end
	else if B < MB then // 2^20 ($100 000)
	begin
		Result := NToS((B + KB div 2) div KB, 0, OutputFormat) + Sep + 'KB';
	end
	else if B < 10 * MB then
	begin
		Result := NToS((100 * B + MB div 2) div MB, -2, OutputFormat) + Sep + 'MB';
	end
	else if B < 100 * MB then
	begin
		Result := NToS((10 * B + MB div 2) div MB, -1, OutputFormat) + Sep + 'MB';
	end
	else if B < GB then // 2^30 ($40 000 000)
	begin
		Result := NToS((B + MB div 2) div MB, 0, OutputFormat) + Sep + 'MB';
	end
	else //if B < GB then
		Result := NToS((100 * (B div 128) + GB div 256) div (GB div 128), -2, OutputFormat) + Sep + 'GB';
//	if B < 0 then Result := '-' + Result;
end;

function BToStr(const B: S8; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
	Assert(B >= 0);
	if B < KB then // 2^10 ($400)
	begin
		Result := NToS(B, 0, OutputFormat) + Sep + 'byte' + Plural(B);
	end
	else if B < 10 * KB then
	begin
		Result := NToS((100 * B + KB div 2) div KB, -2, OutputFormat) + Sep + 'KB'; // Kilo
	end
	else if B < 100 * KB then
	begin
		Result := NToS((10 * B + KB div 2) div KB, -1, OutputFormat) + Sep + 'KB';
	end
	else if B < MB then // 2^20 ($100 000)
	begin
		Result := NToS((B + KB div 2) div KB, 0, OutputFormat) + Sep + 'KB';
	end
	else if B < 10 * MB then
	begin
		Result := NToS((100 * B + MB div 2) div MB, -2, OutputFormat) + Sep + 'MB'; // Mega
	end
	else if B < 100 * MB then
	begin
		Result := NToS((10 * B + MB div 2) div MB, -1, OutputFormat) + Sep + 'MB';
	end
	else if B < GB then // 2^30 ($40 000 000)
	begin
		Result := NToS((B + MB div 2) div MB, 0, OutputFormat) + Sep + 'MB';
	end
	else if B < 10737418240 then
	begin
		Result := NToS((100 * B + GB div 2) div GB, -2, OutputFormat) + Sep + 'GB'; // Giga
	end
	else if B < 107374182400 then
	begin
		Result := NToS((10 * B + GB div 2) div GB, -1, OutputFormat) + Sep + 'GB';
	end
	else if B < 1099511627776 then //2^40 ($10 000 000 000)
	begin
		Result := NToS((B + GB div 2) div GB, 0, OutputFormat) + Sep + 'GB';
	end
	else if B < 10995116277760 then
	begin
		Result := NToS((100 * B + 1099511627776 div 2) div 1099511627776, -2, OutputFormat) + Sep + 'TB'; // Tera
	end
	else if B < 109951162777600 then
	begin
		Result := NToS((10 * B + 1099511627776 div 2) div 1099511627776, -1, OutputFormat) + Sep + 'TB';
	end
	else if B < 1125899906842624 then //2^50 ($4 000 000 000 000)
	begin
		Result := NToS((B + 1099511627776 div 2) div 1099511627776, 0, OutputFormat) + Sep + 'TB';
	end
	else if B < 11258999068426240 then
	begin;
		Result := NToS((100 * B + 1125899906842624 div 2) div 1125899906842624, -2, OutputFormat) + Sep + 'PB'; // Peta
	end
	else if B < 112589990684262400 then
	begin
		Result := NToS((10 * B + 1125899906842624 div 2) div 1125899906842624, -1, OutputFormat) + Sep + 'PB';
	end
	else if B < 1152921504606846976 then //2^60 ($1 000 000 000 000 000)
	begin
		Result := NToS((B + 1125899906842624 div 2) div 1125899906842624, 0, OutputFormat) + Sep + 'PB';
	end
	else //if B < 11529215046068469760 then
		Result := NToS((100 * (B div 128) + 1152921504606846976 div 256) div (1152921504606846976 div 128), -2, OutputFormat) + Sep + 'EB'; // Exa
//	if B < 0 then Result := '-' + Result;
end;

function NodesToS(const Value: U8; const OutputFormat: TOutputFormat): string;
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
		Result := NToS(Value, OutputFormat)
	else if Value < G then
		Result := NToS((Value + K div 2) div K, OutputFormat) + ' k'
	else if Value < T then
		Result := NToS((Value + M div 2) div M, OutputFormat) + ' M'
	else if Value < E then
		Result := NToS((Value + G div 2) div G, OutputFormat) + ' G'
	else
		Result := NToS((Value + T div 2) div T, OutputFormat) + ' T';
end;

procedure MsToHMSD(const T: Int64; out GH, GM, GS, GD: U4);
var
	DW: U4;
begin
	if Abs(T) >= 1000 * U8(High(U4)) then
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


function TryEncodeDate(Year, Month, Day: U2; out Date: TDateTime): Boolean;
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

function MsToStr(DT: Int64; Display: TDisplay;
	const Decimals: SG = 0; const FixedWidth: Boolean = False; const OutputFormat: TOutputFormat = ofDisplay): string;
var
	h, m, s, d: U4;
	Day: SG;
	Res, Rem: U1;

	TimeSep, DecimalSep, ListSep: string[3];
begin
	case OutputFormat of
	ofDisplay:
	begin
		TimeSep := TimeSeparator;
		DecimalSep := DecimalSeparator;
		ListSep := ListSeparator;
	end
	else {ofIO, ofHTML:}
	begin
		TimeSep := ':';
		DecimalSep := '.';
		ListSep := '; ';
	end;
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
			Result := Result + IntToStr(Day) + ' day' + Plural(Day)+ ListSep;
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
			if FixedWidth then Result := Result + StringOfChar(CharSpace, 3);
		end
		else if h < 10 then
		begin
			if (DT >= 0) and FixedWidth then Result := Result + CharSpace;
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

function DateToS(const Year, Month, Day: U2; const OutputFormat: TOutputFormat): string; overload;
begin
	Assert(OutputFormat <> ofDisplay);
	Result := NToS(Year, '0000') + '-' + NToS(Month, '00') + '-' + NToS(Day, '00');
end;

function DateToS(const D: TDateTime; const OutputFormat: TOutputFormat): string; overload;
var Year, Month, Day: U2;
begin
	if D = 0 then
		Result := ''
	else
	begin
		case OutputFormat of
		ofDisplay:
		begin
			try
				Result := DateToStr(D);
			except
				Result := 'unknown';
			end;
		end
		else
		begin
			DecodeDate(D, Year, Month, Day);
			Result := DateToS(Year, Month, Day, OutputFormat);
		end;
		end;
	end;
end;

function TimeToS(const T: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
begin
	case OutputFormat of
	ofDisplay:
		try
			Result := TimeToStr(T);
		except
			Result := 'unknown';
		end;
	else
		Result := MsToStr(Round(T * MSecsPerDay), diHHMSD, Decimals, False, OutputFormat);
	end;
end;

function DateTimeToS(const DT: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
begin
	if DT = 0 then
		Result := ''
	else
	begin
		case OutputFormat of
		ofDisplay:
		begin
			try
				Result := DateTimeToStr(DT);
			except
				Result := 'unknown';
			end;
		end
		else
			// YYYY-MM-DD HH:MM:SS
			Result := DateToS(Trunc(DT), OutputFormat) + ' ' + TimeToS(Frac(DT), Decimals, OutputFormat);
		end;
	end;
end;

function PhoneToStr(const Phone: U8): string;
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
