//* File:     Lib\uAdd.pas
//* Created:  1998-01-01
//* Modified: 2005-03-07
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uAdd;

interface

uses
	SysUtils, Forms, ShlObj, ActiveX, ComObj, ComCtrls, Controls, DateUtils, Classes;

{ Mul EAX, 10
	asm
	mov eax, a
	lea eax, [eax*4+eax] // eax = eax * 5
	shl eax, 1
	end;}
{ Exchange eax <-> ebx
	xor eax, ebx
	xor ebx, eax
	xor eax, ebx}
{
		GetMem call SysGetMem
		FreeMem call SysFreeMem
		etc.

		New = GetMem
		AllocMem = GetMem + FillChar

		Initialize, Finalize: strings, dyn. arrays, interfaces

		FreeMem = Dispose
}

type
	SG = Integer; // LongInt for Delphi 6
	UG = Cardinal; // LongWord for Delphi 6
	S1 = ShortInt;
	U1 = Byte;
	S2 = SmallInt;
	U2 = Word;
	S4 = LongInt;
	U4 = LongWord;
	S8 = Int64;
	U8 = Int64; // Car64 for 64bit Delphi?

	PS1 = ^S1;
	PU1 = ^U1;
	PS2 = ^S2;
	PU2 = ^U2;
	PS4 = ^S4;
	PU4 = ^U4;
	PS8 = ^S8;
	PU8 = ^U8;

	TS2 = record
		case Integer of
		0: (
				B0: U1;
				B1: S1);
		1: (
				A: S2);
	end;
	TU2 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1);
		1: (
				A: U2);
	end;
	TS4 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: S1);
		1: (
				W0: U2;
				W1: S2);
		2: (
			A: S4);
	end;
	TU4 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1);
		1: (
				W0: U2;
				W1: U2);
		2: (
			A: U4);
	end;
	TS8 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1;
				B4: U1;
				B5: U1;
				B6: U1;
				B7: S1);
		1: (
				W0: U2;
				W1: U2;
				W2: U2;
				W3: S2);
		2: (
			D0: U4;
			D1: S4);
		3: (
			A: U8);
	end;
	TU8 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1;
				B4: U1;
				B5: U1;
				B6: U1;
				B7: U1);
		1: (
				W0: U2;
				W1: U2;
				W2: U2;
				W3: U2);
		2: (
			D0: U4;
			D1: U4);
		3: (
			A: U8);
	end;
{ S8 = record
		case Integer of
		0: (
			LowPart: U4;
			HighPart: S4);
		1: (
			QuadPart: Int64);
	end; overload;
	U8 = record
		case Integer of
		0: (
			LowPart: U4;
			HighPart: U4);
		1: (
			QuadPart: Int64);
	end;}

	FG = Real; // Double for Delphi 6
	F4 = Single;
//	F6 = Real48;
	F8 = Double;
	FA = Extended;

{ CG = Char; // AnsiChar for Delphi 6
	C1 = AnsiChar;
	C2 = WideChar;

	TG = string; // AnsiString for Delphi 6
	TA1 = ShortString;
	T1 = AnsiString;
	T2 = WideString;}

	BG = Boolean; // ByteBool for Delphi 6
	B1 = ByteBool;
	B2 = WordBool;
	B4 = LongBool;

//	TArrayU1 = array[0..1024 * 1024 * 1024 - 1] of Byte;
	TArrayS1 = array[0..1024 * 1024 * 1024 - 1] of S1;
	PArrayS1 = ^TArrayS1;
	TArrayU1 = array[0..1024 * 1024 * 1024 - 1] of U1;
	PArrayU1 = ^TArrayU1;
	TArrayS2 = array[0..1024 * 1024 * 1024 - 2] of S2;
	PArrayS2 = ^TArrayS2;
	TArrayU2 = array[0..1024 * 1024 * 1024 - 2] of U2;
	PArrayU2 = ^TArrayU2;
	TArrayS4 = array[0..512 * 1024 * 1024 - 2] of S4;
	PArrayS4 = ^TArrayS4;
	TArrayU4 = array[0..512 * 1024 * 1024 - 2] of U4;
	PArrayU4 = ^TArrayU4;
	TArrayS8 = array[0..256 * 1024 * 1024 - 2] of S8;
	PArrayS8 = ^TArrayS8;
	TArraySG = array[0..256 * 1024 * 1024 - 2] of SG;
	PArraySG = ^TArraySG;

	TArrayF8 = array[0..256 * 1024 * 1024 - 2] of F8;
	PArrayF8 = ^TArrayF8;
	TArrayFA = array[0..128 * 1024 * 1024 - 2] of FA;
	PArrayFA = ^TArrayFA;

	TArrayChar = array[0..1024 * 1024 * 1024 - 1] of AnsiChar;
	PArrayChar = ^TArrayChar;

	TIndex = SG;

//	string = string;

// Graphics
const
	MaxSpectrum = 1529;
type
{	PRColor = ^TRColor;
	TRColor = packed record // RGBA=4
		case Integer of
		0: (L: -$7FFFFFFF - 1..$7FFFFFFF);
		1: (R, G, B, A: Byte);
		2: (RG, BA: Word);
	end;}

	PRGB = ^TRGB;
	TRGB = packed record // RGB=3
		case Integer of
		0: (R, G, B: U1);
		1: (I: array[0..2] of U1);
		2: (RG: U2);
	end;
	PRGBA = ^TRGBA;
	TRGBA = packed record // RGBA=4
		case Integer of
		0: (R, G, B, A: U1);
		1: (L: S4);
		2: (I: array[0..3] of U1);
		3: (RG, BA: U2);
	end;
	// 4 byte for CPU
//	PRColor = PRGBA;
	TRColor = TRGBA;

	{	THSVColor = packed record // 4
		case Integer of
		0:
		(
		H: -1..MaxSpectrum; // 2
		S: 0..239; // 1
		V: 0..255; // 1
		);
		1:
		(
		Hue: 0..MaxSpectrum; // 2
		Saturation: 0..239; // 1
		Value: 0..255; // 1
		);
	end;}
	THLSColor = packed record // 4
{		case Integer of
		0:
		(}
		H: -1..MaxSpectrum; // 2
		L: 0..255; // 1
		S: 0..255; // 1
(*		);
		1: (
		Hue: -1..MaxSpectrum; // 2
		Lightness{Lum(inary, inous)}: 0..255; // 1
		Saturation{Sat(iety)}: 0..255; // 1
		);*)
	end;

const
	MinInt = Low(Integer);
	MinInt8 = Low(Int64);
	MaxInt8 = Int64($7FFFFFFFFFFFFFFF);

// Mathematics
const
	AngleCount = 16384; // 2*pi 256; 65536 // 2^x 1..
	SinDiv = 32768; // 1.0 65536; // 1..128..1024*1024
type
	TAngle = SG;
var
	Sins: array[0..AngleCount - 1] of TAngle;

function RGBToHLS(C: TRColor): THLSColor;
function HLSToRGB(C: THLSColor): TRColor;
//function RGBtoHSV(C: TRColor): THSVColor;
//function HSVtoRGB(C: THSVColor): TRColor;
function RColor(R, G, B: U1): TRColor;

function Sgn(const I: S1): SG; overload;
function Sgn(const I: S2): SG; overload;
function Sgn(const I: S4): SG; overload;
function Sgn(const I: S8): SG; overload;
function Sgn(const I: F4): SG; overload;
function Sgn(const I: F8): SG; overload;
function Sgn(const I: FA): SG; overload;
function SgnMul(const Signum, Num: SG): SG;
//function Min(const A, B: UG): UG;

procedure DivModU2(const Dividend: U2; const Divisor: U1;
	out Res, Remainder: U1);
procedure DivModU4(const Dividend: U4; const Divisor: U2;
	out Res, Remainder: U2);
procedure DivModS4(const Dividend: S4; const Divisor: S2;
	out Res, Remainder: S2);
procedure DivModU8(const Dividend: U8; const Divisor: U4;
	out Res, Remainder: U4); pascal;
procedure DivModS8(const Dividend: S8; const Divisor: S4;
	out Res, Remainder: S4); pascal;
function UnsignedMod(const Dividend: Int64; const Divisor: Integer): Integer;
function ModE(x, y: Extended): Extended;

function FastSqrt(A: SG): SG;
function LinearMax(Clock, Maximum: LongWord): LongWord;

function RoundSG(Value: FA): SG;
function RoundS8(Value: FA): S8;
function RangeS8(Value: FA): BG;
function RoundDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function RoundDivU8(const Dividend: U8; const Divisor: U8): S8; //overload;
function RoundDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;
function MaxDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function MaxDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;

function Range(const Min, Cur, Max: Integer): Integer; overload;
function Range(const Min, Cur, Max, Def: Integer): Integer; overload;
function Range(const Min, Cur, Max: Cardinal): Cardinal; overload;

procedure Exchange(var A, B: B1); register; overload;
procedure Exchange(var A, B: B4); register; overload;
procedure Exchange(var A, B: U1); register; overload;
procedure Exchange(var A, B: S1); register; overload;
procedure Exchange(var A, B: U2); register; overload;
procedure Exchange(var A, B: S2); register; overload;
procedure Exchange(var A, B: U4); register; overload;
procedure Exchange(var A, B: S4); register; overload;
procedure Exchange(var A, B: S8); register; overload;
procedure Exchange(var A, B: F8); register; overload;
procedure Exchange(var A, B: FA); register; overload;
procedure Exchange(var A, B: Pointer); register; overload;
procedure Exchange(var P0, P1; Count: Cardinal); register; overload;

function Arg(X, Y: Extended): Extended; overload;

function Random2(Range: SG): SG;
function RandomU4: U4;
function RandomM: U4;

procedure CheckBool(var Bool: ByteBool); overload;
procedure CheckBool(var Bool: WordBool); overload;
procedure CheckBool(var Bool: LongBool); overload;

procedure Order(var I1, I2: Integer); overload;
procedure Order(var I1, I2: Cardinal); overload;

function CalcShr(N: U4): S1;

(*function AllocByB(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;
function AllocByEx(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;*)
function AllocByExp(const OldSize: SG; var NewSize: SG): Boolean;

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

function KeyToStr(Key: Word): string;

procedure msToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
type
	TDisplay = (diDHMSD, diHHMSD, diHMSD, diMSD, diSD);

{Decimals
-3: 0:34.34
3: 0:34.340
}

//function SToMs(const Str: string): SG; // MsToStr<-

function StrToFA(Str: string): FA;
function StrToSG(Str: string): SG;
function SToTime(Str: string): TTime;
function SToDate(Str: string): TDate;
function SToDateTime(Str: string): TDateTime;


function MsToStr(const DT: Int64;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string; overload;
function MsToStr(const DT: Int64; const UseWinFormat: BG;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string; overload;

function DateToS(var Year, Month, Day: U2): string; overload;
function DateToS(D: TDate): string; overload;
function TimeToS(T: TTime): string;
function DateTimeToS(DT: TDateTime): string;
function DTToStr(DT: TDateTime): string; // UseWinFormat = True
function PhoneToStr(Phone: U8): string;

(*
function StrToValC(S: string;
	const MinVal, DefVal, MaxVal, Denominator: Cardinal): Cardinal;
function StrToValE(S: string;
	const MinVal, DefVal, MaxVal: Extended): Extended;
*)

// System
{$ifopt d+}
procedure Nop;
{$endif}
//procedure GetMem0(var P: Pointer; Size: Cardinal);
procedure ReadMem(P: Pointer; Size: Cardinal);
procedure FillU4(var Desc; Count: Cardinal; Value: U4);
procedure FillOrderU4(var Desc; Size: Cardinal); register;
procedure Swap02(var Desc; Count: Cardinal; Step: S4);
function SelectDirectory(var Dir: string): BG;
function DriveTypeToStr(const DriveType: Integer): string;
function ProcessPriority(const Prior: Byte): Integer;
function ThreadPriority(const Prior: Byte): Integer;

function GetCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const Index, Count: Integer): string;

function ComponentName(Name: string): string;
function MenuNameToFileName(Name: string): string;
function ButtonNameToFileName(Name: string; const Space: Boolean): string;

//procedure CorrectFormPos(Form: TForm);
//procedure SetListViewItems(ListView: TListView; NewSize: SG);
procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: Word;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);

function DropFiles(hDrop: THandle): TStrings;

procedure RAToXY(Len: SG; Angle: TAngle; out X, Y: SG);

implementation

uses
	Windows, Math, Dialogs, ShellAPI,
	uError, uStrings, uInput, uFiles, uParser;

{
Nìco k pøevodu RGB -> YUV, RGB -> YCbCr
Oba pøevody (RGB -> YUV i RGB -> YCbCr) jsou jednoduše vyjádøitelné maticemi:
|Y|   |0.299  0.587  0.114  | |R|
|U| = |-0.141  -0.289 0.437 | |G|
|V|   |0.615 -0.515 -0.1    | |B|


|Y |   |0.299  0.587  0.114   | |R|
|Cb| = |-0.1687  -0.3313 -0.5 | |G|
|Cr|   |0.5 -0.4187 -0.0813   | |B|

Zpìtný pøevod se provádí pomocí inverzní matice.


Model HSV vykazuje nìkteré nedostatky, které sice nejsou zásadního charakteru,
nicménì mohou ztìžovat práci s definováním barvy v prostoru HSV.
Jedním z nedostatkù je jehlanovitý tvar, který zpùsobuje,
že ve øezu se musí bod o konstantní hodnotì s pohybovat pøi zmìnì h po dráze ve tvaru šestiúhelníku,
nikoliv po kružnici, jak by bylo pøirozené.
Dalším záporným jevem je nesymetrie modelu z hlediska pøechodù ve stupních šedi od èerné k bílé.
Tyto nedostatky odstraòuje model HLS zavedený firmou Tektronix
}

function RGBToHLS(C: TRColor): THLSColor;
var
	MaxC, MinC, delta, H: SG;
begin
	Result.H := -1;
	Result.L := 0;
	Result.S := 0;

	MaxC := max(max(C.R, C.G), C.B);
	MinC := min(min(C.R, C.G), C.B);

	Result.L := (maxC + minC) div 2;

	delta := maxC - minC;
	if delta = 0 then
	begin
		Result.S := 0;
		Result.H := -1;
	end
	else
	begin
		if (Result.L < 128) then
			Result.S := RoundDiv(255 * delta, (maxC + minC))
		else
			Result.S := RoundDiv(255 * delta, (2 * 255 - maxC - minC));

		H := 0;
		if (C.R = maxC) then
			H := ((MaxSpectrum + 1) div 6) * (C.G - C.B) div delta
		else if (C.G = maxC) then
			H := ((MaxSpectrum + 1) div 6) * 2 + RoundDiv(((MaxSpectrum + 1) div 6) * (C.B - C.R), delta)
		else if (C.B = maxC) then
			H := ((MaxSpectrum + 1) div 6) * 4 + RoundDiv(((MaxSpectrum + 1) div 6) * (C.R - C.G), delta);
		if (H < 0) then Inc(H, (MaxSpectrum + 1));
		Result.H := H;
	end;
end;

function HLSToRGB(C: THLSColor): TRColor;

	function HLSRGBValue(n1, n2, hue: SG): U1;
	begin
		if(hue >= (MaxSpectrum + 1)) then
			Dec(hue, (MaxSpectrum + 1))
		else if (hue < 0) then
			Inc(hue, (MaxSpectrum + 1));
		if (hue < ((MaxSpectrum + 1) div 6)) then
			Result := RoundDiv(n1+(n2-n1)*hue div ((MaxSpectrum + 1) div 6), 255)
		else if (hue < ((MaxSpectrum + 1) div 2)) then
			Result := RoundDiv(n2, 255)
		else if (hue < (2 * (MaxSpectrum + 1) div 3)) then    //  n1+(n2-n1)*(240-hue)/60;
			Result := RoundDiv(n1+(n2-n1)*(2 * (MaxSpectrum + 1) div 3-hue) div ((MaxSpectrum + 1) div 6), 255)
		else
			Result := RoundDiv(n1, 255);
	end;

var m2, m1: SG;
begin
	Result.L := 0;

	if (C.L < 128) then
		m2 := C.L * (255 + C.S)
	else
		m2 := 255 * (C.L + C.S) - C.L * C.S;
	m1 := 2 * 255 * C.L - m2;
	if (C.S = 0) then
	begin
		Result.R := C.L;
		Result.G := C.L;
		Result.B := C.L;
	end
	else
	begin
		Result.R := HLSRGBValue(m1, m2, C.H + ((MaxSpectrum + 1) div 3));
		Result.G := HLSRGBValue(m1, m2, C.H);
		Result.B := HLSRGBValue(m1, m2, C.H - ((MaxSpectrum + 1) div 3));
	end;
end;

(*
function RGBtoHSV(C: TRColor): THSVColor;
var
	MaxC, MinC, delta, H: SG;
begin
	maxC := Math.max(Math.max(C.r,C.g),C.b);
	minC := Math.min(Math.min(C.r,C.g),C.b);

	Result.v := maxC;
	Result.h := 0;
	h := 0;

	if (maxC <> 0) then
		Result.s := (maxC - minC) div maxC
	else
		Result.s := 0;

	if(Result.s = 0) then
		Result.h := -1
	else
	begin
		delta := maxC - minC;
		if(C.r = maxC) then h := 60*(C.g-C.b) div delta
		else if(C.g = maxC) then h := 60*(2+(C.b-C.r)) div delta
		else if(C.b = maxC) then h := 60*(4+(C.r-C.g)) div delta;
		if(h<0) then Inc(h, 360);
		Result.H := H;
	end;
end;*)

(*
function HSVtoRGB(C: THSVColor): TRColor;
var i, f, p, q, t: SG;
begin
	Result.L := 0;
	if(C.s = 0) then
	begin
		if(C.h = -1) then
		begin
			Result.r := C.v;
			Result.g := C.v;
			Result.b := C.v;
		end
		else
		begin
{							rIndex.setText("xxx");
			gIndex.setText("xxx");
			bIndex.setText("xxx"); D???}
		end;
	end
	else
	begin
		if(C.h = 360) then C.h := 0;

		C.h:=C.h div 60; // D???
//            i := (int)Math.floor((double)h); D???

		f := C.h - i;
		p := C.v*(1-C.s);
		q := C.v*(1-(C.s*f));
		t := C.v*(1-(C.s*(1-f)));

		case i of
		0:
		begin
			Result.r := C.v;
			Result.g := t;
			Result.b := p;
		end;
		1:
		begin
			Result.r := q;
			Result.g := C.v;
			Result.b := p;
		end;
		2:
		begin
			Result.r := p;
			Result.g := C.v;
			Result.b := t;
		end;
		3:
		begin
			Result.r := p;
			Result.g := q;
			Result.b := C.v;
		end;
		4:
		begin
			Result.r := t;
			Result.g := p;
			Result.b := C.v;
		end;
		5:
		begin
			Result.r := C.v;
			Result.g := p;
			Result.b := q;
		end;
		end;
	end;
end;*)


function RColor(R, G, B: U1): TRColor;
begin
	Result.A := 0;
	Result.R := R;
	Result.G := G;
	Result.B := B;
end;

function Sgn(const I: S1): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: S2): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: S4): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: S8): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: F4): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: F8): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: FA): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function SgnMul(const Signum, Num: Integer): Integer;
begin
	if Signum = 0 then
		Result := 0
	else if Signum > 0 then
		Result := Num
	else
		Result := -Num;
end;

{
function Min(const A, B: UG): UG;
begin
	if A < B then
		Result := A
	else
		Result := B;
end;}

procedure DivModU2(const Dividend: U2; const Divisor: U1;
	out Res, Remainder: U1); register;
asm
	div dl // al := ax div dl; ah := ax mod dl
	mov edx, Remainder
	mov [ecx], al
	mov [edx], ah
end;

procedure DivModU4(const Dividend: U4; const Divisor: U2;
	out Res, Remainder: U2); register;
asm
	push ebx
	mov bx, dx
	mov edx, eax
	shr edx, 16
	div bx // ax := dx&ax div bx; dx := dx&ax mod bx
	mov ebx, Remainder
	mov [ecx], ax
	mov [ebx], dx
	pop ebx
end;

procedure DivModS4(const Dividend: S4; const Divisor: S2;
	out Res, Remainder: S2); register;
asm
	push ebx
	mov ebx, edx
	mov edx, eax
	shr edx, 16
	idiv bx // ax := dx&ax div bx; dx := dx&ax mod bx
	mov ebx, Remainder
	mov [ecx], ax
	mov [ebx], dx
	pop ebx
end;

procedure DivModU8(const Dividend: U8; const Divisor: U4;
	out Res, Remainder: U4); pascal;
asm
	push ebx
	mov edx, dword ptr [Dividend + 4]// Divident-hi
	mov eax, dword ptr Dividend // Divident-lo
	mov ebx, Divisor
	div ebx // eax:=edx&eax div ebx; edx:=edx&eax mod ebx
	mov ebx, Res
	mov [ebx], eax
	mov ebx, Remainder
	mov [ebx], edx
	pop ebx
end;

procedure DivModS8(const Dividend: S8; const Divisor: S4;
	out Res, Remainder: S4); pascal;
asm
	pushad
	mov edx, dword ptr [Dividend + 4]// Divident-hi
	mov eax, dword ptr Dividend // Divident-lo
	mov ebx, Divisor
	idiv ebx // eax:=edx&eax div ebx; edx:=edx&eax mod ebx
	mov edi, Res
	mov [edi], eax
	mov edi, Remainder
	mov [edi], edx
	popad
end;

function UnsignedMod(const Dividend: Int64; const Divisor: Integer): Integer;
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	if Dividend >= 0 then
		Result := Dividend mod Divisor
	else
	begin
		Result := Dividend + Divisor * (Abs(Dividend - Divisor + 1) div Divisor);
	end;
end;

function ModE(x, y: Extended): Extended;
begin
	Result := x - {Trunc}Floor(x / y) * y;
end;

function FastSqrt(A: SG): SG;
const
	Base = 16;
	BaseS = 4;
	Base2 = Base * Base; // 256
	BaseS2 = 8;
var
	AX, B, k, Pow: SG;
begin
	B := 0;
	AX := 0;
	Pow := 24;
	while Pow >= 0 do
	begin
		B := B shl BaseS;
		k := B shl 1 + 1;
		AX := AX shl BaseS2 + (A shr Pow) and (Base2 - 1);
		while True do
		begin
			if AX < k then Break;
			Dec(AX, k);
			Inc(k, 2);
			Inc(B);
		end;
		Pow := Pow - BaseS2;
	end;
	Result := B;
end;

function LinearMax(Clock, Maximum: LongWord): LongWord;
begin
	Result := Clock mod (2 * Maximum);
	if Result > Maximum then Result := 2 * Maximum - Result;
end;

function RoundSG(Value: FA): SG;
begin
	if Value > MaxInt then
		Result := MaxInt
	else if Value < MinInt then
		Result := MinInt
	else
		Result := Round(Value);
end;

function RoundS8(Value: FA): S8;
begin
	if Value > High(Result) then
		Result := High(Result)
	else if Value < Low(Result) then
		Result := Low(Result)
	else
		Result := Round(Value);
end;

function RangeS8(Value: FA): BG;
begin
	if Value > High(S8) then
		Result := False
	else if Value < Low(S8) then
		Result := False
	else
		Result := True;
end;

function RoundDiv(const Dividend: SG; const Divisor: SG): SG;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	if Dividend < 0 then
		Result := (Dividend - (Divisor div 2)) div Divisor
	else
		Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function RoundDivU8(const Dividend: U8; const Divisor: U8): U8;
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function RoundDivS8(const Dividend: S8; const Divisor: S8): S8;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	if Dividend < 0 then
		Result := (Dividend - (Divisor div 2)) div Divisor
	else
		Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function MaxDiv(const Dividend: SG; const Divisor: SG): SG;
// 0 div 4 is 0
// 1 div 4 is 1
// 2 div 4 is 1
// 3 div 4 is 1
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	if Dividend < 0 then
		Result := (Dividend - Divisor + 1) div Divisor
	else
		Result := (Dividend + Divisor - 1) div Divisor;
end;

function MaxDivS8(const Dividend: S8; const Divisor: S8): S8;
// 0 div 4 is 0
// 1 div 4 is 1
// 2 div 4 is 1
// 3 div 4 is 1
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	if Dividend < 0 then
		Result := (Dividend - Divisor + 1) div Divisor
	else
		Result := (Dividend + Divisor - 1) div Divisor;
end;

function Range(const Min, Cur, Max: Integer): Integer;
begin
	Result := Cur;
	if Cur < Min then
		Result := Min
	else if Cur > Max then
		Result := Max;
end;

function Range(const Min, Cur, Max, Def: Integer): Integer;
begin
	Result := Cur;
	if Cur < Min then
		Result := Def
	else if Cur > Max then
		Result := Def;
end;

function Range(const Min, Cur, Max: Cardinal): Cardinal;
begin
	Result := Cur;
	if Cur < Min then
		Result := Min
	else if Cur > Max then
		Result := Max;
end;

function Random2(Range: SG): SG;
begin
	Result := Random(2 * Range + 1) - Range;
end;

var
	InitJ: SG = 24 - 1;
	InitK: SG = 55 - 1;
	InitX: array[0..54] of U4 = (
		1410651636, 3012776752, 3497475623, 2892145026, 1571949714,
		3253082284, 3489895018, 387949491, 2597396737, 1981903553,
		3160251843, 129444464, 1851443344, 4156445905, 224604922,
		1455067070, 3953493484, 1460937157, 2528362617, 317430674,
		3229354360, 117491133, 832845075, 1961600170, 1321557429,
		747750121, 545747446, 810476036, 503334515, 4088144633,
		2824216555, 3738252341, 3493754131, 3672533954, 29494241,
		1180928407, 4213624418, 33062851, 3221315737, 1145213552,
		2957984897, 4078668503, 2262661702, 65478801, 2527208841,
		1960622036, 315685891, 1196037864, 804614524, 1421733266,
		2017105031, 3882325900, 810735053, 384606609, 2393861397 );

function RandomU4: U4;
begin
	Result := U4(Random(65536)) + U4(Random(65536)) shl 16;
end;

function RandomM: U4;
(*
	random numbers from Mathematica 2.0.
	SeedRandom = 1;
	Table[Random[Integer, {0, 2^32 - 1}]
	*)
begin
	Result := (InitX[InitJ] + InitX[InitK]);
	InitX[InitJ] := Result;
	if InitJ = 0 then
		InitJ := High(InitX)
	else
		Dec(InitJ);
	if InitK = 0 then
		InitK := High(InitX)
	else
		Dec(InitK);
end;


function Arg(X, Y: Extended): Extended; // <0..2pi)
begin
{	if Abs(X) > Abs(Y) then
	begin
		Result := ArcTan(Y / X);
	end
	else
	begin
		Result := pi / 2 - ArcTan(X / Y);
	end;}
	Result := ArcTan2(Y, X);
	if Result < 0 then Result := 2 * pi - Abs(Result);
end;

procedure CheckBool(var Bool: ByteBool);
begin
	Bool := ByteBool(Byte(Bool) and 1);
end;

procedure CheckBool(var Bool: WordBool);
begin
	Bool := WordBool(Word(Bool) and 1);
end;

procedure CheckBool(var Bool: LongBool);
begin
	Bool := LongBool(LongWord(Bool) and 1);
end;

procedure Order(var I1, I2: Integer);
var I: Integer;
begin
	if I1 > I2 then
	begin
		I := I1;
		I1 := I2;
		I2 := I;
	end;
end;

procedure Order(var I1, I2: Cardinal);
var I: Cardinal;
begin
	if I1 > I2 then
	begin
		I := I1;
		I1 := I2;
		I2 := I;
	end;
end;

function CalcShr(N: U4): S1;
{
	0: -1
	1: 0
	2: 1
	4: 2
	8: 3
	16
	32
	64
	16384: 14
	32768: 15
	65536: 16

	0: -1
	1: 0
	2: 1
	3..4: 2
	5..8: 3

	1 shl -1 = 0
	1 shl 0 = 1
	1 shl 1 = 2
	1 shl 2 = 4
	1 shl 3 = 8

}
var M: U4;
begin
	if N = 0 then
	begin
		Result := -1;
	end
	else
	begin
		Result := 0;
		M := 1;
		while N > M do
		begin
			Inc(Result);
			M := M shl 1;
		end;
	end;
end;
(*
function AllocByB(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;
{
	OldSize = <0, 2^31)
	NewSize = <0, 2^31)
	BlockSize = 2^n, <2, 2^30>
}
var Sh: SG;
begin
{	Result := True;
	Exit;}
	Sh := CalcShr(BlockSize);
	if (1 shl Sh) <> BlockSize then
	begin
		{$ifopt d+}
		ErrorMessage('Bad AllocBy block size' + LineSep + NToS(BlockSize) + ' bytes');
		{$endif}
		if NewSize > OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) mod (BlockSize + 0);
			Result := OldSize <> NewSize;
		end
		else if NewSize + BlockSize + BlockSize div 2 < OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) mod (BlockSize + 0);
			Result := OldSize <> NewSize;
		end
		else
		begin
			NewSize := OldSize;
			Result := False;
		end;
	end
	else
	begin
		if NewSize > OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1);
			Result := OldSize <> NewSize;
		end
		else if NewSize + BlockSize + BlockSize div 2 < OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1);
			Result := OldSize <> NewSize;
		end
		else
		begin
			NewSize := OldSize;
			Result := False;
		end;
	end;
end;

function AllocByEx(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;
{
	OldSize = <0, 2^31)
	NewSize = <0, 2^31)
	BlockSize = 2^n, <2, 2^30>
}
var Sh: SG;
begin
{	Result := True;
	Exit;}
	Sh := CalcShr(BlockSize);
	if (1 shl Sh) <> BlockSize then
	begin
		{$ifopt d+}
		ErrorMessage('Bad AllocBy block size' + LineSep + NToS(BlockSize) + ' bytes');
		{$endif}
//		BlockSize := 1 shl CalcShr(DefMemBuffer div BlockSize);
		BlockSize := DefMemBuffer;
		if NewSize > OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1);
			Result := OldSize <> NewSize;
		end
		else if NewSize + BlockSize + BlockSize div 2 < OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1);
			Result := OldSize <> NewSize;
		end
		else
		begin
			NewSize := OldSize;
			Result := False;
		end;
	end
	else
	begin
		BlockSize := DefMemBuffer shr Sh;
		if NewSize > OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1);
			Result := OldSize <> NewSize;
		end
		else if NewSize + BlockSize + BlockSize div 2 < OldSize then
		begin
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1);
			Result := OldSize <> NewSize;
		end
		else
		begin
			NewSize := OldSize;
			Result := False;
		end;
	end;
end;*)

function AllocByExp(const OldSize: SG; var NewSize: SG): Boolean;
{
	0 <= OldSize < 2^31
	0 <= NewSize < 2^31
}
begin
  Result := False;
	if NewSize > OldSize then
	begin
		{$ifopt d+}
		if OldSize > 0 then
		if OldSize <> 1 shl CalcShr(OldSize) then
		begin
			ErrorMessage('Bad AllocBy block size' + LineSep + NToS(OldSize) + ' bytes');
		end;
		{$endif}
		NewSize := Max(1 shl CalcShr(NewSize), 0{Minimum items});
		Result := True;
	end;
end;

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
	if (Base < 2) or (Base > MaxNumericBase) then
	begin
		{$ifopt d+}
		IE(5465);
		{$endif}
		Exit;
	end;
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
			IE(34343){$endif};
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
		if Abs(Nu) < MaxInt8 div NumericBase then
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
		IE(434333);
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

function KeyToStr(Key: Word): string;
begin
	case Key and $ff of
	0: Result := '';
	VK_LBUTTON: Result := 'L. Button';
	VK_RBUTTON: Result := 'R. Button';
	VK_CANCEL: Result := 'Cancel';
	VK_MBUTTON: Result := 'M.Button';
	VK_BACK: Result := 'Back';
	VK_TAB: Result := 'Tab';
	VK_CLEAR: Result := 'Clear';
	VK_RETURN: Result := 'Return';
	VK_SHIFT: Result := 'Shift'; // Old Win
	VK_CONTROL: Result := 'Ctrl'; // Old Win
	VK_MENU: Result := 'Alt'; // Old Win
	VK_PAUSE: Result := 'Pause';
	VK_CAPITAL: Result := 'Caps Lock';
	VK_KANA: Result := 'Kana';
//	VK_HANGUL: Result := 'Hangul';
	VK_JUNJA: Result := 'Junja';
	VK_FINAL: Result := 'Final';
	VK_HANJA: Result := 'Hanja';
//	VK_KANJI: Result := 'Kanji';
	VK_CONVERT: Result := 'Convert';
	VK_NONCONVERT: Result := 'Nonconvert';
	VK_ACCEPT: Result := 'Accept';
	VK_MODECHANGE: Result := 'Mode Change';
	VK_ESCAPE: Result := 'ESC';
	VK_SPACE: Result := 'Space';
	VK_PRIOR: Result := 'Page Up';
	VK_NEXT: Result := 'Page Down';
	VK_END: Result := 'End';
	VK_HOME: Result := 'Home';
	VK_LEFT: Result := 'Left';
	VK_UP: Result := 'Up';
	VK_RIGHT: Result := 'Right';
	VK_DOWN: Result := 'Down';
	VK_SELECT: Result := 'Select';
	VK_PRINT: Result := 'Print';
	VK_EXECUTE: Result := 'Execute';
	VK_SNAPSHOT: Result := 'Print Screen';
	VK_INSERT: Result := 'Insert';
	VK_DELETE: Result := 'Delete';
	VK_HELP: Result := 'Help';
{ VK_0 thru VK_9 are the same as ASCII '0' thru '9' ($30 - $39) }
{ VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' ($41 - $5A) }
	Ord('0')..Ord('9'): Result := Chr(Key and $ff);
	Ord('A')..Ord('Z'): Result := Chr(Key and $ff);
	VK_LWIN: Result := 'L.Win';
	VK_RWIN: Result := 'R.Win';
	VK_APPS: Result := 'Apps';
	96..96 + 9: Result := 'Num ' + Chr(Ord('0') + (Key and $ff) - 96);
	VK_MULTIPLY: Result := 'Num *';
	VK_ADD: Result := 'Num +';
	VK_SEPARATOR: Result := 'Separator';
	VK_SUBTRACT: Result := 'Num -';
	VK_DECIMAL: Result := 'Num ,';
	VK_DIVIDE: Result := 'Num /';
	112..112 + 23: Result := 'F' + NToS((Key and $ff)- 111, False);

	VK_NUMLOCK: Result := 'Num Lock';
	VK_SCROLL: Result := 'Scroll Lock';
{ VK_L & VK_R - left and right Alt, Ctrl and Shift virtual keys.
	Used only as parameters to GetAsyncKeyState() and GetKeyState().
	No other API or message will distinguish left and right keys in this way. }
	VK_LSHIFT: Result := 'L. Shift';
	VK_RSHIFT: Result := 'R. Shift';
	VK_LCONTROL: Result := 'L. Control';
	VK_RCONTROL: Result := 'R. Control';
	VK_LMENU: Result := 'L. Alt';
	VK_RMENU: Result := 'R. Alt';
	187: Result := '=';
	189: Result := '-';

	219: Result := '[';
	221: Result := ']';

	186: Result := ';';
	222: Result := '''';
	220: Result := '\';

	188: Result := ',';
	190: Result := '.';
	191: Result := '/';

	192: Result := '~';

	172: Result := 'WWW';
	180: Result := 'Mail';
	170: Result := 'Search';


	VK_PROCESSKEY: Result := 'Process Key';
	VK_ATTN: Result := 'Attn';
	VK_CRSEL: Result := 'CRSEL';
	VK_EXSEL: Result := 'EXSEL';
	VK_EREOF: Result := 'EREOF';
	VK_PLAY: Result := 'Play';
	VK_ZOOM: Result := 'Zoom';
	VK_NONAME: Result := 'Noname';
	VK_PA1: Result := 'PA1';
	VK_OEM_CLEAR: Result := 'OEM Clear';
	else Result := 'SC: ' + IntToStr(Key);
	end;
	if Key and scShift <> 0 then Result := 'Shift+' + Result;
	if Key and scCtrl <> 0 then Result := 'Ctrl+' + Result;
	if Key and scAlt <> 0 then Result := 'Alt+' + Result;
end;

procedure MsToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
var
	DW: LongWord;
begin
	if Abs(T) >= 1000 * Int64(High(LongWord)) then
	begin
		GH := 0;
		GM := 0;
		GS := 0;
		GD := 0;
		{$ifopt d+}
		IE(5453);
		{$endif}
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
				0: Mul := 60 * 1000;
				1: Mul := 60 * 60 * 1000;
				2: Mul := 60 * 60 * 1000;
				end;
				Inc(DP);
			end;
			end;
		end;
	end;
	Result := V;
end;

{
function SToMs(const Line: string): SG;
var
	h, m, s, d: SG;
	InLineIndex: SG;
begin
	InLineIndex := 1;
	h := StrToValI(ReadToChar(Line, InLineIndex, ':'), False, 0, 0, MaxInt, 1);
	m := StrToValI(ReadToChar(Line, InLineIndex, ':'), False, 0, 0, SG(59), 1);
	s := StrToValI(ReadToChar(Line, InLineIndex, '.'), False, 0, 0, SG(59), 1);
	d := StrToValI(ReadToChar(Line, InLineIndex, ' '), False, 0, 0, SG(999), 1);
	Result := (3600000 * h + 60000 * m + 1000 * s + d);
end;}

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

function SToTime(Str: string): TTime;
begin
	Result := SToMs(Str) / MSecsPerDay;
end;

function SToDate(Str: string): TDate;
var
	DateSep: Char;
	Year, Month, Day: U2;
	InLineIndex: SG;
begin
	if Str = '' then
	begin
		Result := 0;
		Exit;
	end;

	InLineIndex := 1;
	if Pos('/', Str) <> 0 then
	begin
		DateSep := '/';
		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
	end
	else if Pos('-', Str) <> 0 then
	begin
		DateSep := '-';
		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
	end
	else if Pos('.', Str) <> 0 then
	begin
		DateSep := '.';
		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
	end
	else if Length(Str) = 6 then
	begin
		Year := StrToValI(Copy(Str, 1, 2), False, 00, UG(00), 99, 1);
		if Year < ICentury then Inc(Year, 2000) else Inc(Year, 1900);
		Month := StrToValI(Copy(Str, 3, 2), False, 1, UG(1), 99, 1);
		Day := StrToValI(Copy(Str, 5, 2), False, 1, UG(1), 31, 1);
	end
	else if Length(Str) = 8 then
	begin
		Year := StrToValI(Copy(Str, 1, 4), False, 1900, UG(1900), 9999, 1);
		Month := StrToValI(Copy(Str, 5, 2), False, 1, UG(1), 12, 1);
		Day := StrToValI(Copy(Str, 7, 2), False, 1, UG(1), 31, 1);

	end
	else
//	if (Pos(',', Str) <> 0) or (Str[1] = '3') then
	begin
		Result := StrToValI(Str, False, 0, 0, MaxInt, 1);
		Exit;
	end;
	if Month > 50 then Dec(Month, 50); // Female offset
	if TryEncodeDate(Year, Month, Day, TDateTime(Result)) = False then
	begin
		// AddMes2 D???
		Result := 0;
	end;
end;

function SToDateTime(Str: string): TDateTime;
var InLineIndex: SG;
begin
	InLineIndex := 1;
	Result := SToDate(ReadToChar(Str, InLineIndex, ' ')) +
		SToTime(ReadToChar(Str, InLineIndex, CharCR));
end;

function MsToStr(const DT: Int64;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string;
begin
	Result := MsToStr(DT, True, Display, Decimals, FixedWidth);
end;

function MsToStr(const DT: Int64; const UseWinFormat: BG;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string;
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
	MsToHMSD(Abs(DT), h, m, s, d);

	if DT < 0 then Result := '-' else Result := '';

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

function DateToS(D: TDate): string;
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

function TimeToS(T: TTime): string;
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
		Result := DateToS(DateOf(DT)) + ' ' + TimeToS(TimeOf(DT));
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

{$ifopt d+}
procedure Nop;
asm
	nop
end;
{$endif}

{
procedure GetMem0(var P: Pointer; Size: Cardinal);
begin
	GetMem(P, Size);
	FillChar(P^, Size, 0);
end;}

procedure ReadMem(P: Pointer; Size: Cardinal); register;
asm
	cmp Size, 0
	je @Exit
	add Size, P
	@Loop:
		mov ecx, [P]
		add P, 4
		cmp P, Size
	jb @Loop
	@Exit:
end;

procedure FillU4(var Desc; Count: Cardinal; Value: U4); register;
asm
{     ->EAX     Pointer to destination  }
{       EDX     count   }
{       ECX     value   }

				PUSH    EDI

				MOV     EDI,EAX { Point EDI to destination              }

				MOV     EAX,ECX

				MOV     ECX,EDX
//        JS      @@exit
//				DEC ECX
//        SAR     ECX,2
//				JS      @@exit

				REP     STOSD   { Fill count dwords       }

{        MOV     ECX,EDX
				AND     ECX,3
				REP     STOSB   { Fill count MOD 4 bytes        }}

@@exit:
				POP     EDI
end;

procedure FillOrderU4(var Desc; Size: Cardinal); register;
asm
	cmp Size, 0
	je @Exit
	shl Size, 2
	add Size, Desc
	xor ecx, ecx
	@Loop:
		mov [Desc], ecx
		add Desc, 4
		cmp Desc, Size
		inc ecx
	jb @Loop
	@Exit:
end;

procedure Swap02(var Desc; Count: Cardinal; Step: S4); register;
asm
{     ->EAX     Pointer to destination  }
{       EDX     Count   }
{       ECX     Step    }

	PUSH    EDI
	MOV     EDI,EAX { Point EDI to destination              }
	add edx, edi
	@Loop:
//	xchg [edi], [edi + 2]
		mov al, [edi]
		xchg al, [edi + 2]
		mov [edi], al
		add edi, ecx
		cmp edi, edx
	jb @Loop
	POP     EDI
end;

procedure Exchange(var A, B: B1); register;
asm
	push bx
	push cx
	mov bl, B1 ptr [A]
	mov cl, B1 ptr [B]
	mov [B], bl
	mov [A], cl
	pop cx
	pop bx
end;

procedure Exchange(var A, B: B4); register;
asm
	push ebx
	push ecx
	mov ebx, B4 ptr [A]
	mov ecx, B4 ptr [B]
	mov [B], ebx
	mov [A], ecx
	pop ecx
	pop ebx
//  xchg A, B
end;

procedure Exchange(var A, B: U1); register;
asm
	push bx
	mov bl, U1 ptr [A]
	mov bh, U1 ptr [B]
	mov [B], bl
	mov [A], bh
	pop bx
end;

procedure Exchange(var A, B: S1); register;
asm
	push bx
	mov bl, S1 ptr [A]
	mov bh, S1 ptr [B]
	mov [B], bl
	mov [A], bh
	pop bx
end;

procedure Exchange(var A, B: U2); register;
asm
	push bx
	push cx
	mov bx, U2 ptr [A]
	mov cx, U2 ptr [B]
	mov [B], bx
	mov [A], cx
	pop cx
	pop bx
end;

procedure Exchange(var A, B: S2); register;
asm
	push bx
	push cx
	mov bx, S2 ptr [A]
	mov cx, S2 ptr [B]
	mov [B], bx
	mov [A], cx
	pop cx
	pop bx
end;

procedure Exchange(var A, B: U4); register;
asm
	push ebx
	push ecx
	mov ebx, U4 ptr [eax]
	mov ecx, U4 ptr [edx]
	mov [B], ebx
	mov [A], ecx
	pop ecx
	pop ebx
//  xchg A, B
end;

procedure Exchange(var A, B: S4); register;
asm
	push ebx
	push ecx
	mov ebx, S4 ptr [A]
	mov ecx, S4 ptr [B]
	mov [B], ebx
	mov [A], ecx
	pop ecx
	pop ebx
//  xchg A, B
end;

procedure Exchange(var A, B: S8); register;
var C: S8;
begin
	C := A;
	A := B;
	B := C;
end;

procedure Exchange(var A, B: F8); register;
var C: F8;
begin
	C := A;
	A := B;
	B := C;
end;

procedure Exchange(var A, B: FA); register;
var C: FA;
begin
	C := A;
	A := B;
	B := C;
end;

procedure Exchange(var A, B: Pointer); register;
asm
	push ebx
	push ecx
	mov ebx, U4 ptr [A]
	mov ecx, U4 ptr [B]
	mov [B], ebx
	mov [A], ecx
	pop ecx
	pop ebx
//  xchg A, B
end;

procedure Exchange(var P0, P1; Count: Cardinal); register;
asm
{     ->EAX     P0  }
{       EDX     P1   }
{       ECX     Count   }

				PUSH    EDI
				PUSH    ESI

				MOV     ESI,EAX { Point EDI to destination              }
				MOV     EDI,EDX { Point EDI to destination              }
				ADD     EDX,ECX
	@Loop:
//	xchg [edi], [esi]
		mov al, [edi]
		xchg al, [esi]
		mov [edi], al
		add edi, 1
		add esi, 1
		cmp edi, edx
	jb @Loop

				POP ESI
				POP     EDI
end;

function SelectDirectory(var Dir: string): BG;
var OpenDialog1: TOpenDialog;
begin
	OpenDialog1 := TOpenDialog.Create(nil);
	try
		if Dir = '' then Dir := WorkDir;
		OpenDialog1.FileName := '*.*';
		OpenDialog1.InitialDir := ExtractFilePath(Dir);
		OpenDialog1.Options := OpenDialog1.Options + [ofPathMustExist];
		if OpenDialog1.Execute then
		begin
			Result := True;
			Dir := ExtractFilePath(OpenDialog1.FileName);
		end
		else
			Result := False;
	finally
		OpenDialog1.Free;
	end;
end;

function DriveTypeToStr(const DriveType: Integer): string;
begin
	Result := '';
	case DriveType of
	DRIVE_UNKNOWN:  Result := 'Unknown'; // The drive type cannot be determined.
	DRIVE_NO_ROOT_DIR: Result := 'No root dir'; // The root directory does not exist.
	DRIVE_REMOVABLE: Result := 'Removable'; // The drive can be removed from the drive.
	DRIVE_FIXED: Result := 'Fixed'; // The disk cannot be removed from the drive.
	DRIVE_REMOTE: Result := 'Remote'; // The drive is a remote (network) drive.
	DRIVE_CDROM: Result := 'CD-ROM'; // The drive is a CD-ROM drive.
	DRIVE_RAMDISK: Result := 'Ramdisk'; // The drive is a RAM disk.
	end;
end;

function ProcessPriority(const Prior: Byte): Integer;
begin
	case Prior of
	0: Result := IDLE_PRIORITY_CLASS;
	1: Result := NORMAL_PRIORITY_CLASS;
	2: Result := HIGH_PRIORITY_CLASS;
	3: Result := REALTIME_PRIORITY_CLASS;
	else
		Result := NORMAL_PRIORITY_CLASS;
	end;
end;

function ThreadPriority(const Prior: Byte): Integer;
begin
	case Prior of
	0: Result := THREAD_PRIORITY_IDLE;
	1: Result := THREAD_PRIORITY_LOWEST;
	2: Result := THREAD_PRIORITY_BELOW_NORMAL;
	3: Result := THREAD_PRIORITY_NORMAL;
	4: Result := THREAD_PRIORITY_ABOVE_NORMAL;
	5: Result := THREAD_PRIORITY_HIGHEST;
	6: Result := THREAD_PRIORITY_TIME_CRITICAL;
	else
		Result := THREAD_PRIORITY_NORMAL;
	end;
end;

function GetCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const Index, Count: Integer): string;
begin
	Result := Application.Title;
	if Count > 0 then
	begin
		Result := Result + ' - ';
		if Count > 1 then
			Result := Result + '(' + NToS(Index + 1) + '/' + NToS(Count) + ') ';
		Result := Result + ShortDir(FName);
		if Changed then Result := Result + ' *';
		if New <> 0 then Result := Result + ' (New)';
	end;
end;

function ComponentName(Name: string): string;
var i: SG;
begin
	i := 1;
	while i <= Length(Name) do
	begin
		if not (CharsTable[Name[i]] in [ctLetter, ctNumber]) then
			Delete(Name, i, 1)
		else
			Inc(i);
	end;
	if Name = '' then
		Name := 'N'
	else
	begin
		if CharsTable[Name[1]] <> ctLetter then
			Name := 'N' + Name;
	end;
	Result := Name;
end;

function MenuNameToFileName(Name: string): string;
begin
	Result := Name;
	while Length(Result) > 0 do
	begin
		case Result[Length(Result)] of
		'0'..'9':
		begin
			SetLength(Result, Length(Result) - 1);
		end;
		'_':
		begin
			SetLength(Result, Length(Result) - 1);
			Break;
		end
		else
			Break;
		end;
	end;
end;

function ButtonNameToFileName(Name: string; const Space: Boolean): string;
label LDel;
var
	Index, i: SG;
	Found: BG;
const
	Names: array[0..4] of string = ('DBUTTON', 'BUTTON', 'COMBOBOX', 'EDIT', 'MEMO');
begin
	Result := Name;
	Found := False;
	for i := 0 to Length(Names) - 1 do
	begin
		Index := Pos(Names[i], UpperCase(Result));
		if Index = 1 then
		begin
			Delete(Result, Index, Length(Names[i]));
			Found := True;
			Break;
		end;
	end;

	if Found = False then
	while Length(Result) > 0 do
	begin
		case Result[Length(Result)] of
		'0'..'9':
		begin
			SetLength(Result, Length(Result) - 1);
		end;
		'_':
		begin
			SetLength(Result, Length(Result) - 1);
			Break;
		end
		else
			Break;
		end;
	end;


	if Space then
		for Index := 2 to Length(Result) do
			if Result[Index] in ['A'..'Z'] then
				if Result[Index - 1] in ['a'..'z'] then
					Insert(' ', Result, Index);
end;
{
procedure CorrectFormPos(Form: TForm);
begin
	if not Assigned(Form) then Exit;
	if Form.Left + Form.Width > Screen.Width then Form.Left := Screen.Width - Form.Width;
	if Form.Top + Form.Height > Screen.Height then Form.Top := Screen.Height - Form.Height;
	if Form.Left < 0 then Form.Left := 0;
	if Form.Top < 0 then Form.Top := 0;
end;}
{
procedure SetListViewItems(ListView: TListView; NewSize: SG);
var j: SG;
begin
	if NewSize > ListView.Items.Count then
	begin
		for j := 0 to NewSize - ListView.Items.Count - 1 do
		begin
			ListView.Items.Add;
		end;
	end
	else
	begin
		for j := ListView.Items.Count - 1 downto NewSize do
		begin
			ListView.Items[j].Delete;
		end;
	end;
end;
}
procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: Word;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);
var
	MyObject : IUnknown;
	MySLink : IShellLink;
	MyPFile : IPersistFile;
begin
	MyObject := CreateComObject(CLSID_ShellLink);
	MySLink := MyObject as IShellLink;
	MyPFile := MyObject as IPersistFile;

	MySLink.SetArguments(@Arguments[1]);
	MySLink.SetPath(@Target[1]);
	MySLink.SetWorkingDirectory(@StartIn[1]);
	MySLink.SetDescription(@Description[1]);
	MySLink.SetIconLocation(@IconFileName[1], IconIdex);
	MySLink.SetHotkey(HotKey);

	if not DirectoryExists(ExtractFileDir(LinkFileName)) then
		CreateDir(ExtractFileDir(LinkFileName));
	MyPFile.Save(PWChar(LinkFileName), False);
	MySLink := nil;
	MyPFile := nil;
	MyObject := nil;
end;

function DropFiles(hDrop: THandle): TStrings;
var
	fName: array[0..4095] of Char;
	NumberOfFiles: Integer;
	fCounter: Integer;
begin
	NumberOfFiles := DragQueryFile(hDrop, $FFFFFFFF, fName, SizeOf(fName));
	Result := TStringList.Create;
	Result.BeginUpdate;
	for fCounter := 0 to NumberOfFiles - 1 do
	begin
		DragQueryFile(hDrop, fCounter, fName, 254);
		Result.Add(fName);
	end;
	Result.EndUpdate;
	DragFinish(hDrop);
end;

procedure RAToXY(Len: SG; Angle: TAngle; out X, Y: SG);
begin
	X := RoundDiv(Len * (Sins[Angle mod AngleCount]), SinDiv);
	Y := RoundDiv(Len * (Sins[(AngleCount div 4 + Angle) mod AngleCount]), SinDiv);
end;

procedure InitSin;
var i: TAngle;
begin
	for i := 0 to AngleCount - 1 do
	begin
		Sins[i] := Round(SinDiv * sin(2 * pi * i / AngleCount));
//  Sins[i]:=Trunc(127*(sin(pi*i/128))+127);
//  Sins[i]:=Trunc(128*(sin(pi*i/128)+1))-128;
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
	{$WARN SYMBOL_PLATFORM OFF}
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
	{$WARN SYMBOL_PLATFORM ON}
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
	{$ifndef LINUX}
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	{$endif}
	InitSin;
	GetLocale;
end.

