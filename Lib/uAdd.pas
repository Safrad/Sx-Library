//* File:     Lib\uAdd.pas
//* Created:  1998-01-01
//* Modified: 2004-04-28
//* Version:  X.X.31.X
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
	U8 = Int64; // Wor64/Car64 for 64bit Delphi?

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
	F6 = Real48;
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

	TArrayChar = array[0..1024 * 1024 * 1024 - 1] of AnsiChar;
	PArrayChar = ^TArrayChar;

	TIndex = SG;

//	string = string;

// Graphics
const
	MaxSpectrum = 1529;
type
	TRColor = packed record
		case Integer of
		0: (L: - $7FFFFFFF - 1..$7FFFFFFF);
		1: (R, G, B, T: Byte);
		2: (WordRG, WordBT: Word);
	end;
	THSVColor = packed record // 4
		Hue: 0..MaxSpectrum; // 2
		Saturation: 0..239; // 1
		Value: 0..255; // 1
	end;
	THLSColor = packed record // 4
		Hue: 0..MaxSpectrum; // 2
		Lightness: 0..255; // 1
		Saturation: 0..239; // 1
	end;

const
	DefMemBuffer = 4096; // Best Performance
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

procedure RAToXY(Len: SG; Angle: TAngle; out X, Y: SG);

function RColor(R, G, B: U1): TRColor;

function Sgn(const I: S1): SG; overload;
function Sgn(const I: S2): SG; overload;
function Sgn(const I: S4): SG; overload;
function Sgn(const I: S8): SG; overload;
function Sgn(const I: F4): SG; overload;
function Sgn(const I: F8): SG; overload;
function Sgn(const I: FA): SG; overload;
function SgnMul(const Signum, Num: SG): SG;

procedure DivModU32(const Dividend: LongWord; const Divisor: Word;
	out Res, Remainder: Word);
procedure DivModS32(const Dividend: LongInt; const Divisor: SmallInt;
	out Res, Remainder: SmallInt);
procedure DivModU64(const Dividend: Int64; const Divisor: LongWord;
	out Res, Remainder: LongWord);
procedure DivModS64(const Dividend: Int64; const Divisor: LongInt;
	out Res, Remainder: LongInt);

function UnsignedMod(const Dividend: Int64; const Divisor: Integer): Integer;
function FastSqrt(A: SG): SG;
function LinearMax(Clock, Maximum: LongWord): LongWord;

function RoundEx(Value: FA): SG;
function RoundDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function RoundDivU8(const Dividend: U8; const Divisor: U8): S8; //overload;
function RoundDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;
function MaxDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function MaxDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;

function Range(const Min, Cur, Max: Integer): Integer; overload;
function Range(const Min, Cur, Max, Def: Integer): Integer; overload;
function Range(const Min, Cur, Max: Cardinal): Cardinal; overload;

procedure Change(var A, B: Integer); overload;
procedure Change(var A, B: Extended); overload;

function Arg(X, Y: Extended): Extended; overload;

function Random2(Range: SG): SG;

procedure CheckBool(var Bool: ByteBool); overload;
procedure CheckBool(var Bool: WordBool); overload;
procedure CheckBool(var Bool: LongBool); overload;

procedure Order(var I1, I2: Integer); overload;
procedure Order(var I1, I2: Cardinal); overload;

function CalcShr(N: U4): S1;

function AllocByB(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;
function AllocByEx(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;

// Format functions


// Number Format
var
	NativeSymbols: string[10];

	DecimalSeparator: string[3]; // Decimal symbol
	DigitsAfterDecimal: SG; // No. of digits after decimal
	ThousandSeparator: string[3]; // Digit grouping symbol
	ThousandGroup: SG; // Digit grouping
	FractionGroup: SG;
	NegSymbol: string[4]; // Negatove sing symbol
	PosSymbol: string[4]; // Negatove sing symbol
	NegFormat: SG; // Negative number format
	LeadingZero: SG; // Display leading zeros
	ListSeparator: string[3]; // List separator

// Time Format
	TimeSeparator: string[3];

//var WinDecimalSeparator: Char;
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

function NToS(const Num: Int64): string; overload;
function NToS(const Num: Int64; const Decimals: SG): string; overload;
function NToS(const Num: Int64; const UseFormat: string): string; overload;
function NToS(const Num: Int64; const UseWinFormat: BG): string; overload;
function NToS(const Num: Int64; const UseWinFormat: BG; const Decimals: SG): string; overload;
function NToHS(Num: Int64): string;

function FToS(Num: Extended): string; overload;
function FToS(Num: Extended; const UseWinFormat: BG): string; overload;

procedure msToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
type
	TDisplay = (diDHMSD, diHHMSD, diHMSD, diMSD, diSD);

{Decimals
-3: 0:34.34
3: 0:34.340
}

function MsToStr(const DT: Int64;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string; overload;
function MsToStr(const DT: Int64; const UseWinFormat: BG;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): string; overload;

function DateToS(var Year, Month, Day: U2): string; overload;
function DateToS(D: TDate): string; overload;
function TimeToS(T: TTime): string;
function DateTimeToS(DT: TDateTime): string;
function DTToStr(DT: TDateTime): string; // UseWinFormat = True

(*
function StrToValC(S: string;
	const MinVal, DefVal, MaxVal, Denominator: Cardinal): Cardinal;
function StrToValE(S: string;
	const MinVal, DefVal, MaxVal: Extended): Extended;
*)
function BToStr(const B: S4): string; overload;
function BToStr(const B: S8): string; overload;

//function SToMs(const Str: string): SG; // MsToStr<-

function SToDate(Str: string): TDate;
function SToTime(Str: string): TTime;
function SToDateTime(Str: string): TDateTime;

function PhoneToStr(Phone: U8): string;

// System
procedure Nop;
procedure GetMem0(var P: Pointer; Size: Cardinal);
function DriveTypeToStr(const DriveType: Integer): string;
function ProcessPriority(const Prior: Byte): Integer;
function ThreadPriority(const Prior: Byte): Integer;

function GetCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const Index, Count: Integer): string;

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

procedure ObjectFree(var Obj: TObject);

function DropFiles(hDrop: THandle): TStrings;

implementation

uses
	Windows, Math, Dialogs, ShellAPI,
	uError, uStrings, uInput, uFiles;

function RColor(R, G, B: U1): TRColor;
begin
//	Result := R + B shl 8 + G shl 16;
	Result.T := 0;
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

procedure DivModU32(const Dividend: LongWord; const Divisor: Word;
	out Res, Remainder: Word);
asm
	PUSH    EBX
	MOV     EBX,EDX
	MOV     EDX,EAX
	SHR     EDX,16
	DIV     BX
	MOV     EBX,Remainder
	MOV     [ECX],AX
	MOV     [EBX],DX
	POP     EBX
end;

procedure DivModS32(const Dividend: LongInt; const Divisor: SmallInt;
	out Res, Remainder: SmallInt);
asm
	PUSH    EBX
	MOV     EBX,EDX
	MOV     EDX,EAX
	SHR     EDX,16
	IDIV    BX
	MOV     EBX,Remainder
	MOV     [ECX],AX
	MOV     [EBX],DX
	POP     EBX
end;

procedure DivModU64(const Dividend: Int64; const Divisor: LongWord;
	out Res, Remainder: LongWord);
begin
	asm
	pushad
	mov edx, [ebp+$0c] //Divident-hi
	mov eax, [ebp+$08] //Divident-lo
	mov ebx, Divisor
	div ebx // eax:=edx&eax div ebx; edx:=edx&eax mod ebx
	mov edi, Res
	mov [edi], eax
	mov edi, Remainder
	mov [edi], edx
	popad
	end;
end;

procedure DivModS64(const Dividend: Int64; const Divisor: LongInt;
	out Res, Remainder: LongInt);
begin
	asm
	pushad
	mov edx, [ebp+$0c] //Divident-hi
	mov eax, [ebp+$08] //Divident-lo
	mov ebx, Divisor
	idiv ebx // eax:=edx&eax div ebx; edx:=edx&eax mod ebx
	mov edi, Res
	mov [edi], eax
	mov edi, Remainder
	mov [edi], edx
	popad
	end;
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

function RoundEx(Value: FA): SG;
begin
	if Value > MaxInt then
		Result := MaxInt
	else if Value < MinInt then
		Result := MinInt
	else
		Result := Round(Value);
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

procedure Change(var A, B: Integer);
asm
	push ebx
	push ecx
	mov ebx, dword ptr [A]
	mov ecx, dword ptr [B]
	mov [B], ebx
	mov [A], ecx
	pop ebx
	pop ecx
//  xchg A, B
end;

procedure Change(var A, B: Extended);
var C: Extended;
begin
	C := A;
	A := B;
	B := C;
end;

function Random2(Range: SG): SG;
begin
	Result := Random(2 * Range + 1) - Range;
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
end;

function NToS(const Num: Int64; const UseFormat: string): string;
var
	Nums: string;
	i, j: SG;
	PointPos: SG;
	NumFound: BG;
begin
	Result := '';
	Nums := IntToStr(Abs(Num));
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
						Result := '0' + Result;
				end;
			end
			else
			begin
				if (UseFormat[i] = '#') and ((i < PointPos) or (NumFound = False)) then
					Result := ' ' + Result
				else
					Result := '0' + Result;
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
			if Result <> '' then
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

	if Num = 0 then
		Nums := ''
	else
		Nums := IntToStr(Abs(Num));

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
					if UseWinFormat then
						Result := ThousandSep + Result
					else
						Result := ',' + Result;
				end;
		end
		else if (M > 0) then
		begin
			if ThousandGr > 0 then
				if Abs(M) mod ThousandGr = 0 then
				begin
					if UseWinFormat then
						Result := ThousandSep + Result
					else
						Result := ',' + Result;
				end;
		end;
	end;

	if Num < 0 then
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
end;

function NToHS(Num: Int64): string;
begin
	repeat
		case Num and $f of
		0..9: Result := Result + Chr(Ord('0') + (Num and $f));
		else Result := Result + Chr(Ord('A') + (Num and $f) - $a);
		end;
		Num := Num shr 4;
	until Num = 0;
end;

function FToS(Num: Extended): string;
begin
	Result := FToS(Num, True);
end;

function FToS(Num: Extended; const UseWinFormat: BG): string;
var
	D: SG;
	Nu: Extended;
begin
	D := 0;
	Nu := Num;
	while True do
	begin
		if Abs(Frac(Nu)) <= Math.MinExtended then Break;
		if Abs(Nu) < MaxInt8 div 10 then
			Nu := Nu * 10
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
	end
	else
	begin
		DivModU64(Abs(T), 1000, DW, GD);
		DivModU64(DW, 60, DW, GS);
		DivModU64(DW, 60, GH, GM);
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
		Year := StrToValI(Copy(Str, 1, 2), False, 1900, UG(1900), 9999, 1);
		Month := StrToValI(Copy(Str, 3, 2), False, 1, UG(1), 12, 1);
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
	Result := EncodeDate(Year, Month, Day);
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
	Res, Rem: U2;

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
			DivModU32(h, 10, Res, Rem);
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
			DivModU32(h, 10, Res, Rem);
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
			DivModU32(m, 10, Res, Rem);
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
			DivModU32(s, 10, Res, Rem);
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
		Result := DateTimeToStr(DT);
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

procedure Nop;
begin
	asm
	nop
	end;
end;

procedure GetMem0(var P: Pointer; Size: Cardinal);
begin
	GetMem(P, Size);
	FillChar(P^, Size, 0);
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
var Index, Count: SG;
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

	Index := Pos('DBUTTON', UpperCase(Result));
	Count := 7;
	if Index <> 0 then Delete(Result, Index, Count);

	Index := Pos('BUTTON', UpperCase(Result));
	Count := 6;
	if Index <> 0 then Delete(Result, Index, Count);

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

	MySLink.SetArguments(PChar(Arguments));
	MySLink.SetPath(PChar(Target));
	MySLink.SetWorkingDirectory(PChar(StartIn));
	MySLink.SetDescription(PChar(Description));
	MySLink.SetIconLocation(PChar(IconFileName), IconIdex);
	MySLink.SetHotkey(HotKey);

	if not DirectoryExists(ExtractFileDir(LinkFileName)) then
		CreateDir(ExtractFileDir(LinkFileName));
	MyPFile.Save(PWChar(LinkFileName), False);
	MySLink := nil;
	MyPFile := nil;
	MyObject := nil;
end;

procedure ObjectFree(var Obj: TObject);
begin
	Obj.Free; Obj := nil;
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
var
	s: string;
	InLineIndex: SG;
begin
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

//	WinDecimalSeparator := DecimalSeparator;
end;

initialization
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	InitSin;
	GetLocale;
end.
