// Build: 01/1998-09/1999 Author: Safranek David

unit uAdd;

interface

uses SysUtils, Forms, ShlObj, ActiveX, ComObj, ComCtrls;

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

	TArrayByte = array[0..1024 * 1024 * 1024 - 1] of Byte;
	TArrayChar = array[0..1024 * 1024 * 1024 - 1] of AnsiChar;
	PArrayByte = ^TArrayByte;
	PArrayChar = ^TArrayChar;

	TString = string;

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

function Sgn(const I: S1): SG; overload;
function Sgn(const I: S2): SG; overload;
function Sgn(const I: S4): SG; overload;
function Sgn(const I: S8): SG; overload;
function Sgn(const I: F4): SG; overload;
function Sgn(const I: F8): SG; overload;
function Sgn(const I: FA): SG; overload;
function SgnMul(const Signum, Num: SG): SG;

procedure DivModU32(const Dividend: LongWord; const Divisor: Word;
	var Res, Remainder: Word);
procedure DivModS32(const Dividend: LongInt; const Divisor: SmallInt;
	var Res, Remainder: SmallInt);
procedure DivModU64(const Dividend: Int64; const Divisor: LongWord;
	var Res, Remainder: LongWord);
procedure DivModS64(const Dividend: Int64; const Divisor: LongInt;
	var Res, Remainder: LongInt);

function UnsignedMod(const Dividend: Int64; const Divisor: Integer): Integer;
function FastSqrt(A: SG): SG;
function LinearMax(Clock, Maximum: LongWord): LongWord;

function RoundDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function RoundDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;
function MaxDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function MaxDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;

function Range(const Min, Cur, Max: Integer): Integer; overload;
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
function NToS(const Num: Int64): TString; overload;
function NToS(const Num: Int64; const Decimals: SG): TString; overload;
function NToS(const Num: Int64; const UseFormat: TString): TString; overload;
function NToS(const Num: Int64; const UseWinFormat: BG): TString; overload;
function NToS(const Num: Int64; const UseWinFormat: BG; const Decimals: SG): TString; overload;

function FToS(Num: Extended): TString; overload;
function FToS(Num: Extended; const UseWinFormat: BG): TString; overload;

function TimeToInt(Line: string): SG;

function StrToValExt(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string;
	var InStr: string; var ErrorLineIndex: SG): Extended;

function StrToValE(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended; overload;
function StrToValE(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended; overload;

function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG; overload;
function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG; overload;

function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG; out ErrorMsg: string): SG; overload;
function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; out ErrorMsg: string): UG; overload;

function StrToValS8(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8): S8;

function StrToValU1(Line: TString; const UseWinFormat: BG;
	const DefVal: U1): U1;

(*
function StrToValC(S: TString;
	const MinVal, DefVal, MaxVal, Denominator: Cardinal): Cardinal;
function StrToValE(S: TString;
	const MinVal, DefVal, MaxVal: Extended): Extended;
*)
function BToStr(const B: Integer): TString; overload;
function BToStr(const B: Int64): TString; overload;

// Time
procedure msToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
type
	TDisplay = (diDHMSD, diHMSD, diMSD, diSD);
function msToStr(const DT: Int64;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): TString; overload;
function msToStr(const DT: Int64; const UseWinFormat: BG;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): TString; overload;
function Date6(Year, Month, Day: Word): string;
function Date6Now: string;

// System
procedure Nop;
function DriveTypeToStr(const DriveType: Integer): TString;
function ProcessPriority(const Prior: Byte): Integer;
function ThreadPriority(const Prior: Byte): Integer;

function GetSingleCaption(const FName: TFileName; const Changed: Boolean;
	const New: Boolean): string;

function GetMultiCaption(const FName: TFileName; const Changed: Boolean;
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

implementation

uses
	Windows, Math, Dialogs,
	uError, uDialog, uStrings;

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
	var Res, Remainder: Word);
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
	var Res, Remainder: SmallInt);
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
	var Res, Remainder: LongWord);
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
	var Res, Remainder: LongInt);
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
		MessageD('Div0', mtError, [mbOk]);
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

function RoundDiv(const Dividend: SG; const Divisor: SG): SG;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	{$ifopt d+}
	if Divisor = 0 then
	begin
		MessageD('Division by 0 (' + NToS(Dividend) + ' / 0)', mtError, [mbOk]);
		Result := 0;
		Exit;
	end;
	{$endif}
	if Dividend < 0 then
		Result := (Dividend - (Divisor div 2)) div Divisor
	else
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
		MessageD('Division by 0 (' + NToS(Dividend) + ' / 0)', mtError, [mbOk]);
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
		MessageD('Div0', mtError, [mbOk]);
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
		MessageD('Div0', mtError, [mbOk]);
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
		ErrorMessage('Bad AllocBy block size ' + NToS(BlockSize) + ' bytes');
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
		ErrorMessage('Bad AllocBy block size ' + NToS(BlockSize) + ' bytes');
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

function NToS(const Num: Int64; const UseFormat: TString): TString;
var
	Nums: TString;
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
					if (UseFormat[i] = '#') and ((i < PointPos) or (NumFound = False)) then
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
		else
			IE(34343);
	end;
end;

function NToS(const Num: Int64): TString;
begin
	Result := NToS(Num, True, 0);
end;

function NToS(const Num: Int64; const Decimals: SG): TString;
begin
	Result := NToS(Num, True, Decimals);
end;

function NToS(const Num: Int64; const UseWinFormat: BG): TString;
begin
	Result := NToS(Num, UseWinFormat, 0);
end;

// 454,545,455.456465; 0.045
function NToS(const Num: Int64; const UseWinFormat: BG; const Decimals: SG): TString;
var
	DecimalSep, ThousandSep: string[3];
	ThousandGr, FractionGr: SG;

	Nums: TString;
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

function FToS(Num: Extended): TString;
begin
	Result := FToS(Num, True);
end;

function FToS(Num: Extended; const UseWinFormat: BG): TString;
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
function Using(const Typ: TString; const Num: Int64): TString;
label LExit;
var
	inp: TString;
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

function StrToI(s: string; Decimals: SG): SG; overload;
var
	Code: Integer;
	e: Extended;
	i: Integer;
	Point: SG;
begin
	if s = '' then
	begin
		Result := 0;
		Exit;
	end;
	// Disk Format 2,456,454,546.42454
	if CharCount(s, '.') > 0 then IE(431);

	DelChars(s, ',');

	if Decimals > 0 then
	begin
		Val(s, e, Code);
		if Code <> 0 then
			Result := 0
		else
		begin
			Point := 10;
			for i := 2 to Decimals do
				Point := Point * 10;

			Result := Round(Point * e);
		end;
	end
	else
	begin
		Val(s, Result, Code);
		if Code <> 0 then
			Result := 0;
	end;
{var
	i, j: SG;
	Point: SG;
	Minus: Boolean;
begin
	Result := 0;
	Minus := False;
	Point := -1;
	for i := 1 to Length(s) do
	begin
		case s[i] of
		'-': Minus := not Minus;
		'0'..'9':
		begin
			if Point = 0 then MessageD('Invalid float number decimals', mtWarning, [mbOk]);
			if Point <> -1 then
			begin
				if Point <> 0 then
				begin
					if Minus then
						Result := Result - Point * (Ord(s[i]) - Ord('0'))
					else
						Result := Result + Point * (Ord(s[i]) - Ord('0'));
				end;
				Point := Point div 10;
//				if Point = 0 then Break;
			end
			else
			begin
				Result := Result * 10;
				if Minus then
					Result := Result - (Ord(s[i]) - Ord('0'))
				else
					Result := Result + (Ord(s[i]) - Ord('0'));
			end;
		end;
		'.':
		begin
			if Decimals = 0 then
			begin
				MessageD('Integer number required, float found', mtWarning, [mbOk]);
				Break;
			end;
//			if Decimals = 0 then Break;
			Point := 1;
			for j := 2 to Decimals do
				Point := Point * 10;
			Result := Result * 10 * Point;
		end;
		end;
	end;
end;}

function TimeToInt(Line: string): SG;
var
	h, m, s, d: SG;
	InLineIndex: SG;
begin
	InLineIndex := 1;
	h := StrToValI(ReadToChar(Line, InLineIndex, ':'), False, 0, 0, MaxInt, 1);
	m := StrToValI(ReadToChar(Line, InLineIndex, ':'), False, 0, 0, SG(59), 1);
	s := StrToValI(ReadToChar(Line, InLineIndex, ','), False, 0, 0, SG(59), 1);
	d := StrToValI(ReadToChar(Line, InLineIndex, ' '), False, 0, 0, SG(999), 1);
	Result := (3600000 * h + 60000 * m + 1000 * s + d);
end;


var
	CharsTable: array[Char] of (ctSpace, ctLetter, ctIllegal, ctNumber, ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose);
const
	ConstE = 2.7182818284590452353602874713527;

function StrToValExt(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string;
	var InStr: string; var ErrorLineIndex: SG): Extended;
type
	TOperator = (opNone,
		opPlus, opMinus, opMul, opDiv,
		opAbs, opNeg, opInv, opNot, opInc, opDec, opFact,
		opPower, opExp, opLn, opSqr, opSqrt,
		opSin, opCos, opTan, opArcSin, opArcCos, opArcTan,
		opHypSin, opHypCos,
		opAvg);
var
	DecimalSep, ThousandSep: string[3];

	i: Integer;
	Per, Point: Boolean;
	PointDiv: Extended;
	Num: Integer;
	Base: Integer;

	MaxLineIndex: Integer;
	LineIndex: Integer;
	StartIndex: SG;
	LastLineIndex: Integer;

	Res, Exp: Extended;
	Level, MaxLevel: Integer;
	Where: (whNum, whExp);

	procedure ShowError(s: string);
	begin
		if ErrorMsg = '' then
		begin
			ErrorLineIndex := LineIndex;
			{$ifopt d+}
//			ErrorMessage(ErrorMsg);
			{$endif}
		end;
		ErrorMsg := ErrorMsg + 'Error (' + NToS(LineIndex) + '): ' + s + #13 + #10;
	end;

	function Make(LastOperator: TOperator): Extended;
	label LFin, LNext;
	var
		ArgCount: SG;
		Args: array of Extended;
		Unar, UnarExp: Boolean;

		procedure AddArgument(Res: Extended);
		begin
			Inc(ArgCount);
			SetLength(Args, ArgCount);
			if Unar then
			begin
				Unar := False;
				Res := -Res;
			end;
			Args[ArgCount - 1] := Res;
		end;

	var
		a: SG;
		Fce, FceO: string;
		NextOperator: TOperator;
	begin
		Result := 0;
		if Level >= 255 then Exit;
		Inc(Level); if Level > MaxLevel then MaxLevel := Level;
		Unar := False;
		UnarExp := False;
		ArgCount := 0;
		SetLength(Args, 0);
		NextOperator := opNone;

		while True do
		begin
			if (LineIndex > Length(Line)) then Break;
			if CharsTable[Line[LineIndex]] = ctSpace then
			begin
				Inc(LineIndex);
				Continue;
			end;
			if CharsTable[Line[LineIndex]] = ctNumber then
			begin
				LastLineIndex := LineIndex;
{       repeat
					Inc(LineIndex);
				until not ((LineIndex <= Length(Line)) and (CharsTable[Line[LineIndex]] = ctNumber));}

				Per := False;
				Base := 10;
				Point := False;
				PointDiv := 1;
				Res := 0;
				Exp := 0;
				Where := whNum;
				while LineIndex <= Length(Line) do
				begin
					case Line[LineIndex] of
					'%': Per := True;
					'#': Base := 2;
					'O', 'o': Base := 8;
					'!': Base := 10;
					'$', 'x', 'X', 'h', 'H': Base := 16;
					'*', '/', ':', '^', ')', '(': Break;
					'-', '+': if (Base <> 10) or (UpCase(Line[LineIndex - 1]) <> 'E') then Break else UnarExp := True;
					'.': Point := True;
					',':
					begin
					
					end
					else
					begin
{						if (UseWinFormat and (Copy(Line, LineIndex, Length(DecimalSep)) = DecimalSep)) then
						begin
							if Point = True then ShowError('Too many decimal points');
							Point := True;
							Inc(LineIndex, Length(DecimalSep) - 1);
						end
						else if (Line[LineIndex] = '.') then
						begin
							if Point = True then ShowError('Too many decimal points');
							Point := True;
//							Inc(LineIndex);
						end
						else
						begin}
							case UpCase(Line[LineIndex]) of
							'0'..'9': Num := Ord(Line[LineIndex]) - Ord('0');
							'A'..'F':
							begin
								if Base = 16 then
									Num := 10 + Ord(UpCase(Line[LineIndex])) - Ord('A')
								else if UpCase(Line[LineIndex]) = 'E' then
								begin
									Where := whExp;
									Base := 10;
									Point := False;
									PointDiv := 1;
									goto LNext;
								end;
							end
							else
								Break;
							end;
{							case UpCase(Line[LineIndex]) of
							'0'..'9', 'A'..'F':
							begin}
								if Where = whExp then
									if Point = False then
									begin
										Exp := Exp * Base;
										Exp := Exp + Num;
									end
									else
									begin
										PointDiv := PointDiv * Base;
										Exp := Exp + Num / PointDiv;
									end
								else
									if Point = False then
									begin
										Res := Res * Base;
										Res := Res + Num;
									end
									else
									begin
										PointDiv := PointDiv * Base;
										Res := Res + Num / PointDiv;
									end;
{							end;
							end;}
//						end;
					end;
					end;
					LNext:
					Inc(LineIndex);
				end;

				if Per then Res := Res * MaxVal / 100;
				if UnarExp then Exp := -Exp;
				if Abs(Exp) > 1024 then Exp := Sgn(Exp) * 1024;
				Res := Res * Power(10, Exp);
//				if Unar then Res := -Res;

//        Val(Copy(Line, LastLineIndex, LineIndex - LastLineIndex), Res, ErrorCode);
				AddArgument(Res);
{				if LastOperator = opNone then
				begin
					R1 := Res;
					LastOperator := opWaitOperator;
				end
				else
				begin
					if LastOperator = opWaitOperator then
					begin
						ShowError('Missing operator');
					end
					else
						R2 := Res;
//						LastOperator := opNone;
				end;}
			end
			else
			begin
				case Line[LineIndex] of
				'(':
				begin
//					if LastOperator = opNone then ShowError('Operator required');
					Inc(LineIndex);
					Res := Make(NextOperator);
					AddArgument(Res);
				end;
				')':
				begin
					LFin:
					case LastOperator of
					opNone:
					begin
						if ArgCount = 0 then
							ShowError('Argument required')
						else if ArgCount = 1 then
							Result := Args[0]
						else
							ShowError('Too many arguments');
					end;
					opPlus:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Plus');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result + Args[a];
							end;
						end;
					end;
					opAvg:
					begin
						if ArgCount < 1 then
						begin
							ShowError('Too few arguments for Avg');
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result + Args[a];
							end;
							Result := Result / ArgCount;
						end;
					end;
					opMinus:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Minus');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result - Args[a];
							end;
						end;
					end;
					opMul:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Mul');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								Result := Result * Args[a];
							end;
						end;
					end;
					opDiv:
					begin
						if ArgCount < 2 then
						begin
							ShowError('Too few arguments for Div');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Args[0];
							for a := 1 to ArgCount - 1 do
							begin
								if Args[a] = 0 then
								begin
									ShowError('Division by zero');
								end
								else
									Result := Result / Args[a];
							end;
						end;
					end;
					opAbs:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Abs');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Abs(Args[0]);
						end;
					end;
					opNeg:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Neg');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := -Args[0];
						end;
					end;
					opInv:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Inv');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							if Args[0] = 0 then
								ShowError('Division by zero')
							else
								Result := 1 / Args[0];
						end;
					end;
					opNot:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Not');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := -Args[0] - 1;
						end;
					end;
					opInc:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Inc');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Args[0] + 1;
						end;
					end;
					opDec:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Dec');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Args[0] - 1;
						end;
					end;
					opFact:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Neg');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := 1;
							if Round(Args[0]) < 0 then
								ShowError('Input -infinity..2000 for Fact')
							else if Round(Args[0]) <= 1754 then
							begin
								for a := 2 to Round(Args[0]) do
									Result := Result * a;
							end
							else
							begin
								if Round(Args[0]) > 1754 then Result := Infinity;
							end;
						end;
					end;
					opPower:
					begin
						if ArgCount < 2 then
						begin
							ShowError('2 arguments required for Power');
							if ArgCount = 1 then Result := Args[0];
						end
						else
						begin
							Result := Power(Args[0], Args[1]);
	{         Result := 1;
						i := 1;
						while  i <= R2 do
						begin
							Result := Result * R1;
							Inc(i);
						end;}
						end;
					end;
					opExp:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Exp');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							Result := Power(ConstE, Args[0]);
						end;
					end;
					opLn:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Ln');
							if ArgCount = 1 then Result := Args[0];
						end;
						if ArgCount >= 1 then
						begin
							if Args[0] > 0 then
								Result := Ln(Args[0])
							else
								ShowError('Input 0..infinity for Ln');
						end;
					end;
					opSqr:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Sqr');
						end;
						if ArgCount >= 1 then
							Result := Sqr(Args[0]);
					end;
					opSqrt:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Sqrt');
						end;
						if ArgCount >= 1 then
							Result := Sqrt(Args[0]);
					end;

					opSin:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Sin');
						end;
						if ArgCount >= 1 then
							Result := Sin(Args[0]);
					end;
					opCos:
					begin
						if ArgCount <> 1 then
						begin
							ShowError('1 argument required for Cos');
						end;
						if ArgCount >= 1 then
							Result := Sin(Args[0]);
					end;
					end;
					Inc(LineIndex);
					Break;
				end;
				'+':
				begin
{					case LastOperator of
					opNone:}
						LastOperator := opPlus;
//					end;
					Inc(LineIndex);
				end;
				'-':
				begin
{		      if LastOperator = opWaitOperator then
					begin
						if (Line[LineIndex - 1] = 'E') then
							UnarExp := not UnarExp
						else
							LastOperator := opMinus
					end
					else
						Unar := not Unar;}
{					if LastOperator = opNone then
					begin
					end}
					if ArgCount = 0 then
						Unar := not Unar
					else
						LastOperator := opMinus;
					Inc(LineIndex);
				end;
				'*':
				begin
					LastOperator := opMul;
					Inc(LineIndex);
				end;
				'/', ':':
				begin
					LastOperator := opDiv;
					Inc(LineIndex);
				end;
				'^':
				begin
					LastOperator := opPower;
					Inc(LineIndex);
				end;
				';', ' ':
				begin
					// Next argument
					Inc(LineIndex);
				end
{				'=':
				begin
					for a := 0 to MemCount - 1 do
						if Mems[a].Name =
				end}
				else
				begin
					if CharsTable[Line[LineIndex]] = ctLetter then
					begin
						// Functions
						StartIndex := LineIndex;
						while (LineIndex <= Length(Line)) and (CharsTable[Line[LineIndex]] = ctLetter) do
							Inc(LineIndex);
						FceO := Copy(Line, StartIndex, LineIndex - StartIndex);
//						Inc(LineIndex);

						Fce := UpperCase(FceO);
						if Fce = 'PI' then
						begin
							AddArgument(pi); // 3,1415926535897932384626433832795
						end
						else if Fce = 'E' then
						begin
							AddArgument(ConstE);
						end
						else if Fce = 'C' then
						begin
							AddArgument(299999025);
						end
						else if (Fce = 'ZERO') or (Fce = 'FALSE') then
						begin
							AddArgument(0);
						end
						else if Fce = 'TRUE' then
						begin
							AddArgument(1);
						end
						else if (Fce = 'INFINITY') or (Fce = 'INFINITY') then
						begin
							AddArgument(Infinity);
						end
						else if (Fce = 'NEGINFINITY') or (Fce = 'NEGINFINITY') then
						begin
							AddArgument(NegInfinity);
						end
						else if (Fce = 'PLUS') or (Fce = 'SUM') then
						begin
							NextOperator := opPlus;
						end
						else if Fce = 'MINUS' then
						begin
							NextOperator := opMinus;
						end
						else if Fce = 'MUL' then
						begin
							NextOperator := opMul;
						end
						else if Fce = 'DIV' then
						begin
							NextOperator := opDiv;
						end
						else if Fce = 'ABS' then
						begin
							NextOperator := opAbs;
						end
						else if (Fce = 'NEG') then
						begin
							NextOperator := opNeg;
						end
						else if (Fce = 'INV') then
						begin
							NextOperator := opInv;
						end
						else if (Fce = 'NOT') then
						begin
							NextOperator := opNot;
						end
						else if (Fce = 'INC') then
						begin
							NextOperator := opInc;
						end
						else if (Fce = 'DEC') then
						begin
							NextOperator := opDec;
						end
						else if (Fce = 'FACT') then
						begin
							NextOperator := opFact;
						end
						else if Fce = 'POWER' then
						begin
							NextOperator := opPower;
						end
						else if Fce = 'EXP' then
						begin
							NextOperator := opExp;
						end
						else if Fce = 'LN' then
						begin
							NextOperator := opLn;
						end
						else if Fce = 'SQR' then
						begin
							NextOperator := opSqr;
						end
						else if Fce = 'SQRT' then
						begin
							NextOperator := opSqrt;
						end
						else if Fce = 'SIN' then
						begin
							NextOperator := opSin;
						end
						else if Fce = 'COS' then
						begin
							NextOperator := opCos;
						end
						else if Fce = 'TAN' then
						begin
							NextOperator := opTan;
						end
						else if Fce = 'ARCSIN' then
						begin
							NextOperator := opArcSin;
						end
						else if Fce = 'ARCCOS' then
						begin
							NextOperator := opArcCos;
						end
						else if Fce = 'ARCTAN' then
						begin
							NextOperator := opArcTan;
						end
						else if Fce = 'AVG' then
						begin
							NextOperator := opAvg;
						end
						else if Fce = 'AVG' then
						begin
							NextOperator := opAvg;
						end
						else
						begin
{							Found := False;
							for a := 0 to MemCount - 1 do
								if Fce = Mems[a].Name then
								begin
									AddArgument(Mems[a].Value);
									Found := True;
									Break;
								end;
							if Found = False then}
								ShowError('Invalid identifier ''' + FceO + '''');
						end;
					end
					else
					begin
						ShowError('Invalid operator ''' + Line[LineIndex] + '''');
						Inc(LineIndex);
					end;
				end;
				end;
			end;
			if LineIndex > MaxLineIndex then
			begin
				ShowError(''')'' missed');
				goto LFin;
			end;
		end;
		if Level > 0 then
			Dec(Level)
		else
			ShowError('Many '')'' chars');
	end;

label LNext;
var
	c1, c2: Integer;
	Ok1, Ok2: (okOperator, okOpen, okClose, okOther);
begin
{
	IntStr := ReadString(Section, Ident, '');
	if (Length(IntStr) > 2) and (IntStr[1] = '0') and
		((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
		IntStr := '$' + Copy(IntStr, 3, Maxint);
	Result := StrToIntDef(IntStr, Default);
}
	ErrorLineIndex := 0;
	Result := DefVal;
	ErrorMsg := '';
	LineIndex := 0;
	if Length(Line) <= 0 then
	begin
		ShowError('Line too short');
		Exit;
	end;
	if Length(Line) = 3 then
	begin
		if Pos(UpperCase(Line), 'DEF') <> 0 then Exit;
		if Pos(UpperCase(Line), 'MIN') <> 0 then
		begin
			Result := MinVal;
			Exit;
		end;
		if Pos(UpperCase(Line), 'MAX') <> 0 then
		begin
			Result := MaxVal;
			Exit;
		end;
	end;

	if UseWinFormat then
	begin
		DecimalSep := DecimalSeparator;
		ThousandSep := ThousandSeparator;
	end
	else
	begin
		DecimalSep := '.';
		ThousandSep := ',';
	end;

//	DelStr(Line, ThousandSep); // D??? '.'

	// Make ()
	if Line[1] <> '(' then
	begin
		Insert('(', Line, 1);
		Insert(')', Line, Length(Line) + 1);
	end;
	for i := 0 to 2 do
	begin
		LineIndex := 1;
		while LineIndex <= Length(Line) do
		begin
			if ((i = 2) and ((CharsTable[Line[LineIndex]] = ctPlus) or (CharsTable[Line[LineIndex]] = ctMinus)))
			or ((i = 1) and ((CharsTable[Line[LineIndex]] = ctMul) or (CharsTable[Line[LineIndex]] = ctDiv)))
			or ((i = 0) and ((CharsTable[Line[LineIndex]] = ctExp))) then
			begin
				Ok1 := okOperator;
				c1 := LineIndex;
				Level := 0;
				while True do
				begin
					Dec(c1);
					if c1 <= 0 then
					begin
						Ok1 := okOperator;
						Break;
					end;

					if Line[c1] = '(' then
					begin
						Inc(Level);
						if Level > 0 then
						begin
							Ok1 := okOpen;
							Break;
						end;
					end;
					if Line[c1] = ')' then Dec(Level);
					if Level = 0 then
					if (CharsTable[Line[c1]] >= ctPlus)
					and (CharsTable[Line[c1]] <= ctDiv) then
					begin
						Ok1 := okOperator;
						Inc(c1);
						Break;
					end;
				end;

				if Ok1 <> okOther then
				begin
					Ok2 := okOperator;
					c2 := LineIndex;
					Level := 0;
					while True do
					begin
						Inc(c2);
						if c2 > Length(Line) then
						begin
							Ok2 := okOperator;
							Break;
						end;

						if Line[c2] = '(' then Inc(Level);
						if Line[c2] = ')' then
						begin
							Dec(Level);
							if Level < 0 then
							begin
								Ok2 := okClose;
								Break;
							end;
						end;
						if Level = 0 then
						if (CharsTable[Line[c2]] >= ctPlus)
						and (CharsTable[Line[c2]] <= ctDiv) then
						begin
							Ok2 := okOperator;
							Break;
						end;
					end;

					if (Ok2 <> okOther) and ((Ok1 <> okOpen) or (Ok2 <> okClose)) then
					begin
						Insert('(', Line, c1);
						Insert(')', Line, c2 + 1);
						Inc(LineIndex);
						goto LNext;
					end;
				end;
			end;
			LNext:
			Inc(LineIndex);
		end;
	end;

	InStr := Line;

	Level := 0; MaxLevel := 0;
	LineIndex := 2; MaxLineIndex := Length(Line);
	Result := Make(opNone);
	if Level > 0 then ShowError('Incorect level');
	if LineIndex <= MaxLineIndex  then ShowError('Line too long, unused chars');

	if Result < MinVal then
	begin
		ShowError('Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal));
		Result := MinVal;
	end
	else if Result > MaxVal then
	begin
		ShowError('Value ' + FloatToStr(Result) + ' out of range ' + FloatToStr(MinVal) + '..' + FloatToStr(MaxVal));
		Result := MaxVal;
	end;
end;


function StrToValE(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended;
var ErrorMsg: string;
begin
	Result := StrToValE(Line, UseWinFormat, MinVal, DefVal, MaxVal, ErrorMsg);
end;

function StrToValE(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended;
var
	InStr: string;
	LineIndex: SG;
begin
	Result := StrToValExt(Line, UseWinFormat, MinVal, DefVal, MaxVal, ErrorMsg, InStr, LineIndex);
end;

function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer;
var ErrorMsg: string;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator, ErrorMsg);
end;

function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG;
var ErrorMsg: string;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator, ErrorMsg);
end;

function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: Integer; out ErrorMsg: string): Integer;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, ErrorMsg));
end;

function StrToValI(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; out ErrorMsg: string): UG;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, ErrorMsg));
end;

function StrToValS8(Line: TString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8): S8;
var ErrorMsg: string;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, ErrorMsg));
end;

function StrToValU1(Line: TString; const UseWinFormat: BG;
	const DefVal: U1): U1;
begin
	Result := StrToValI(Line, UseWinFormat, 0, UG(DefVal), 255, 1);
end;

const Sep = ' ';

function BToStr(const B: Integer): TString;
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

function BToStr(const B: Int64): TString;
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

procedure msToHMSD(const T: Int64; out GH, GM, GS, GD: LongWord);
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

function msToStr(const DT: Int64;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): TString;
begin
	Result := msToStr(DT, True, Display, Decimals, FixedWidth);
end;

function msToStr(const DT: Int64; const UseWinFormat: BG;
	const Display: TDisplay; const Decimals: ShortInt; FixedWidth: Boolean): TString;
var
	h, m, s, d: LongWord;
	Day: SG;

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

	msToHMSD(Abs(DT), h, m, s, d);

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
	diHMSD, diDHMSD:
	begin
		if h < 10 then
		begin
			if (DT >= 0) and FixedWidth then Result := Result + ' ';
			Result := Result + Chr(h + 48) + TimeSep;
		end
		else if h < 100 then
			Result := Result + Chr((h div 10) + 48) + Chr((h mod 10) + 48) + TimeSep
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
			Result := Result + Chr(h + 48) + TimeSep
		end
		else if h < 100 then
			Result := Result + Chr((h div 10) + 48) + Chr((h mod 10) + 48) + TimeSep
		else
			Result := Result + IntToStr(h) + TimeSep;
	end;
	end;

	if Display <> diSD then
		if m < 10 then
		begin
			if (h = 0) and (not (Display in [diHMSD, diDHMSD])) then
			begin
				if FixedWidth then Result := Result + ' ';
				Result := Result + Chr(m + 48) + TimeSep
			end
			else
				Result := Result + '0' + Chr(m + 48) + TimeSep;
		end
		else
		begin
			Result := Result + Chr((m div 10) + 48) + Chr((m mod 10) + 48) + TimeSep;
		end;

	if Display = diSD then
	begin
		Result := Result + IntToStr(3600 * h + 60 * m + s);
	end
	else
		if s < 10 then
		begin
			Result := Result + '0' + Chr(s + 48);
		end
		else
		begin
			Result := Result + Chr((s div 10) + 48) + Chr((s mod 10) + 48);
		end;

	case Abs(Decimals) of
	0: Exit;
	1:
	begin
		d := (d + 50) div 100;
		if (Decimals > 0) or (d <> 0) then
			Result := Result + DecimalSep + Chr(d + 48);
	end;
	2:
	begin
		d := (d + 5) div 10;
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

function Date6(Year, Month, Day: Word): string;
begin
	Result := NToS(Year, '00') + NToS(Month, '00') + NToS(Day, '00')
end;

function Date6Now: string;
var
	Year, Month, Day: Word;
begin
	DecodeDate(Date, Year, Month, Day);
	Result := Date6(Year, Month, Day);
end;

procedure Nop;
begin
	asm
	nop
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

function GetSingleCaption(const FName: TFileName; const Changed: Boolean;
	const New: Boolean): string;
begin
	Result := Application.Title + ' - ' + FName;
	if Changed then Result := Result + ' *';
	if New then Result := Result + ' (New)';
end;

function GetMultiCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const Index, Count: Integer): string;
begin
	Result := Application.Title + ' - (' + NToS(Index + 1) + '/' +
		NToS(Count) + ') ' + FName;
	if Changed then Result := Result + ' *';
	if New <> 0 then Result := Result + ' (New)';
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

{	Index := Pos('BUT', UpperCase(Result));
	Count := 3;
	if Index <> 0 then goto LDel;}

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

procedure FillCharsTable;
var c: Char;
begin
	// Make Char Table
	for c := Low(Char) to High(Char) do
		case c of
		' ': CharsTable[c] := ctSpace;
		'a'..'z', 'A'..'Z', '_': CharsTable[c] := ctLetter;
		'0'..'9', '!', '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharsTable[c] := ctNumber;
		'+': CharsTable[c] := ctPlus;
		'-': CharsTable[c] := ctMinus;
		'^': CharsTable[c] := ctExp;
		'*': CharsTable[c] := ctMul;
		'/': CharsTable[c] := ctDiv;
		'(': CharsTable[c] := ctOpen;
		')': CharsTable[c] := ctClose;
		'.', ',': CharsTable[c] := ctNumber;
		else
			if (c = DecimalSeparator[1]) or (c = ThousandSeparator[1]) then
				CharsTable[c] := ctNumber
			else
				CharsTable[c] := ctIllegal;
		end;
end;

procedure ObjectFree(var Obj: TObject);
begin
	Obj.Free; Obj := nil;
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
	FillCharsTable;
	GetLocale;
end.
