// Build: 01/1998-09/1999 Author: Safranek David

unit uAdd;

interface

uses SysUtils, Forms, FileCtrl, ShlObj, ActiveX, ComObj, ComCtrls;

type
	SG = Integer; // LongInt
	UG = Cardinal; // LongWord
	S8 = ShortInt;
	U8 = Byte;
	S16 = SmallInt;
	U16 = Word;
	S32 = LongInt;
	U32 = LongWord;
	S64 = Int64;
	U64 = Int64; // Wor64/Car64
	TS16 = record
		case Integer of
		0: (
				B0: U8;
				B1: S8);
		1: (
				A: S16);
	end;
	TU16 = record
		case Integer of
		0: (
				B0: U8;
				B1: U8);
		1: (
				A: U16);
	end;
	TS32 = record
		case Integer of
		0: (
				B0: U8;
				B1: U8;
				B2: U8;
				B3: S8);
		1: (
				W0: U16;
				W1: S16);
		2: (
			A: S32);
	end;
	TU32 = record
		case Integer of
		0: (
				B0: U8;
				B1: U8;
				B2: U8;
				B3: U8);
		1: (
				W0: U16;
				W1: U16);
		2: (
			A: U32);
	end;
	TS64 = record
		case Integer of
		0: (
				B0: U8;
				B1: U8;
				B2: U8;
				B3: U8;
				B4: U8;
				B5: U8;
				B6: U8;
				B7: S8);
		1: (
				W0: U16;
				W1: U16;
				W2: U16;
				W3: S16);
		2: (
			D0: U32;
			D1: S32);
		3: (
			A: U64);
	end;
	TU64 = record
		case Integer of
		0: (
				B0: U8;
				B1: U8;
				B2: U8;
				B3: U8;
				B4: U8;
				B5: U8;
				B6: U8;
				B7: U8);
		1: (
				W0: U16;
				W1: U16;
				W2: U16;
				W3: U16);
		2: (
			D0: U32;
			D1: U32);
		3: (
			A: U64);
	end;
{	S64 = record
		case Integer of
		0: (
			LowPart: U32;
			HighPart: S32);
		1: (
			QuadPart: Int64);
	end; overload;
	U64 = record
		case Integer of
		0: (
			LowPart: U32;
			HighPart: U32);
		1: (
			QuadPart: Int64);
	end;}

	FG = Real; // Double
	F32 = Single;
	F48 = Real48;
	F64 = Double;
	F80 = Extended;

{	CG = Char; // AnsiChar
	C8 = AnsiChar;
	C16 = WideChar;

	TG = string; // AnsiString
	TA8 = ShortString;
	T8 = AnsiString;
	T16 = WideString;}

	BG = Boolean; // ByteBool
	B8 = ByteBool;
	B16 = WordBool;
	B32 = LongBool;

	TArrayByte = array[0..1024 * 1024 * 1024 - 1] of Byte;
	TArrayChar = array[0..1024 * 1024 * 1024 - 1] of AnsiChar;
	PArrayByte = ^TArrayByte;
	PArrayChar = ^TArrayChar;

	TString = {$ifdef DLL}ShortString{$else}string{$endif};

// Graphics
	TRColor = packed record
		case Integer of
		0: (L: -$7FFFFFFF - 1..$7FFFFFFF);
		1: (R, G, B, T: Byte);
		2: (WordRG, WordBT: Word);
	end;

const
	DefMemBuffer = 4096; // Best Performance
	MaxInt64 = Int64($7FFFFFFFFFFFFFFF);
// Mathematics
function Sgn(const I: Integer): Integer; overload; {$ifdef DLL}stdcall;{$endif}
function Sgn(const I: Int64): Integer; overload; {$ifdef DLL}stdcall;{$endif}
function Sgn(const I: Single): Integer; overload; {$ifdef DLL}stdcall;{$endif}
function Sgn(const I: Double): Integer; overload; {$ifdef DLL}stdcall;{$endif}
function Sgn(const I: Extended): Integer; overload; {$ifdef DLL}stdcall;{$endif}
function SgnMul(const Signum, Num: Integer): Integer; {$ifdef DLL}stdcall;{$endif}

procedure DivModU32(const Dividend: LongWord; const Divisor: Word;
	var Res, Remainder: Word);
procedure DivModS32(const Dividend: LongInt; const Divisor: SmallInt;
	var Res, Remainder: SmallInt);
procedure DivModU64(const Dividend: Int64; const Divisor: LongWord;
	var Res, Remainder: LongWord);
procedure DivModS64(const Dividend: Int64; const Divisor: LongInt;
	var Res, Remainder: LongInt);

function UnsignedMod(const Dividend: Int64; const Divisor: Integer): Integer;
function LinearMax(Clock, Maximum: LongWord): LongWord;

function RoundDiv(const Dividend: Integer; const Divisor: Integer): Integer; //overload;
function RoundDiv64(const Dividend: Int64; const Divisor: Int64): Int64; //overload;
function MaxDiv(const Dividend: Integer; const Divisor: Integer): Integer; //overload;
function MaxDiv64(const Dividend: Int64; const Divisor: Integer): Int64; //overload;

function Range(const Min, Cur, Max: Integer): Integer; overload;
function Range(const Min, Cur, Max: Cardinal): Cardinal; overload;

procedure Change(var A, B: Integer); overload;

procedure CheckBool(var Bool: ByteBool); overload;
procedure CheckBool(var Bool: WordBool); overload;
procedure CheckBool(var Bool: LongBool); overload;

procedure Order(var I1, I2: Integer); overload;
procedure Order(var I1, I2: Cardinal); overload;

function CalcShr(N: U32): S8;

function AllocBy(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;

// Format functions
var WinDecimalSeparator: Char;
function Using(const Typ: TString; const Num: Int64): TString;  {$ifdef DLL}stdcall;{$endif}
function StrToValE(Line: TString;
	const MinVal, DefVal, MaxVal: Extended): Extended; overload;
function StrToValE(Line: TString;
	const MinVal, DefVal, MaxVal: Extended; var ErrorMsg: string): Extended; overload;
function StrToValI(Line: TString;
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer; overload;
function StrToValI(Line: TString;
	const MinVal, DefVal, MaxVal, Denominator: Integer; var ErrorMsg: string): Integer; overload;
function StrToValI64(Line: TString;
	const MinVal, DefVal, MaxVal, Denominator: Int64): Int64;
(*
function StrToValC(S: TString;
	const MinVal, DefVal, MaxVal, Denominator: Cardinal): Cardinal; {overload;} {$ifdef DLL}stdcall;{$endif}
function StrToValE(S: TString;
	const MinVal, DefVal, MaxVal: Extended): Extended; {overload;} {$ifdef DLL}stdcall;{$endif}
*)
function BToStr(const B: Integer): TString; overload; {$ifdef DLL}stdcall;{$endif}
function BToStr(const B: Int64): TString; overload; {$ifdef DLL}stdcall;{$endif}

// Time
procedure msToHMSD(const T: Int64; var GH, GM, GS, GD: LongWord);  {$ifdef DLL}stdcall;{$endif}
type
	TDisplay = (diHMSD, diMSD, diSD);
function msToStr(const DT: Int64;
	const Display: TDisplay; const Decimals: ShortInt): TString;  {$ifdef DLL}stdcall;{$endif}

// System
procedure Beep;
procedure SndWarn;
function DriveTypeToStr(const DriveType: Integer): TString;
function ProcessPriority(const Prior: Byte): Integer;  {$ifdef DLL}stdcall;{$endif}
function ThreadPriority(const Prior: Byte): Integer;  {$ifdef DLL}stdcall;{$endif}

function GetSingleCaption(const FName: TFileName; const Changed: Boolean;
	const New: Boolean): string;

function GetMultiCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const Index, Count: Integer): string;

function MenuNameToFileName(Name: string): string;
function ButtonNameToFileName(Name: string): string;

procedure CorrectPos(Form: TForm);
procedure SetListViewItems(ListView: TListView; NewSize: SG);
procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: Word;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);

implementation

uses
	Windows, Math,
	uError;

function Sgn(const I: Integer): Integer;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: Int64): Integer;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: Single): Integer;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: Double): Integer;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;

function Sgn(const I: Extended): Integer;
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
	if Dividend >= 0 then
		Result := Dividend mod Divisor
	else
	begin
		Result := Dividend + Divisor * (Abs(Dividend - Divisor + 1) div Divisor);
	end;
end;

function LinearMax(Clock, Maximum: LongWord): LongWord;
begin
	Result := Clock mod (2 * Maximum);
	if Result > Maximum then Result := 2 * Maximum - Result;
end;

function RoundDiv(const Dividend: Integer; const Divisor: Integer): Integer;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	if Dividend < 0 then
		Result := (Dividend - (Divisor div 2)) div Divisor
	else
		Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function RoundDiv64(const Dividend: Int64; const Divisor: Int64): Int64;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	if Dividend < 0 then
		Result := (Dividend - (Divisor div 2)) div Divisor
	else
		Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function MaxDiv(const Dividend: Integer; const Divisor: Integer): Integer;
// 0 div 4 is 0
// 1 div 4 is 1
// 2 div 4 is 1
// 3 div 4 is 1
begin
	if Dividend < 0 then
		Result := (Dividend - Divisor + 1) div Divisor
	else
		Result := (Dividend + Divisor - 1) div Divisor;
end;

function MaxDiv64(const Dividend: Int64; const Divisor: Integer): Int64;
// 0 div 4 is 0
// 1 div 4 is 1
// 2 div 4 is 1
// 3 div 4 is 1
begin
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

procedure Change(var A, B: Integer); overload;
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

procedure CheckBool(var Bool: ByteBool); overload;
begin
	Bool := ByteBool(Byte(Bool) and 1);
end;

procedure CheckBool(var Bool: WordBool); overload;
begin
	Bool := WordBool(Word(Bool) and 1);
end;

procedure CheckBool(var Bool: LongBool); overload;
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

function CalcShr(N: U32): S8;
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
var M: U32;
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

function AllocBy(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): Boolean;
{
	OldSize = <0, 2^31)
	NewSize = <0, 2^31)
	BlockSize = 2^n, <2, 2^30>
}
begin
	if (1 shl CalcShr(BlockSize)) <> BlockSize then
	begin
		ErrorMessage('Bad AllocBy block size ' + IntToStr(BlockSize));
		BlockSize := 1 shl CalcShr(BlockSize);
	end;

	if NewSize > OldSize then
		NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1)
	else
		if NewSize + BlockSize + BlockSize div 2 < OldSize then
			NewSize := (NewSize + BlockSize - 1) and ($7fffffff - BlockSize + 1)
		else
			NewSize := OldSize;

	Result := OldSize <> NewSize;
end;

function Using(const Typ: TString; const Num: Int64): TString;
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
	if Typ = '' then
	begin
		Exit;
	end;

	Poin := Pos('.', Typ);
	if Typ[Length(Typ)] = '~' then DelSpace := True else DelSpace := False;

	if Num = 0 then inp := '' else inp := IntToStr(Abs(Num));
	inpP := Length(inp);

	FixedSign := False;
	for i := 1 to Length(typ) do
	begin
		if (typ[i] = '+') or (typ[i] = '-') then
		begin
			FixedSign := True;
			Break;
		end;
	end;

	Fra := False;
	for i := Length(typ) downto 1 do
	begin
		case typ[i] of
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
					if Result[1] = ' ' then Delete(Result, 1, 1) else Exit;
				end;
				Exit;
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
				Result := DecimalSeparator + Result;
			end;
			if Typ[1] = '~' then DelSpace := True else DelSpace := False;
		end;
		' ':
		begin
			Result := ' ' + Result;
		end;
		',':
		begin
				if (inpP > 0) then
				begin
					Result := ThousandSeparator + Result;
				end
				else
				begin
					if DelSpace = False then Result := ' ' + Result;
				end;
		end;
		end;
	end;
end;

var
	CharsTable: array[Char] of (ctIllegal, ctNumber, ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose);

function StrToValE(Line: TString;
	const MinVal, DefVal, MaxVal: Extended): Extended; overload;
var ErrorMsg: string;
begin
	Result := StrToValE(Line, MinVal, DefVal, MaxVal, ErrorMsg);
end;

function StrToValE(Line: TString;
	const MinVal, DefVal, MaxVal: Extended; var ErrorMsg: string): Extended;
type
	TOperator = (opNone, opWaitOperator, opNumber, opPlus, opExp, opMinus, opMul, opDiv);
var
	i: Integer;
	Per, Point: Boolean;
	PointDiv: Extended;
	Num: Integer;
	Base: Integer;

	MaxLineIndex: Integer;
	LineIndex: Integer;
	LastLineIndex: Integer;

	Res, Exp: Extended;
	Level, MaxLevel: Integer;
	Where: (whNum, whExp);

	procedure ShowError(s: string);
	begin
		if ErrorMsg = '' then
			ErrorMsg := 'Error: (' + IntToStr(LineIndex) + '): ' + s;
	end;

	function Make: Extended;
	label LFin, LNext;
	var
		R1, R2: Extended;
		LastOperator: TOperator;
		Unar, UnarExp: Boolean;
	begin
		Result := 0;
		if Level >= 255 then Exit;
		Inc(Level); if Level > MaxLevel then MaxLevel := Level;
		Unar := False;
		UnarExp := False;
		R1 := 0;
		R2 := 0;
		LastOperator := opNone;

		while True do
		begin
			if (LineIndex > Length(Line)) then Break;
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
				while LineIndex < Length(Line) do
				begin
					case Line[LineIndex] of
					'%': Per := True;
					'#': Base := 2;
					'O', 'o': Base := 8;
					'!': Base := 10;
					'$', 'x', 'X', 'h', 'H': Base := 16;
					'.': Point := True;
					'*', '/', ':', '^', ')', '(': Break;
					'-', '+': if (Base <> 10) or (UpCase(Line[LineIndex - 1]) <> 'E') then Break else UnarExp := True;
					else
					begin
						if Line[LineIndex] = DecimalSeparator then
							Point := True
						else
						begin
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
							end;
							end;
							case UpCase(Line[LineIndex]) of
							'0'..'9', 'A'..'F':
							begin
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
							end;
							end;
						end;
					end;
					end;
					LNext:
					Inc(LineIndex);
				end;

				if Per then Res := Res * MaxVal / 100;
				if UnarExp then Exp := -Exp;
				if Abs(Exp) > 1024 then Exp := Sgn(Exp) * 1024;
				Res := Res * Power(10, Exp);
				if Unar then Res := -Res;

//        Val(Copy(Line, LastLineIndex, LineIndex - LastLineIndex), Res, ErrorCode);
				if LastOperator = opNone then
				begin
					R1 := Res;
					LastOperator := opWaitOperator;
				end
				else
					R2 := Res;
			end
			else
			case Line[LineIndex] of
			'(':
			begin
				if LastOperator = opWaitOperator then ShowError('Operator required');
				Inc(LineIndex);
				Res := Make;
				if Unar then Res := -Res;
				if LastOperator = opNone then
				begin
					R1 := Res;
					LastOperator := opWaitOperator;
				end
				else
					R2 := Res;
			end;
			')':
			begin
				LFin:
				case LastOperator of
				opNone:
				begin
					ShowError('Number required');
					Result := 0;
				end;
				opWaitOperator:
				begin
//          ShowError('Unary operation');
					Result := R1;
				end;
				opPlus: Result := R1 + R2;
				opMinus: Result := R1 - R2;
				opMul: Result := R1 * R2;
				opDiv:
				begin
					if R2 = 0 then
					begin
						ShowError('Division by zero');
						Result := R1;
					end
					else
						Result := R1 / R2;
				end;
				opExp:
				begin
					Result := Power(R1, R2);
{         Result := 1;
					i := 1;
					while  i <= R2 do
					begin
						Result := Result * R1;
						Inc(i);
					end;}
				end;
				end;
				Inc(LineIndex);
				Break;
			end;
			'+':
			begin
				if LastOperator = opWaitOperator then
					LastOperator := opPlus;
				Inc(LineIndex);
			end;
			'-':
			begin
{       if LastOperator = opWaitOperator then
				begin
					if (Line[LineIndex - 1] = 'E') then
						UnarExp := not UnarExp
					else
						LastOperator := opMinus
				end
				else
					Unar := not Unar;}
				if LastOperator = opWaitOperator then
				begin
					LastOperator := opMinus
				end
				else
					Unar := not Unar;
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
				LastOperator := opExp;
				Inc(LineIndex);
			end;
			else
			begin
				ShowError('Invalid operator ''' + Line[LineIndex] + '''');
				Inc(LineIndex);
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

	Result := DefVal;
	ErrorMsg := '';
	LineIndex := 0;
	if Length(Line) <= 0 then
	begin
		ShowError('Line too short');
		Exit;
	end;
	if Pos(UpperCase(Line), 'DEF') <> 0 then Exit;
	if Pos(UpperCase(Line), 'MIN') <> 0 then
	begin
		Result := MinVal;
		Exit;
	end;
	if Pos(UpperCase(Line), 'MAX') <> 0 then
	begin
		ShowError('Line too short');
		Result := MaxVal;
		Exit;
	end;

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

	Level := 0; MaxLevel := 0;
	LineIndex := 2; MaxLineIndex := Length(Line);
	Result := Make;
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

function StrToValI(Line: TString;
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer;
var ErrorMsg: string;
begin
	Result := StrToValI(Line, MinVal, DefVal, MaxVal, Denominator, ErrorMsg);
end;

function StrToValI(Line: TString;
	const MinVal, DefVal, MaxVal, Denominator: Integer; var ErrorMsg: string): Integer;
begin
	Result := Round(Denominator * StrToValE(Line, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, ErrorMsg));
end;

function StrToValI64(Line: TString;
	const MinVal, DefVal, MaxVal, Denominator: Int64): Int64;
var ErrorMsg: string;
begin
	Result := Round(Denominator * StrToValE(Line, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, ErrorMsg));
end;

const Sep = ' ';

function BToStr(const B: Integer): TString;
label LExit;
begin
	if B < 1024 then //2^10 ($400)
	begin
		Result := Using('~###0', B) + Sep + 'B';
		goto LExit;
	end;
	if B < 10240 then
	begin
		Result := Using('0.##~', (100 * B) div 1024) + Sep + 'KB';
		goto LExit;
	end;
	if B < 102400 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1024) + Sep + 'KB';
		goto LExit;
	end;
	if B < 1048576 then //2^20 ($100 000)
	begin
		Result := Using('~###0', B div 1024) + Sep + 'KB';
		goto LExit;
	end;
	if B < 10485760 then
	begin
		Result := Using('0.##~', (100 * B) div 1048576) + Sep + 'MB';
		goto LExit;
	end;
	if B < 104857600 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1048576) + Sep + 'MB';
		goto LExit;
	end;
	if B < 1073741824 then //2^30 ($40 000 000)
	begin
		Result := Using('~###0', B div 1048576) + Sep + 'MB';
		goto LExit;
	end;
	//if B<10737418240 then
	Result := Using('0.##~', (100 * (B div 128)) div (1073741824 div 128)) + Sep + 'GB';
	LExit:
	if B < 0 then Result := '-' + Result;
end;

function BToStr(const B: Int64): TString;
label LExit;
begin
	if B < 1024 then //2^10 ($400)
	begin
		Result := Using('~###0', B) + Sep + 'B';
		goto LExit;
	end;
	if B < 10240 then
	begin
		Result := Using('0.##~', (100 * B) div 1024) + Sep + 'KB'; //Kilo
		goto LExit;
	end;
	if B < 102400 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1024) + Sep + 'KB';
		goto LExit;
	end;
	if B < 1048576 then //2^20 ($100 000)
	begin
		Result := Using('~###0', B div 1024) + Sep + 'KB';
		goto LExit;
	end;
	if B < 10485760 then
	begin
		Result := Using('0.##~', (100 * B) div 1048576) + Sep + 'MB'; //Mega
		goto LExit;
	end;
	if B < 104857600 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1048576) + Sep + 'MB';
		goto LExit;
	end;
	if B < 1073741824 then //2^30 ($40 000 000)
	begin
		Result := Using('~###0', B div 1048576) + Sep + 'MB';
		goto LExit;
	end;
	if B < 10737418240 then
	begin
		Result := Using('0.##~', (100 * B) div 1073741824) + Sep + 'GB'; //Giga
		goto LExit;
	end;
	if B < 107374182400 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1073741824) + Sep + 'GB';
		goto LExit;
	end;
	if B < 1099511627776 then //2^40 ($10 000 000 000)
	begin
		Result := Using('~###0', B div 1073741824) + Sep + 'GB';
		goto LExit;
	end;
	if B < 10995116277760 then
	begin
		Result := Using('0.##~', (100 * B) div 1099511627776) + Sep + 'TB'; //Tera
		goto LExit;
	end;
	if B < 109951162777600 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1099511627776) + Sep + 'TB';
		goto LExit;
	end;
	if B < 1125899906842624 then //2^50 ($4 000 000 000 000)
	begin
		Result := Using('~###0', B div 1099511627776) + Sep + 'TB';
		goto LExit;
	end;
	if B < 11258999068426240 then
	begin;
		Result := Using('0.##~', (100 * B) div 1125899906842624) + Sep + 'PB'; //Peta
		goto LExit;
	end;
	if B < 112589990684262400 then
	begin
		Result := Using('~#0.#~', (10 * B) div 1125899906842624) + Sep + 'PB';
		goto LExit;
	end;
	if B < 1152921504606846976 then //2^60 ($1 000 000 000 000 000)
	begin
		Result := Using('~###0', B div 1125899906842624) + Sep + 'PB';
		goto LExit;
	end;
	//if B<11529215046068469760 then
	Result := Using('0.##~', (100 * (B div 128)) div (1152921504606846976 div 128)) + Sep + 'EB'; //Exa
	LExit:
	if B < 0 then Result := '-' + Result;
end;

procedure msToHMSD(const T: Int64; var GH, GM, GS, GD: LongWord);
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
	const Display: TDisplay; const Decimals: ShortInt): TString;
var
	h, m, s, d: LongWord;
begin
	msToHMSD(Abs(DT), h, m, s, d);

	if DT < 0 then Result := '-' else Result := '';

	case Display of
	diHMSD:
	begin
		if h < 10 then
		begin
			if DT >= 0 then Result := Result + ' ';
			Result := Result + Chr(h + 48) + ':';
		end
		else if h < 100 then
			Result := Result + Chr((h div 10) + 48) + Chr((h mod 10) + 48) + ':'
		else
			Result := Result + IntToStr(h) + ':';
	end;
	diMSD: 
	begin
		if h = 0 then
			Result := Result + '   '
		else if h < 10 then
			Result := Result + ' ' + Chr(h + 48) + ':'
		else if h < 100 then
			Result := Result + Chr((h div 10) + 48) + Chr((h mod 10) + 48) + ':'
		else
			Result := Result + IntToStr(h) + ':';
	end;
	end;

	if Display <> diSD then
		if m < 10 then
		begin
			if (h = 0) and (Display <> diHMSD) then
				Result := Result + ' ' + Chr(m + 48) + ':'
			else
				Result := Result + '0' + Chr(m + 48) + ':';
		end
		else
		begin
			Result := Result + Chr((m div 10) + 48) + Chr((m mod 10) + 48) + ':';
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
			Result := Result + '.' + Chr(d + 48);
	end;
	2: 
	begin
		d := (d + 5) div 10;
		if (Decimals > 0) then
			Result := Result + Using('.00', d)
		else
			Result := Result + Using('.##', d);
	end;
	3:
	begin
		if (Decimals > 0) then
			Result := Result + Using('.000', d)
		else
			Result := Result + Using('.###', d);
	end;
	end;
end;

procedure Beep;
begin
	Windows.Beep(0, 0);
end;

procedure SndWarn;
begin
	Windows.Beep(0, 0);
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
	Result := Application.Title + ' - (' + IntToStr(Index + 1) + '/' +
		IntToStr(Count) + ') ' + FName;
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

function ButtonNameToFileName(Name: string): string;
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
	Index := Pos('DITBTN', UpperCase(Result));
	Count := 7;
	if Index <> 0 then goto LDel;

	Index := Pos('BITBTN', UpperCase(Result));
	Count := 6;
	if Index <> 0 then goto LDel;

	Index := Pos('BUTTON', UpperCase(Result));
	Count := 6;
	if Index <> 0 then goto LDel;

	Exit;
	LDel:
		Delete(Result, Index, Count);
end;

procedure CorrectPos(Form: TForm);
begin
	if not Assigned(Form) then Exit;
	if Form.Left + Form.Width > Screen.Width then Form.Left := Screen.Width - Form.Width;
	if Form.Top + Form.Height > Screen.Height then Form.Top := Screen.Height - Form.Height;
	if Form.Left < 0 then Form.Left := 0;
	if Form.Top < 0 then Form.Top := 0;
end;

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
		'0'..'9', '.', '!', '#', '$', '%', 'a'..'z', 'A'..'Z', ' ': CharsTable[c] := ctNumber;
		'+': CharsTable[c] := ctPlus;
		'-': CharsTable[c] := ctMinus;
		'^': CharsTable[c] := ctExp;
		'*': CharsTable[c] := ctMul;
		'/': CharsTable[c] := ctDiv;
		'(': CharsTable[c] := ctOpen;
		')': CharsTable[c] := ctClose;
		else
			if c = DecimalSeparator then
				CharsTable[c] := ctNumber
			else
				CharsTable[c] := ctIllegal;
		end;
end;

initialization
	FillCharsTable;
	WinDecimalSeparator := DecimalSeparator;
end.
