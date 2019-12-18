unit uMath;

{$IF defined(CPUX86) and defined(ASSEMBLER)}
  // Can use 32 bit assembler
  {$DEFINE X86ASMRTL}
{$ENDIF}

{$if SizeOf(Extended) > SizeOf(Double)}
  {$DEFINE HasExtended}
{$endif}

interface

uses
  uTypes,
  Types;

const
	SinDiv = 32768; // 1.0 65536; // 1..128..MB
type
	PAngle = ^TAngle;
	TAngle = S2;
	PSinTable = ^TSinTable;
	TSinTable = array[0..32767] of TAngle;

procedure Increment(var X: F4; const N: F4 = 1); overload;
procedure Increment(var X: F8; const N: F8 = 1); overload;
{$ifdef HasExtended}
procedure Increment(var X: FA; const N: FA = 1); overload;
{$endif}

procedure Decrement(var X: F4; const N: F4 = 1); overload;
procedure Decrement(var X: F8; const N: F8 = 1); overload;
{$ifdef HasExtended}
procedure Decrement(var X: FA; const N: FA = 1); overload;
{$endif}

procedure Multiply(var X: U1; const N: U1); overload;
procedure Multiply(var X: U2; const N: U2); overload;
procedure Multiply(var X: U4; const N: U4); overload;
procedure Multiply(var X: U8; const N: U8); overload;
{$ifdef CPUX64}
procedure Multiply(var X: UG; const N: UG); overload;
{$endif}

procedure Multiply(var X: S1; const N: S1); overload;
procedure Multiply(var X: S2; const N: S2); overload;
procedure Multiply(var X: S4; const N: S4); overload;
procedure Multiply(var X: S8; const N: S8); overload;
{$ifdef CPUX64}
procedure Multiply(var X: SG; const N: SG); overload;
{$endif}

procedure Multiply(var X: F4; const N: F4); overload;
procedure Multiply(var X: F8; const N: F8); overload;
{$ifdef HasExtended}
procedure Multiply(var X: FA; const N: FA); overload;
{$endif}

procedure Divide(var X: F4; const N: F4); overload;
procedure Divide(var X: F8; const N: F8); overload;
{$ifdef HasExtended}
procedure Divide(var X: FA; const N: FA); overload;
{$endif}

function Sgn(const I: S1): SG; overload;
function Sgn(const I: S2): SG; overload;
function Sgn(const I: S4): SG; overload;
function Sgn(const I: S8): SG; overload;
function Sgn(const I: F4): SG; overload;
function Sgn(const I: F8): SG; overload;
{$ifdef HasExtended}
function Sgn(const I: FA): SG; overload;
{$endif}
function SgnMul(const Signum, Num: SG): SG; overload;
function SgnMul(const Signum, Num: FG): FG; overload;
function AbsMin(const A, B: SG): SG;

// Result := Value1 * Value2;
procedure Multiply(const ValueA, ValueB: U8; out HighResult, LowResult: U8); overload;

// Result := (Value1 * Value2) shr 64;
function MultiplyAndReturnMostSignificantHalf(const ValueA, ValueB: U8): U8;

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
procedure UnsignedDivMod10(const Dividend: U4; out Result: U4; out Reminder: U4);
function UnsignedMod(const Dividend: S8; const Divisor: SG): SG;
function ModE(x, y: FM): FM;

function GetAbsoluteError(const A, B: FG): FG; overload;
{$ifdef HasExtended}
function GetAbsoluteError(const A, B: FA): FA; overload;
{$endif}
function GetRelativeError(const A, B: FG): FG; overload;
{$ifdef HasExtended}
function GetRelativeError(const A, B: FA): FA; overload;
{$endif}
function EqualRelative(const A, B, MaxRelativeError: FG): BG; overload;
{$ifdef HasExtended}
function EqualRelative(const A, B, MaxRelativeError: FA): BG; overload;
{$endif}
function EqualAbsolute(const A, B, MaxAbsoluteError: FG): BG; overload;
{$ifdef HasExtended}
function EqualAbsolute(const A, B, MaxAbsoluteError: FA): BG; overload;
{$endif}
function Factorial(const AValue: SG): SG;
function FastSqrt(A: UG): UG;
function LinearMax(Clock, Maximum: UG): UG;
procedure Rotate(var X, Y: SG; MaxX, MaxY: SG; Angle: SG); overload;
procedure Rotate(var X, Y: Double; MaxX, MaxY: Double; Angle: SG); overload;

function RoundN(const X: FG): S8; overload;
{$ifdef HasExtended}
function RoundN(const X: FA): S8; overload;
{$endif}

function RoundSG(Value: F4): SG; overload;
function RoundSG(Value: F8): SG; overload;
{$ifdef HasExtended}
function RoundSG(Value: FA): SG; overload;
{$endif}
function RoundS8(Value: FG): S8; overload;
{$ifdef HasExtended}
function RoundS8(Value: FA): S8; overload;
{$endif}
function RoundU8(Value: FG): U8; overload;
{$ifdef HasExtended}
function RoundU8(Value: FA): U8; overload;
{$endif}
function TruncS8(Value: FG): S8; overload;
{$ifdef HasExtended}
function TruncS8(Value: FA): S8; overload;
{$endif}
function RangeS8(Value: FG): BG; overload;
{$ifdef HasExtended}
function RangeS8(Value: FA): BG; overload;
{$endif}

function RoundDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function RoundDivU8(const Dividend: U8; const Divisor: U8): U8; //overload;
function RoundDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;
function MaxDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function MaxDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;
function RandomDiv(const Dividend: SG; const Divisor: SG): SG;

function IsInRange(const Min, Cur, Max: S1): BG; overload;
function IsInRange(const Min, Cur, Max: U1): BG; overload;
function IsInRange(const Min, Cur, Max: S2): BG; overload;
function IsInRange(const Min, Cur, Max: U2): BG; overload;
function IsInRange(const Min, Cur, Max: S4): BG; overload;
function IsInRange(const Min, Cur, Max: U4): BG; overload;
function IsInRange(const Min, Cur, Max: S8): BG; overload;
function IsInRange(const Min, Cur, Max: U8): BG; overload;
function IsInRange(const Min, Cur, Max: F4): BG; overload;
function IsInRange(const Min, Cur, Max: F8): BG; overload;
{$ifdef HasExtended}
function IsInRange(const Min, Cur, Max: FA): BG; overload;
{$endif}
{$ifdef MSWINDOWS}
function IsInRange(const Min, Cur, Max: AnsiChar): BG; overload;
{$endif}
function IsInRange(const Min, Cur, Max: WideChar): BG; overload;

function Range(const Min, Cur, Max: SG): SG; overload;
function Range(const Min, Cur, Max, Def: SG): SG; overload;
function Range(const Min, Cur, Max: UG): UG; overload;
function Range(const Min, Cur, Max: FG): FG; overload;
function RangeOverflow(const Min, Cur, Max: SG): SG; overload;

function IsInTheMiddle(const AValue: SG; const AMaximalValue: SG; const AMaximalWidth: SG): BG;

procedure Exchange(var A, B: B1); register; overload;
procedure Exchange(var A, B: B4); register; overload;
procedure Exchange(var A, B: U1); register; overload;
procedure Exchange(var A, B: S1); register; overload;
procedure Exchange(var A, B: U2); register; overload;
procedure Exchange(var A, B: S2); register; overload;
procedure Exchange(var A, B: U4); register; overload;
procedure Exchange(var A, B: S4); register; overload;
procedure Exchange(var A, B: S8); register; overload;
{$if CompilerVersion >= 23}
procedure Exchange(var A, B: NativeInt); register; overload;
procedure Exchange(var A, B: NativeUInt); register; overload;
{$endif}
procedure Exchange(var A, B: F4); register; overload;
procedure Exchange(var A, B: F8); register; overload;
{$ifdef HasExtended}
procedure Exchange(var A, B: FA); register; overload;
{$endif}
procedure Exchange(var A, B: Pointer); register; overload;
procedure Exchange(var P0, P1; Count: UG); register; overload;
procedure Exchange(P0, P1: Pointer; Count: UG); register; overload;
procedure Exchange(var s0, s1: string); overload;
procedure Exchange(var A, B: TObject); overload;

function Arg(X, Y: FM): FM; overload;

procedure CheckBool(var Bool: ByteBool); overload;
procedure CheckBool(var Bool: WordBool); overload;
procedure CheckBool(var Bool: LongBool); overload;

{$ifdef CPUX64}
procedure Order(var I1, I2: S4); overload;
{$endif}
procedure Order(var I1, I2: SG); overload;
procedure Order(var I1, I2: UG); overload;
procedure FillSinTable(Sins: PSinTable; const AngleCount, SinDiv: SG);

procedure ReadMem(P: Pointer; Size: UG);
function SameData(P0, P1: Pointer; Size: UG): BG;

procedure ClearMemory(var AAddress; const ACount: UG);

procedure FillMemory(var Desc; Count: UG; Value: S1); overload;
procedure FillMemory(var Desc; Count: UG; Value: S2); overload;
procedure FillMemory(var Desc; Count: UG; Value: S4); overload;
procedure FillMemory(var Desc; Count: UG; Value: S8); overload;
procedure FillMemory(var Desc; Count: UG; Value: U1); overload;
procedure FillMemory(var Desc; Count: UG; Value: U2); overload;
procedure FillMemory(var Desc; Count: UG; Value: U4); overload;
procedure FillMemory(var Desc; Count: UG; Value: U8); overload;
procedure FillU2(var Desc; Count: UG; Value: U2);
procedure FillU4(var Desc; Count: UG; Value: U4);
procedure FillU8(var Desc; Count: UG; Value: U8);
procedure FillUG(var Desc; Count: UG; Value: UG);
procedure FillOrderU1(var Desc; const Count: UG);
procedure FillOrderU4(var Desc; const Count: UG);
procedure FillOrderUG(var Desc; const Count: UG);
procedure Reverse4(var Desc; Size: UG);
procedure ReverseG(var Desc; Size: UG);
procedure Reverse(var Desc; const ItemSize: SG; const Count: SG);
procedure Swap02(var Desc; Count: UG; Step: S4);
function SwapU4(D: U4): U4;

function BitScanReverse(AValue: U4): U4; overload;
function BitScanReverse(AValue: U8): U4; overload;
function CountDigits(AValue: U4): U4; overload;
function CountDigits(AValue: U8): U4; overload;

function TimeDifference(const NowTime, LastTime: U4): U4; overload;
function TimeDifference(const NowTime, LastTime: U8): U8; overload;

procedure Nop; assembler;
procedure Pause; assembler;

function CalcShr(N: U4): S1;
procedure CheckExpSize(const Size: SG);
function AllocByExp(const OldSize: SG; var NewSize: SG): BG;
function SetNormalSize(var x, y: SG; const MaxWidth, MaxHeight: SG): BG; deprecated;
function SetSmallerSize(var x, y: SG; const MaxWidth, MaxHeight: SG): BG;
function SetLargerSize(var x, y: SG; const MinWidth, MinHeight: SG): BG;
procedure SetScale(var x, y: SG; const MaxWidth, MaxHeight: SG);

function BitsToByte(const Bits: U8): U8;

function Suma(const AValues: TArrayOfSG): SG;
function MaxValueIndex(const AValues: TArrayOfSG): SG;

implementation

uses
  Math,
  SysUtils;

procedure Increment(var X: F4; const N: F4 = 1); overload;
begin
  X := X + N;
end;

procedure Increment(var X: F8; const N: F8 = 1); overload;
begin
  X := X + N;
end;

{$ifdef HasExtended}
procedure Increment(var X: FA; const N: FA = 1); overload;
begin
  X := X + N;
end;
{$endif}

procedure Decrement(var X: F4; const N: F4 = 1); overload;
begin
  X := X - N;
end;

procedure Decrement(var X: F8; const N: F8 = 1); overload;
begin
  X := X - N;
end;

{$ifdef HasExtended}
procedure Decrement(var X: FA; const N: FA = 1); overload;
begin
  X := X - N;
end;
{$endif}

procedure Multiply(var X: U1; const N: U1); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: U2; const N: U2); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: U4; const N: U4); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: U8; const N: U8); overload;
begin
  X := X * N;
end;

{$ifdef CPUX64}
procedure Multiply(var X: UG; const N: UG); overload;
begin
  X := X * N;
end;
{$endif}

procedure Multiply(var X: S1; const N: S1); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: S2; const N: S2); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: S4; const N: S4); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: S8; const N: S8); overload;
begin
  X := X * N;
end;

{$ifdef CPUX64}
procedure Multiply(var X: SG; const N: SG); overload;
begin
  X := X * N;
end;
{$endif}

procedure Multiply(var X: F4; const N: F4); overload;
begin
  X := X * N;
end;

procedure Multiply(var X: F8; const N: F8); overload;
begin
  X := X * N;
end;

{$ifdef HasExtended}
procedure Multiply(var X: FA; const N: FA); overload;
begin
  X := X * N;
end;
{$endif}

procedure Divide(var X: F4; const N: F4); overload;
begin
  X := X / N;
end;

procedure Divide(var X: F8; const N: F8); overload;
begin
  X := X / N;
end;

{$ifdef HasExtended}
procedure Divide(var X: FA; const N: FA); overload;
begin
  X := X / N;
end;
{$endif}

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

{$ifdef HasExtended}
function Sgn(const I: FA): SG;
begin
	if I = 0 then
		Result := 0
	else if I > 0 then
		Result := 1
	else
		Result := -1;
end;
{$endif}

function SgnMul(const Signum, Num: SG): SG; overload;
begin
	if Signum = 0 then
		Result := 0
	else if Signum > 0 then
		Result := Num
	else
		Result := -Num;
end;

function SgnMul(const Signum, Num: FG): FG; overload;
begin
	if Signum = 0 then
		Result := 0
	else if Signum > 0 then
		Result := Num
	else
		Result := -Num;
end;

function AbsMin(const A, B: SG): SG;
begin
	if Abs(A) < Abs(B) then
		Result := A
	else
		Result := B;
end;
{
function Min(const A, B: UG): UG;
begin
	if A < B then
		Result := A
	else
		Result := B;
end;}

// Result := Value1 * Value2;
{$ifdef CPUx64}
procedure Multiply(const ValueA, ValueB: U8; out HighResult, LowResult: U8); assembler; register;
asm
  mov rax, ValueA {rcx}
  mul ValueB {rdx} // Unsigned multiply (RDX:RAX = RAX * operand).
  mov qword ptr [HighResult], rdx
  mov qword ptr [LowResult], rax
{$else}
procedure Multiply(const ValueA, ValueB: U8; out HighResult, LowResult: U8);
var
  Total, R0, R1, R2, R3: TU8;
begin
  R0.A := U8(TU8(ValueA).D0) * U8(TU8(ValueB).D0);
  R1.A := U8(TU8(ValueA).D1) * U8(TU8(ValueB).D0);
  R2.A := U8(TU8(ValueA).D0) * U8(TU8(ValueB).D1);
  Total.A := U8(R0.D1) + U8(R1.D0) + U8(R2.D0);
  TU8(LowResult).D0 := R0.D0;
  TU8(LowResult).D1 := Total.D0;
  R3.A := U8(TU8(ValueA).D1) * U8(TU8(ValueB).D1);
  HighResult := U8(Total.D1) + U8(R1.D1) + U8(R2.D1) + U8(R3.D0);
  Inc(TU8(HighResult).D1, R3.D1);
{$endif}
end;

// Result := (Value1 * Value2) shr 64;
{$ifdef CPUx64}
function MultiplyAndReturnMostSignificantHalf(const ValueA, ValueB: U8): U8; assembler; register;
asm
  mov rax, ValueA {rcx}
  mul ValueB {rdx} // Unsigned multiply (RDX:RAX = RAX * operand).
  mov Result, rdx {rax}
{$else}
function MultiplyAndReturnMostSignificantHalf(const ValueA, ValueB: U8): U8;
var
  Total, R0, R1, R2, R3: TU8;
begin
  R0.A := U8(TU8(ValueA).D0) * U8(TU8(ValueB).D0);
  R1.A := U8(TU8(ValueA).D1) * U8(TU8(ValueB).D0);
  R2.A := U8(TU8(ValueA).D0) * U8(TU8(ValueB).D1);
  Total.A := U8(R0.D1) + U8(R1.D0) + U8(R2.D0);
  // Least Significant Half is in R.D0:R0.D0
  R3.A := U8(TU8(ValueA).D1) * U8(TU8(ValueB).D1);
  Result := U8(Total.D1) + U8(R1.D1) + U8(R2.D1) + U8(R3.D0);
  Inc(TU8(Result).D1, R3.D1);
{$endif}
end;

procedure DivModU2(const Dividend: U2; const Divisor: U1;
	out Res, Remainder: U1); register;
{$ifndef X86ASMRTL}
begin
  Res := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$else}
asm
	div dl // al := ax div dl; ah := ax mod dl
	mov edx, Remainder
	mov [ecx], al
	mov [edx], ah
{$endif}
end;

procedure DivModU4(const Dividend: U4; const Divisor: U2;
	out Res, Remainder: U2); register;
{$ifndef X86ASMRTL}
begin
  Res := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$else}
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
{$endif}
end;

procedure DivModS4(const Dividend: S4; const Divisor: S2;
	out Res, Remainder: S2); register;
{$ifndef X86ASMRTL}
begin
  Res := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$else}
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
{$endif}
end;

procedure DivModU8(const Dividend: U8; const Divisor: U4;
	out Res, Remainder: U4); pascal;
{$ifndef X86ASMRTL}
begin
  Res := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$else}
asm
	push ebx
	mov eax, U4 ptr [Dividend] // Divident-lo
	mov edx, U4 ptr [Dividend + 4]// Divident-hi
	mov ebx, Divisor
	div ebx // eax:=edx&eax div ebx; edx:=edx&eax mod ebx
	mov ebx, Res
	mov [ebx], eax
	mov ebx, Remainder
	mov [ebx], edx
	pop ebx
{$endif}
end;

procedure DivModS8(const Dividend: S8; const Divisor: S4;
	out Res, Remainder: S4); pascal;
{$ifndef X86ASMRTL}
begin
  Res := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$else}
asm
	pushad
	mov eax, U4 ptr [Dividend] // Divident-lo
	mov edx, U4 ptr [Dividend + 4]// Divident-hi
	mov ebx, Divisor
	idiv ebx // eax:=edx&eax div ebx; edx:=edx&eax mod ebx
	mov edi, Res
	mov [edi], eax
	mov edi, Remainder
	mov [edi], edx
	popad
{$endif}
end;

// Divisor = 10
procedure UnsignedDivMod10(const Dividend: U4; out Result: U4; out Reminder: U4);
const
  ReminderTable: array[0..15] of SG = (0, 1, 2, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 8, 9, 0);
var
  Index: SG;
begin
  {$Q-}
  Index := ($19999999 * Dividend + (Dividend shr 1) + (Dividend shr 3)) shr 28;
  Reminder := ReminderTable[Index];
  Result := U4((Dividend - Reminder) shr 1)*$CCCCCCCD;
end;

function UnsignedMod(const Dividend: S8; const Divisor: SG): SG;
begin
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
	if Dividend >= 0 then
		Result := Dividend mod Divisor
	else
	begin
		Result := Dividend + Divisor * (Abs(Dividend - Divisor + 1) div Divisor);
	end;
end;

function ModE(x, y: FM): FM;
begin
	Result := x - Floor(x / y) * y;
end;

function GetAbsoluteError(const A, B: FG): FG;
begin
  Result := Abs(A - B);
end;

{$ifdef HasExtended}
function GetAbsoluteError(const A, B: FA): FA;
begin
  Result := Abs(A - B);
end;
{$endif}

function GetRelativeError(const A, B: FG): FG;
begin
  if Abs(B) > Abs(A) then
    Result := Abs((A - B) / B)
  else
    Result := Abs((A - B) / A);
end;

{$ifdef HasExtended}
function GetRelativeError(const A, B: FA): FA;
begin
  if Abs(B) > Abs(A) then
    Result := Abs((A - B) / B)
  else
    Result := Abs((A - B) / A);
end;
{$endif}

function EqualRelative(const A, B, MaxRelativeError: FG): BG;
var
  RelativeError: FG;
begin
  RelativeError := GetRelativeError(A, B);
  Result := RelativeError <= MaxRelativeError;
end;

{$ifdef HasExtended}
function EqualRelative(const A, B, MaxRelativeError: FA): BG;
var
  RelativeError: FA;
begin
  RelativeError := GetRelativeError(A, B);
  Result := RelativeError <= MaxRelativeError;
end;
{$endif}

function EqualAbsolute(const A, B, MaxAbsoluteError: FG): BG;
var
  AbsoluteError: FG;
begin
  AbsoluteError := GetAbsoluteError(A, B);
  Result := AbsoluteError <= MaxAbsoluteError;
end;

{$ifdef HasExtended}
function EqualAbsolute(const A, B, MaxAbsoluteError: FA): BG;
var
  AbsoluteError: FA;
begin
  AbsoluteError := GetAbsoluteError(A, B);
  Result := AbsoluteError <= MaxAbsoluteError;
end;
{$endif}

function Factorial(const AValue: SG): SG; overload;
const
  FactorialTable: array[0..12] of SG = (
    1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600);
begin
  if (AValue < Low(FactorialTable)) or (AValue > High(FactorialTable)) then
    raise EArgumentOutOfRangeException.Create('Factorial argument not in 0..12 range.');
  Result := FactorialTable[AValue];
end;

function FastSqrt(A: UG): UG;
const
	Base = 16;
	BaseS = 4;
	Base2 = Base * Base; // 256
	BaseS2 = 8;
var
	AX, B, k: UG;
  Pow: SG;
begin
	B := 0;
	AX := 0;
	Pow := 24;
	while Pow >= 0 do
	begin
		B := B shl BaseS;
		k := B shl 1 + 1;
		AX := AX shl BaseS2 + (A shr UG(Pow)) and (Base2 - 1);
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

function LinearMax(Clock, Maximum: UG): UG;
begin
	Result := Clock mod (2 * Maximum);
	if Result > Maximum then Result := 2 * Maximum - Result;
end;

procedure Rotate(var X, Y: SG; MaxX, MaxY: SG; Angle: SG); overload;
var T: SG;
begin
	case Angle and 3 of
	// Up
	1: // Left
	begin
		T := X;
		X := Y;
		Y := MaxX - T;
	end;
	2: // Down
	begin
		X := MaxX - X;
		Y := MaxY - Y;
	end;
	3: // Right
	begin
		T := X;
		X := MaxY - Y;
		Y := T;
	end;
	end;
end;

procedure Rotate(var X, Y: Double; MaxX, MaxY: Double; Angle: SG); overload;
var T: Double;
begin
	case Angle and 3 of
	// Up
	1: // Left
	begin
		T := X;
		X := Y;
		Y := MaxX - T;
	end;
	2: // Down
	begin
		X := MaxX - X;
		Y := MaxY - Y;
	end;
	3: // Right
	begin
		T := X;
		X := MaxY - Y;
		Y := T;
	end;
	end;
end;

function RoundN(const X: FG): S8;
// Rounds a number "normally": if the fractional
// part is >= 0.5 the number is rounded up (see RoundUp)
// Otherwise, if the fractional part is < 0.5, the
// number is rounded down (see RoundDn).
//   RoundN(3.5) = 4     RoundN(-3.5) = -4
//   RoundN(3.1) = 3     RoundN(-3.1) = -3
begin
(*
  if Abs(Frac(X)) >= 0.5 then
    Result := RoundUp(X)
  else
    Result := RoundDn(X);
*)
	Result := Trunc(X) + Trunc(Frac(X) * 2);
end;

{$ifdef HasExtended}
function RoundN(const X: FA): S8;
// Rounds a number "normally": if the fractional
// part is >= 0.5 the number is rounded up (see RoundUp)
// Otherwise, if the fractional part is < 0.5, the
// number is rounded down (see RoundDn).
//   RoundN(3.5) = 4     RoundN(-3.5) = -4
//   RoundN(3.1) = 3     RoundN(-3.1) = -3
begin
(*
  if Abs(Frac(X)) >= 0.5 then
    Result := RoundUp(X)
  else
    Result := RoundDn(X);
*)
	Result := Trunc(X) + Trunc(Frac(X) * 2);
end;
{$endif}

function RoundSG(Value: F4): SG;
begin
	if Value > MaxInt then
		Result := MaxInt
	else if Value < MinInt then
		Result := MinInt
	else
		Result := RoundN(Value);
end;

function RoundSG(Value: F8): SG;
begin
	if Value > MaxInt then
		Result := MaxInt
	else if Value < MinInt then
		Result := MinInt
	else
		Result := RoundN(Value);
end;

{$ifdef HasExtended}
function RoundSG(Value: FA): SG;
begin
	if Value > MaxInt then
		Result := MaxInt
	else if Value < MinInt then
		Result := MinInt
	else
		Result := RoundN(Value);
end;
{$endif}

function RoundS8(Value: FG): S8;
begin
	if Value > High(Result) then
		Result := High(Result)
	else if Value < Low(Result) then
		Result := Low(Result)
	else
		Result := RoundN(Value);
end;

{$ifdef HasExtended}
function RoundS8(Value: FA): S8;
begin
	if Value > High(Result) then
		Result := High(Result)
	else if Value < Low(Result) then
		Result := Low(Result)
	else
		Result := RoundN(Value);
end;
{$endif}

function RoundU8(Value: FG): U8;
begin
	if Value > MaxU8 then
		Result := MaxU8
	else if Value < MinU8 then
		Result := MinU8
	else
		Result := Trunc(Value) + Trunc(Frac(Value) * 2);
end;

{$ifdef HasExtended}
function RoundU8(Value: FA): U8;
begin
	if Value > MaxU8 then
		Result := MaxU8
	else if Value < MinU8 then
		Result := MinU8
	else
		Result := Trunc(Value) + Trunc(Frac(Value) * 2);
end;
{$endif}

function TruncS8(Value: FG): S8;
begin
	if Value > High(Result) then
		Result := High(Result)
	else if Value < Low(Result) then
		Result := Low(Result)
	else
		Result := Trunc(Value);
end;

{$ifdef HasExtended}
function TruncS8(Value: FA): S8;
begin
	if Value > High(Result) then
		Result := High(Result)
	else if Value < Low(Result) then
		Result := Low(Result)
	else
		Result := Trunc(Value);
end;
{$endif}

function RangeS8(Value: FG): BG;
begin
	if Value >= High(S8) then
		Result := False
	else if Value <= Low(S8) then
		Result := False
	else
		Result := True;
end;

{$ifdef HasExtended}
function RangeS8(Value: FA): BG;
begin
	if Value >= High(S8) then
		Result := False
	else if Value <= Low(S8) then
		Result := False
	else
		Result := True;
end;
{$endif}

function RoundDiv(const Dividend: SG; const Divisor: SG): SG;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
	if Dividend < 0 then
		Result := (Dividend - (Divisor div 2)) div Divisor
	else
		Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function RoundDivU8(const Dividend: U8; const Divisor: U8): U8;
begin
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
	Result := (Dividend + (Divisor div 2)) div Divisor;
end;

function RoundDivS8(const Dividend: S8; const Divisor: S8): S8;
// 0 div 4 is 0
// 1 div 4 is 0
// 2 div 4 is 1
// 3 div 4 is 1
begin
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
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
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
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
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
	if Dividend < 0 then
		Result := (Dividend - Divisor + 1) div Divisor
	else
		Result := (Dividend + Divisor - 1) div Divisor;
end;

function RandomDiv(const Dividend: SG; const Divisor: SG): SG;
// 0 div 4 is 0
// 1 div 4 is 0 in 75%, 1 in 25%
// 2 div 4 is 0 in 50%, 1 in 50%
// 3 div 4 is 0 in 25%, 1 in 75%
begin
	if IsDebug then
    if Divisor = 0 then
    begin
      Assert(False);
      Result := 0;
      Exit;
    end;
	if Dividend < 0 then
		Result := (Dividend - Random(Divisor)) div Divisor
	else
		Result := (Dividend + Random(Divisor)) div Divisor;
end;

function IsInRange(const Min, Cur, Max: S1): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: U1): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: S2): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: U2): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: S4): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: U4): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: S8): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: U8): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: F4): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function IsInRange(const Min, Cur, Max: F8): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

{$ifdef HasExtended}
function IsInRange(const Min, Cur, Max: FA): BG;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;
{$endif}

{$ifdef MSWINDOWS}
function IsInRange(const Min, Cur, Max: AnsiChar): BG; overload;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;
{$endif}

function IsInRange(const Min, Cur, Max: WideChar): BG; overload;
begin
  Result := (Min <= Cur) and (Cur <= Max);
end;

function Range(const Min, Cur, Max: SG): SG;
begin
	Result := Cur;
	if Cur < Min then
		Result := Min
	else if Cur > Max then
		Result := Max;
end;

function Range(const Min, Cur, Max, Def: SG): SG;
begin
	Result := Cur;
	if Cur < Min then
		Result := Def
	else if Cur > Max then
		Result := Def;
end;

function Range(const Min, Cur, Max: UG): UG;
begin
	Result := Cur;
	if Cur < Min then
		Result := Min
	else if Cur > Max then
		Result := Max;
end;

function Range(const Min, Cur, Max: FG): FG;
begin
	Result := Cur;
	if Cur < Min then
		Result := Min
	else if Cur > Max then
		Result := Max;
end;

function RangeOverflow(const Min, Cur, Max: SG): SG;
begin
	Result := Cur;
	if Cur < Min then
		Result := Max
	else if Cur > Max then
		Result := Min;
end;

function IsInTheMiddle(const AValue: SG; const AMaximalValue: SG; const AMaximalWidth: SG): BG;
begin
  Result := Abs(2 * AValue - AMaximalValue) < 2 * AMaximalWidth;
end;

procedure Exchange(var A, B: B1); register;
{$ifndef X86ASMRTL}
var
  C: B1;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov cl, [A]
	xchg cl, [B]
	mov [A], cl
{$endif}
end;

procedure Exchange(var A, B: U1); register;
{$ifndef X86ASMRTL}
var
  C: U1;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov cl, [A]
	xchg cl, [B]
	mov [A], cl
{$endif}
end;

procedure Exchange(var A, B: S1); register;
{$ifndef X86ASMRTL}
var
  C: S1;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov cl, [A]
	xchg cl, [B]
	mov [A], cl
{$endif}
end;

procedure Exchange(var A, B: U2); register;
{$ifndef X86ASMRTL}
var
  C: U2;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov cx, [A]
	xchg cx, [B]
	mov [A], cx
{$endif}
end;

procedure Exchange(var A, B: S2); register;
{$ifndef X86ASMRTL}
var
  C: S2;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov cx, [A]
	xchg cx, [B]
	mov [A], cx
{$endif}
end;

procedure Exchange(var A, B: B4); register;
{$ifndef X86ASMRTL}
var
  C: B4;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
{$endif}
end;

procedure Exchange(var A, B: U4); register;
{$ifndef X86ASMRTL}
var
  C: U4;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
{$endif}
end;

procedure Exchange(var A, B: S4); register;
{$ifndef X86ASMRTL}
var
  C: S4;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
{$endif}
end;

procedure Exchange(var A, B: Pointer); register;
{$ifndef X86ASMRTL}
var
  C: Pointer;
begin
  C := A;
  A := B;
  B := C;
{$else}
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
{$endif}
end;

procedure Exchange(var A, B: S8);
var C: S8;
begin
	C := A;
	A := B;
	B := C;
end;

{$if CompilerVersion >= 23}
procedure Exchange(var A, B: NativeInt);
var C: NativeInt;
begin
	C := A;
	A := B;
	B := C;
end;

procedure Exchange(var A, B: NativeUInt);
var C: NativeInt;
begin
	C := A;
	A := B;
	B := C;
end;

{$endif}

procedure Exchange(var A, B: F4);
var C: F8;
begin
	C := A;
	A := B;
	B := C;
end;

procedure Exchange(var A, B: F8);
var C: F8;
begin
	C := A;
	A := B;
	B := C;
end;

{$ifdef HasExtended}
procedure Exchange(var A, B: FA);
var C: FA;
begin
	C := A;
	A := B;
	B := C;
end;
{$endif}

procedure Exchange(var P0, P1; Count: UG); register;
{$ifndef ASSEMBLER}
var
  i: SG;
begin
  for i := 0 to Count - 1 do
  begin
    Exchange(PByte(P0)^, PByte(P1)^);
    Inc(PByte(P0), 1);
    Inc(PByte(P1), 1);
  end;
{$else}
asm
{$ifdef CPUX64}
	push rdi
	push rsi

	MOV rSI,rAX
	MOV rDI,rDX
	ADD rDX,rCX
	@Loop:
		mov al, [esi]
		xchg al, [rdi]
		mov [rsi], al
		add rdi, 1
		add rsi, 1
		cmp rdi, rdx
	jb @Loop

	POP rSI
	POP rDI

{$else}
	push edi
	push esi

	MOV ESI,EAX
	MOV EDI,EDX
	ADD EDX,ECX
	@Loop:
		mov al, [esi]
		xchg al, [edi]
		mov [esi], al
		add edi, 1
		add esi, 1
		cmp edi, edx
	jb @Loop

	POP ESI
	POP EDI
{$endif}
{$endif}
end;

procedure Exchange(P0, P1: Pointer; Count: UG); register;
{$ifndef ASSEMBLER}
var
  i: SG;
begin
  for i := 0 to Count - 1 do
  begin
    Exchange(PByte(P0)^, PByte(P1)^);
    Inc(PByte(P0), 1);
    Inc(PByte(P1), 1);
  end;
{$else}
asm
{$ifdef CPUX64}
	PUSH rDI
	PUSH rSI

	MOV rSI,P0
	MOV rDI,P1
	ADD rDX,Count
	@Loop:
		mov al, [rsi]
		xchg al, [rdi]
		mov [rsi], al
		add rdi, 1
		add rsi, 1
		cmp rdi, rdx
	jb @Loop

	POP rSI
	POP rDI
{$else}
	PUSH EDI
	PUSH ESI

	MOV ESI,EAX
	MOV EDI,EDX
	ADD EDX,ECX
	@Loop:
		mov al, [esi]
		xchg al, [edi]
		mov [esi], al
		add edi, 1
		add esi, 1
		cmp edi, edx
	jb @Loop

	POP ESI
	POP EDI
{$endif}
{$endif}
end;

procedure Exchange(var s0, s1: string);
var
	s: string;
begin
	s := s1;
	s1 := s0;
	s0 := s;
end;

procedure Exchange(var A, B: TObject);
var
	T: TObject;
begin
	T := A;
	A := B;
	B := T;
end;

function Arg(X, Y: FM): FM; // <0..2pi)
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
	if Result < 0 then
    Result := 2 * pi - Abs(Result);
end;

procedure CheckBool(var Bool: B1);
begin
	Bool := B1(U1(Bool) and 1);
end;

procedure CheckBool(var Bool: B2);
begin
	Bool := B2(U2(Bool) and 1);
end;

procedure CheckBool(var Bool: B4);
begin
	Bool := B4(U4(Bool) and 1);
end;

{$ifdef CPUX64}
procedure Order(var I1, I2: S4);
var I: S4;
begin
	if I1 > I2 then
	begin
		I := I1;
		I1 := I2;
		I2 := I;
	end;
end;
{$endif}

procedure Order(var I1, I2: SG);
var I: SG;
begin
	if I1 > I2 then
	begin
		I := I1;
		I1 := I2;
		I2 := I;
	end;
end;

procedure Order(var I1, I2: UG);
var I: UG;
begin
	if I1 > I2 then
	begin
		I := I1;
		I1 := I2;
		I2 := I;
	end;
end;

procedure FillSinTable(Sins: PSinTable; const AngleCount, SinDiv: SG);
var i: TAngle;
begin
	for i := 0 to AngleCount - 1 do
	begin
		Sins[0] := RoundN((SinDiv - 1) * sin(2 * pi * i / AngleCount));
		Inc(PByte(Sins), SizeOf(TAngle));
//  Sins[i]:=Trunc(127*(sin(pi*i/128))+127);
//  Sins[i]:=Trunc(128*(sin(pi*i/128)+1))-128;
	end;
end;

procedure ReadMem(P: Pointer; Size: UG); register;
{$ifndef X86ASMRTL}
var
  i: SG;
begin
  for i := 0 to Size - 1 do
  begin
    if (PByte(P)^ = 0) then
      ; // No Code
    Inc(PByte(P), 1);
  end;
{$else}
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
{$endif}
end;

function SameData(P0, P1: Pointer; Size: UG): BG; register;
{$ifndef X86ASMRTL}
var
  i: SG;
  b0, b1: U1;
begin
  Result := True;
  for i := 0 to Size - 1 do
  begin
    b0 := PByte(P0)^;
    b1 := PByte(P1)^;
    if b0 <> b1 then
    begin
      Result := False;
      Exit;
    end;

    Inc(PByte(P0), 1);
    Inc(PByte(P1), 1);
  end;
{$else}
asm
{	push ebx
	mov Result, 1
	cmp Size, 0
	je @Exit
	mov Result, 0
	add Size, P0
	@Loop:
		mov ebx, [P0]
		cmp ebx, [P1]
		jne @Exit
		add P0, 4
		add P1, 4
		cmp P0, Size
	jb @Loop
	mov Result, 1
	@Exit:
	pop ebx}
	mov Result, 1
	cmp Size, 0
	je @Exit
	push ebx
	push edi
	push esi
	cld
	mov esi, eax
	mov edi, edx
	mov edx, ecx
	mov bl,cl
	sar ecx, 2
	jcxz @L3
	and bl, 3
	repe cmpsd
	jne @NotSame
@L3:
	mov cl,bl
	repe cmpsb
	jne @NotSame

	mov Result, 1
	jmp @Exit
	@NotSame:
	mov Result, 0
	@Exit:
	pop esi
	pop edi
	pop ebx
	@Exit0:
{$endif}
end;

procedure ClearMemory(var AAddress; const ACount: UG);
{$if not defined(ASSEMBLER)}
const
  Size = 8;
var
  I: NativeInt;
  PAddress: PU8;
  Remain: UG;
begin
  PAddress := PU8(@AAddress);
  for I := SG(ACount div Size) - 1 downto 0 do
  begin
    PAddress^ := 0;
    Inc(PAddress);
  end;
  Remain := ACount and (Size - 1); // mod Size
  while Remain > 0 do
  begin
    PU1(PAddress)^ := 0;
    Inc(PU1(PAddress));
    Dec(Remain);
  end;
{$ELSE}
asm
  // Optional speed up if ACount = 0
  test ACount, ACount
  jle @Exit

  cld // Clear direction flag
{$ifndef CPUX64}
  push edi
  mov edi, dword ptr AAddress {eax}
  mov ecx, ACount {edx}
  xor eax, eax
{$else}
  push rdi
  mov rdi, qword ptr AAddress {rcx}
  mov rcx, ACount {rdx}
  xor rax, rax
{$endif}
    rep stosb
{$ifndef CPUX64}
  pop edi
{$else}
  pop rdi
{$endif}
  @Exit:
{$endif}
end;

procedure FillMemory(var Desc; Count: UG; Value: U1);
begin
  FillChar(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: U2);
begin
  FillU2(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: U4);
begin
  FillU4(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: U8);
begin
  FillU8(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: S1);
begin
  FillChar(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: S2);
begin
  FillU2(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: S4);
begin
  FillU4(Desc, Count, Value);
end;

procedure FillMemory(var Desc; Count: UG; Value: S8);
begin
  FillU8(Desc, Count, Value);
end;

procedure FillU2(var Desc; Count: UG; Value: U2); register;
{$ifndef X86ASMRTL}
var
  I: NativeInt;
  V: Int64;
  PB: PU2;
  P: PInt64;
  Total: NativeInt;
begin
  if Count >= 8 then
  begin
    V := Word(Value) or (Word(Value) shl 16);
    V := V or (V shl 32);
    P := PInt64(@Desc);
    Total := Count shr 2;

    for I := 0 to Total - 1 do
    begin
      P^ := V;
      Inc(P);
    end;
    PB := Pointer(P);
    { Get the remainder (mod 4) }
    Total := Count and $03;
  end
  else
  begin
    PB := PU2(@Desc);
    Total := Count;
  end;

  for I := Total - 1 downto 0 do
  begin
    PB^ := Value;
    Inc(PB);
  end;
{$else}
asm
	PUSH    EDI
	MOV     EDI,EAX
	MOV     EAX,ECX
	shl eax, 16
	mov ax, cx
	MOV     ECX,EDX
	sar ecx, 1
	REP     STOSD
@@exit:
	POP     EDI
{$endif}
end;

procedure FillU4(var Desc; Count: UG; Value: U4); register;
{$ifndef X86ASMRTL}
var
  I: NativeInt;
  V: Int64;
  PB: PU4;
  P: PInt64;
  Total: NativeInt;
begin
  if Count >= 8 then
  begin
    V := NativeInt(Value) or (NativeInt(Value) shl 32);
    P := PInt64(@Desc);
    Total := Count shr 1;

    for I := 0 to Total - 1 do
    begin
      P^ := V;
      Inc(P);
    end;
    PB := Pointer(P);
    { Get the remainder (mod 4) }
    Total := Count and $01;
  end
  else
  begin
    PB := PU4(@Desc);
    Total := Count;
  end;

  for I := Total - 1 downto 0 do
  begin
    PB^ := Value;
    Inc(PB);
  end;
{$else}
asm
	PUSH    EDI
	MOV     EDI,EAX
	MOV     EAX,ECX
	MOV     ECX,EDX
	REP     STOSD
@@exit:
	POP     EDI
{$endif}
end;

procedure FillU8(var Desc; Count: UG; Value: U8);
var
  I: NativeInt;
  P: PU8;
begin
  P := PU8(@Desc);
  for I := Count - 1 downto 0 do
  begin
    P^ := Value;
    Inc(P);
  end;
end;

procedure FillUG(var Desc; Count: UG; Value: UG); register;
begin
{$ifdef CPUX64}
  FillU8(Desc, Count, Value);
{$else}
  FillU4(Desc, Count, Value);
{$endif}
end;

procedure FillOrderU1(var Desc; const Count: UG); register;
{$ifndef X86ASMRTL}
var
  P: PU1;
  i: SG;
begin
  P := PU1(@Desc);
  for i := 0 to Count - 1 do
  begin
    P^ := i;
    Inc(P);
  end;
{$else}
asm
	cmp Count, 0
	je @Exit
	add Count, Desc
	xor ecx, ecx
	@Loop:
		mov [Desc], cl
		inc Desc
		cmp Desc, Count
		inc cl
	jb @Loop
	@Exit:
{$endif}
end;

procedure FillOrderU4(var Desc; const Count: UG); register;
{$ifndef X86ASMRTL}
var
  P: PU4;
  i: SG;
begin
  P := PU4(@Desc);
  for i := 0 to Count - 1 do
  begin
    P^ := i;
    Inc(P);
  end;
{$else}
asm
	cmp Count, 0
	je @Exit
	shl Count, 2
	add Count, Desc
	xor ecx, ecx
	@Loop:
		mov [Desc], ecx
		add Desc, 4
		cmp Desc, Count
		inc ecx
	jb @Loop
	@Exit:
{$endif}
end;

procedure FillOrderUG(var Desc; const Count: UG); register;
{$ifndef X86ASMRTL}
var
  P: PUG;
  i: SG;
begin
  P := PUG(@Desc);
  for i := 0 to Count - 1 do
  begin
    P^ := i;
    Inc(P);
  end;
{$else}
asm
	cmp Count, 0
	je @Exit
	shl Count, 2
	add Count, Desc
	xor ecx, ecx
	@Loop:
		mov [Desc], ecx
		add Desc, 4
		cmp Desc, Count
		inc ecx
	jb @Loop
	@Exit:
{$endif}
end;

procedure Reverse4(var Desc; Size: UG); register;
{$ifndef X86ASMRTL}
var
  P1, P2: PU4;
  i: SG;
  d1, d2: U4;
begin
  P1 := PU4(@Desc);
  P2 := PU4(SG(@Desc) + SizeOf(U4) * SG(Size - 1));
  for i := 0 to Size div 2 - 1 do
  begin
    d1 := P1^;
    d2 := P2^;
    P1^ := d2;
    P2^ := d1;
    Inc(P1);
    Dec(P2);
  end;
{$else}
asm
	push esi
	push ebx
	mov esi, Desc
	sub esi, 4
	shl Size, 2
	add esi, Size
	shr Size, 3
	cmp Size, 0
	je @Exit
	shl Size, 2
	add Size, Desc
	@Loop:
		mov ecx, U4 ptr [Desc]
		mov ebx, U4 ptr [esi]
		mov [esi], ecx
		mov [Desc], ebx

		sub esi, 4
		add Desc, 4
		cmp Desc, Size
	jb @Loop
	@Exit:
	pop ebx
	pop esi
{$endif}
end;

procedure ReverseG(var Desc; Size: UG);
var
  P1, P2: PSG;
  i: SG;
  d1, d2: SG;
begin
  P1 := PSG(@Desc);
  P2 := PSG(SG(@Desc) + SizeOf(SG) * SG(Size - 1));
  for i := 0 to SG(Size div 2) - 1 do
  begin
    d1 := P1^;
    d2 := P2^;
    P1^ := d2;
    P2^ := d1;
    Inc(P1);
    Dec(P2);
  end;
end;

procedure Reverse(var Desc; const ItemSize: SG; const Count: SG);
var
  i: SG;
  P1, P2, Tmp: Pointer;
begin
  GetMem(Tmp, ItemSize);
  try
    P1 := PSG(@Desc);
    P2 := PSG(SG(@Desc) + ItemSize * SG(Count - 1));
    for i := 0 to Count div 2 - 1 do
    begin
      Move(P1, Tmp, ItemSize);
      Move(P2, P1, ItemSize);
      Move(Tmp, P2, ItemSize);
      Inc(SG(P1), ItemSize);
      Dec(SG(P2), ItemSize);
    end;
  finally
    FreeMem(Tmp);
  end;
end;

procedure Swap02(var Desc; Count: UG; Step: S4); register;
{$ifndef ASSEMBLER}
var
  i: UG;
begin
  Assert(Step > 0);
  i := 0;
  while i < Count do
  begin
    PWord(Desc) := PWord(Swap(Integer(PWord(Desc))));
    Inc(PByte(Desc), Step);
    Inc(i, Step);
  end;
{$else}
asm
{$ifdef CPUX64}
  push rdi
{$else}
	PUSH    EDI
{$endif}
	MOV     EDI, EAX
	add edx, edi
	@Loop:
//	xchg [edi], [edi + 2]
		mov al, [edi]
		xchg al, [edi + 2]
		mov [edi], al
		add edi, ecx
		cmp edi, edx
	jb @Loop
{$ifdef CPUX64}
  pop rdi
{$else}
	POP EDI
{$endif}
{$endif}
end;

function SwapU4(D: U4): U4; register;
{$ifndef ASSEMBLER}
begin
  TU4(Result).B3 := TU4(D).B0;
  TU4(Result).B2 := TU4(D).B1;
  TU4(Result).B1 := TU4(D).B2;
  TU4(Result).B0 := TU4(D).B3;
{$else}
asm
	bswap D
	mov Result, D
{$endif}
end;

function BitScanReverse(AValue: U4): U4; register;
{$ifndef ASSEMBLER}
var
  i: SG;
begin
  for i := 31 downto 0 do
  begin
    if AValue and $80000000 <> 0 then
    begin
      Result := i;
      Exit;
    end;
    AValue := AValue shl 1;
  end;
  Result := 0;
{$else}
asm // Highest bit set
{$ifdef CPUX64}
  bsr ecx, AValue // ecx = BSR(AValue)
  mov Result, ecx
{$else}
  bsr eax, AValue // Result = BSR(AValue)
{$endif}
{$endif}
end;

function BitScanReverse(AValue: U8): U4; register;
{$ifndef ASSEMBLER}
var
  i: SG;
begin
  for i := 63 downto 0 do
  begin
    if AValue and $8000000000000000 <> 0 then
    begin
      Result := i;
      Exit;
    end;
    AValue := AValue shl 1;
  end;
  Result := 0;
{$else}
asm // Highest bit set
{$ifdef CPUX64}
  bsr rcx, AValue // rcx = BSR(AValue)
  mov Result, ecx {low part of rcx}
{$else}
  xor eax, eax // required if AValue-hi or AValue-lo is 0 (bsr do not change eax in this case)
  bsr eax, U4 ptr [AValue + 4] // AValue-hi
  jz @low
  add eax, 32
  jmp @exit

  @low:
  bsr eax, U4 ptr [AValue] // AValue.lo
  @exit:
	mov Result, eax // Result-lo
{$endif}
{$endif}
end;

function CountDigits(AValue: U4): U4;
const
  Powers: array[0..9] of U4 = (
    0,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000);
  MaxDigits: array[0..32] of U4 =
    (1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
     6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10);
begin
  Result := MaxDigits[BitScanReverse(AValue)];
  if (AValue < Powers[Result - 1]) then
    Dec(Result);
end;

function CountDigits(AValue: U8): U4;
const
  Powers: array[0..19] of U8 = (
    0,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000,
    10000000000000000000
    );
  MaxDigits: array[0..64] of U4 =
    (1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5,
     6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10,
     11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16,
     16, 16, 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 20, 20, 20, 20
   );
begin
  Result := MaxDigits[BitScanReverse(AValue)];
  if (AValue < Powers[Result - 1]) then
    Dec(Result);
end;

function TimeDifference(const NowTime, LastTime: U4): U4;
{$ifndef ASSEMBLER}
begin
  Result := NowTime - LastTime;
{$else}
asm
	mov Result, NowTime
	sub Result, LastTime
{$endif}
end;

function TimeDifference(const NowTime, LastTime: U8): U8;
begin
	Result := NowTime - LastTime;
end;

procedure Nop; assembler;
{$ifndef ASSEMBLER}
begin
  // No code
{$else}
asm
  nop
{$endif}
end;

procedure Pause; assembler;
{$ifndef ASSEMBLER}
begin
  // No code
{$else}
asm
  pause // Same opcode F390h as "rep nop"
{$endif}
end;

function CalcShr(N: U4): S1;
{
	0: -1
	1: 0
	2: 1
	4: 2
	8: 3
	...
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

procedure CheckExpSize(const Size: SG);
begin
	Assert(Size = 1 shl CalcShr(Size), 'Bad type size');
end;

(*
function AllocByB(const OldSize: SG; var NewSize: SG;
	BlockSize: SG): BG;
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
    if IsDebug then
			ErrorMessage('Bad AllocBy block size' + LineSep + NToS(BlockSize) + ' bytes');
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
	BlockSize: SG): BG;
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
		if IsDebug then
			ErrorMessage('Bad AllocBy block size' + LineSep + NToS(BlockSize) + ' bytes');
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

function AllocByExp(const OldSize: SG; var NewSize: SG): BG;
const
	MinimumSize = 8;
{
	0 <= OldSize < 2^31
	0 <= NewSize < 2^31
}
begin
	Assert(NewSize >= 0);
	if IsDebug then
  begin
		if (OldSize < 0) or (OldSize > GB) then
//		ErrorMessage(LineSep + BToStr(OldSize));
		Assert(False, 'Bad AllocBy block OldSize');
	if (NewSize < 0) or (NewSize > GB) then
//		ErrorMessage('Bad AllocBy block NewSize' + LineSep + BToStr(NewSize));
		Assert(False, 'Bad AllocBy block NewSize');
	end;

	Result := False;
	if NewSize > OldSize then
	begin
		if IsDebug then
    begin
      if OldSize > 0 then
        if OldSize <> 1 shl CalcShr(OldSize) then
        begin
          Assert(False, 'Bad AllocBy block size');
    //			ErrorMessage('Bad AllocBy block size' + LineSep + BToStr(OldSize));
        end;
		end;
		NewSize := Max(1 shl CalcShr(NewSize), MinimumSize);
		Result := True;
	end
	else
	begin
		if NewSize < OldSize then
		begin
			if NewSize = 0 then
				Result := True;
		end;
	end;
end;

function SetSmallerSize(var x, y: SG; const MaxWidth, MaxHeight: SG): BG;
begin
	Result := False;
	if (x > 0) and (y > 0) then
	begin
		while (x > MaxWidth) or (y > MaxHeight) do
		begin
			x := x div 2;
			y := y div 2;
			Result := True;
		end;
	end;
end;

function SetLargerSize(var x, y: SG; const MinWidth, MinHeight: SG): BG;
begin
	Result := False;
	if (x > 0) and (y > 0) then
	begin
		while (x < MinWidth) or (y < MinHeight) do
		begin
			x := x * 2;
			y := y * 2;
			Result := True;
		end;
	end;
end;

function SetNormalSize(var x, y: SG; const MaxWidth, MaxHeight: SG): BG;
begin
	Result := False;
	if (x > 0) and (y > 0) then
	begin
		Result := SetSmallerSize(x, y, MaxWidth, MaxHeight);

		if Result then Exit;

		while (x < 200) or (y < 3 * 256 div 4) do
		begin
			x := x * 2;
			y := y * 2;
			Result := True;
		end;
		if Result then
			SetSmallerSize(x, y, MaxWidth, MaxHeight);
	end;
end;

procedure SetScale(var x, y: SG; const MaxWidth, MaxHeight: SG);
var
  D: SG;
begin
	if (x = 0) or (y = 0) then Exit;
  D := x * MaxHeight - MaxWidth * y;
  if D = 0 then
  begin
    x := MaxWidth;
    y := MaxHeight;
  end
  else if D < 0 then
  begin
    x := RoundDiv(MaxHeight * x, y);
    y := MaxHeight;
  end
  else
  begin
    y := RoundDiv(MaxWidth * y, x);
    x := MaxWidth;
  end;
end;

function BitsToByte(const Bits: U8): U8;
begin
	Result := (Bits + 7) shr 3;
end;

function Suma(const AValues: TArrayOfSG): SG;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to Length(AValues) - 1 do
  begin
    Inc(Result, AValues[i]);
  end;
end;

function MaxValueIndex(const AValues: TArrayOfSG): SG;
var
  i: SG;
  MaxValue: SG;
begin
  Result := -1;
  MaxValue := MinInt;
  for i := Length(AValues) - 1 downto 0 do
  begin
    if AValues[i] >= MaxValue then
    begin
      MaxValue := AValues[i];
      Result := i;
    end;
  end;
end;

end.

