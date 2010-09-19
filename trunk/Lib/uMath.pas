//* File:     Lib\uMath.pas
//* Created:  1998-01-01
//* Modified: 2005-11-05
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uMath;

interface

uses uTypes;

// Mathematics
const
//	AngleCount = 512{16384}; // 2*pi 256; 65536 // 2^x 1.. D???
	SinDiv = 32768; // 1.0 65536; // 1..128..1024*1024
type
	PAngle = ^TAngle;
	TAngle = S2;
	PSinTable = ^TSinTable;
	TSinTable = array[0..32767] of TAngle;

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
function TruncS8(Value: FA): S8;
function RangeS8(Value: FA): BG;
function RoundDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function RoundDivU8(const Dividend: U8; const Divisor: U8): S8; //overload;
function RoundDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;
function MaxDiv(const Dividend: SG; const Divisor: SG): SG; //overload;
function MaxDivS8(const Dividend: S8; const Divisor: S8): S8; //overload;

function Range(const Min, Cur, Max: Integer): Integer; overload;
function Range(const Min, Cur, Max, Def: Integer): Integer; overload;
function Range(const Min, Cur, Max: Cardinal): Cardinal; overload;
function Range(const Min, Cur, Max: FG): FG; overload;

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
procedure Exchange(P0, P1: Pointer; Count: Cardinal); register; overload;

function Arg(X, Y: Extended): Extended; overload;

function Random2(Range: SG): SG;
function RandomU4: U4;
function RandomM: U4;

procedure CheckBool(var Bool: ByteBool); overload;
procedure CheckBool(var Bool: WordBool); overload;
procedure CheckBool(var Bool: LongBool); overload;

procedure Order(var I1, I2: Integer); overload;
procedure Order(var I1, I2: Cardinal); overload;
procedure FillSinTable(Sins: PSinTable; const AngleCount, SinDiv: SG);

//procedure GetMem0(var P: Pointer; Size: Cardinal);
procedure ReadMem(P: Pointer; Size: Cardinal);
function SameData(P0, P1: Pointer; Size: Cardinal): Boolean;
procedure FillU2(var Desc; Count: Cardinal; Value: U2);
procedure FillU4(var Desc; Count: Cardinal; Value: U4);
procedure FillOrderU4(var Desc; Size: Cardinal);
procedure Reverse4(var Desc; Size: Cardinal);
function Checksum(var Desc; Size: Cardinal): U4;
procedure Swap02(var Desc; Count: Cardinal; Step: S4);
function SwapU4(D: U4): U4;

var
	PerformanceType: U1;
	PerformanceFrequency: U8;

procedure InitPerformanceCounter;
function GetCPUCounter: TU8;
function PerformanceCounter: Int64;
procedure Delay(const ms: LongWord);
procedure DelayEx(const f: Int64);

implementation

uses Math, Windows;

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
			bIndex.setText("xxx");}
		end;
	end
	else
	begin
		if(C.h = 360) then C.h := 0;

		C.h:=C.h div 60;
//		i := Floor(h);

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
end;
*)

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
//		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		CreateException;
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

function TruncS8(Value: FA): S8;
begin
	if Value > High(Result) then
		Result := High(Result)
	else if Value < Low(Result) then
		Result := Low(Result)
	else
		Result := Trunc(Value);
end;

function RangeS8(Value: FA): BG;
begin
	if Value >= High(S8) then
		Result := False
	else if Value <= Low(S8) then
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
//		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		CreateException;
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
//		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		CreateException;
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
//		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		CreateException;
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
//		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		CreateException;
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
//		MessageD('Division by 0' + LineSep + NToS(Dividend) + ' / 0', mtError, [mbOk]);
		CreateException;
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

function Range(const Min, Cur, Max: FG): FG;
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
	TU4(Result).W0 := U4(Random(65536));
	TU4(Result).W1 := U4(Random(65536));
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

procedure Exchange(var A, B: B1); register;
asm
	mov cl, [A]
	xchg cl, [B]
	mov [A], cl
end;

procedure Exchange(var A, B: U1); register;
asm
	mov cl, [A]
	xchg cl, [B]
	mov [A], cl
end;

procedure Exchange(var A, B: S1); register;
asm
	mov cl, [A]
	xchg cl, [B]
	mov [A], cl
end;

procedure Exchange(var A, B: U2); register;
asm
	mov cx, [A]
	xchg cx, [B]
	mov [A], cx
end;

procedure Exchange(var A, B: S2); register;
asm
	mov cx, [A]
	xchg cx, [B]
	mov [A], cx
end;

procedure Exchange(var A, B: B4); register;
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
end;

procedure Exchange(var A, B: U4); register;
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
end;

procedure Exchange(var A, B: S4); register;
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
end;

procedure Exchange(var A, B: Pointer); register; // 32bit only
asm
	mov ecx, [A]
	xchg ecx, [B]
	mov [A], ecx
end;

procedure Exchange(var A, B: S8);
var C: S8;
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

procedure Exchange(var A, B: FA);
var C: FA;
begin
	C := A;
	A := B;
	B := C;
end;

procedure Exchange(var P0, P1; Count: Cardinal); register;
asm
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
end;

procedure Exchange(P0, P1: Pointer; Count: Cardinal); register;
asm
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

procedure FillSinTable(Sins: PSinTable; const AngleCount, SinDiv: SG);
var i: TAngle;
begin
	for i := 0 to AngleCount - 1 do
	begin
		Sins[0] := Round((SinDiv - 1) * sin(2 * pi * i / AngleCount));
		Inc(SG(Sins), SizeOf(TAngle));
//  Sins[i]:=Trunc(127*(sin(pi*i/128))+127);
//  Sins[i]:=Trunc(128*(sin(pi*i/128)+1))-128;
	end;
end;

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

function SameData(P0, P1: Pointer; Size: Cardinal): Boolean; register;
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
end;

procedure FillU2(var Desc; Count: Cardinal; Value: U2); register;
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
end;

procedure FillU4(var Desc; Count: Cardinal; Value: U4); register;
asm
	PUSH    EDI
	MOV     EDI,EAX
	MOV     EAX,ECX
	MOV     ECX,EDX
	REP     STOSD
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

procedure Reverse4(var Desc; Size: Cardinal); register;
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
end;

function Checksum(var Desc; Size: Cardinal): U4; register;
asm
	mov Result, 0
	and Size, $fffffffc
	cmp Size, 0
	je @Exit
	mov ecx, eax
	add ecx, Size
	@Loop:
		mov edx, [eax]
		add Result, edx

		add eax, 4
		cmp eax, ecx
	jb @Loop
	@Exit:
end;

procedure Swap02(var Desc; Count: Cardinal; Step: S4); register;
asm
	PUSH    EDI
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
	POP EDI
end;

function SwapU4(D: U4): U4; register;
asm
	bswap D
	mov Result, D
{begin
	TU4(Result).B0 := TU4(D).B3;
	TU4(Result).B1 := TU4(D).B2;
	TU4(Result).B2 := TU4(D).B1;
	TU4(Result).B3 := TU4(D).B0;}
end;

procedure InitPerformanceCounter;
begin
	if QueryPerformanceFrequency(PerformanceFrequency) then
	begin
		PerformanceType := 1;
		if PerformanceFrequency < 1000 then
		begin
			PerformanceType := 0;
			PerformanceFrequency := 1000;
		end;
	end
	else
	begin
		PerformanceType := 0;
		PerformanceFrequency := 1000;
	end;
end;

function GetCPUCounter: TU8; register;
asm
	push Result
	mov ecx, 10h
	dw 310fh // RDTSC 10 clocks
	pop ecx
	mov [ecx], eax
	mov [ecx + 4], edx
end;

function PerformanceCounter: Int64;
begin
	case PerformanceType of
	0: Result := GetTickCount;
	1: QueryPerformanceCounter(Result);
	else Result := GetCPUCounter.A;
	end;
end;

procedure Delay(const ms: LongWord);
var
	TickCount: LongWord;
begin
	TickCount := GetTickCount + ms;
	while GetTickCount < TickCount do
end;

procedure DelayEx(const f: Int64);
var
	TickCount: Int64;
begin
	TickCount := PerformanceCounter + f;
	while PerformanceCounter < TickCount do
end;

initialization
	InitPerformanceCounter;
end.

