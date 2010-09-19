unit uColor;

interface

uses
	uTypes,
	Graphics;

const
	clMoneyGreen = TColor($C0DCC0);
	clSkyBlue = TColor($F0CAA6);
	clCream = TColor($F0FBFF);
	clMedGray = TColor($A4A0A0);

	clFlesh = TColor($98ADFF);
	clBaize = TColor($818000);
	clWater = TColor($D1D856);

	clDepth: array[0..3] of TColor = (cl3DDkShadow{Black}, clBtnShadow{Gray}, cl3DLight{Silver}, clBtnHighlight{White});

	MaxSpectrum = 1529;
	MaxSpectrum2 = 762;
	MaxFireColor = 765;

type
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

	THLSColor = packed record // 4
		case Integer of
		0:
		(
		H: -1..MaxSpectrum; // 2
		L: 0..255; // 1
		S: 0..255; // 1
		);
		1: (
		A: S4
(*		Hue: -1..MaxSpectrum; // 2
		Lightness{Lum(inary, inous)}: 0..255; // 1
		Saturation{Sat(iety)}: 0..255; // 1 *)
		);
	end;

function RGBToHLS(C: TRGBA): THLSColor;
function HLSToRGB(C: THLSColor): TRGBA;

function ColorDiv(Color: TColor; const D: Integer): TColor;
function ColorRB(C: TColor): TColor;
function LighterColor(Color: TColor): TColor;
function DarkerColor(Color: TColor): TColor;
function GrayColor(X: SG): TColor;
function SpectrumColor(X: Integer): TColor;
function SpectrumColor2(X: Integer): TColor;
function FireColor(X: Integer): TColor;
function NegColor(C: TColor): TColor;
function NegMonoColor(C: TColor): TColor;

function MixColors(C1, C2: TColor): TColor; overload;
function MixColors(C1, C2: TRGBA): TRGBA; overload;
function MixColors(C1, C2: TRGB): TRGB; overload;

function MixColors(C1, C2: TColor; Per1, Per2: Integer): TColor; overload;
function MixColors(C1, C2: TRGBA; Per1, Per2: Integer): TRGBA; overload;
function MixColors(C1, C2: TRGB; Per1, Per2: Integer): TRGB; overload;

function MixColors(C1, C2: TColor; Per: Integer): TColor; overload;
function MixColors(C1, C2: TRGBA; Per: Integer): TRGBA; overload;
function MixColors(C1, C2: TRGB; Per: Integer): TRGB; overload;

implementation

uses
	uMath,
	Math;

function RGBToHLS(C: TRGBA): THLSColor;
var
	maxC, minC, delta, H: SG;
begin
	Result.H := -1;
	Result.L := 0;
	Result.S := 0;

	maxC := Max(Max(C.R, C.G), C.B);
	minC := Min(Min(C.R, C.G), C.B);

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

function HLSToRGB(C: THLSColor): TRGBA;

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
(*-------------------------------------------------------------------------*)
{function ShadowColor(C: TColor): TColor;
begin
	case C of
	clNone:
		Result := clNone;
	clWindowText, clBtnShadow:
		Result := clBtnHighlight;
	clBtnHighlight:
		Result := clBtnShadow;
	else
	begin
		C := ColorToRGB(C);
		TRGBA(Result).A := 0;
		if (TRGBA(C).R <= 128) and (TRGBA(C).G <= 128) and (TRGBA(C).B <= 128) then
		begin
			if TRGBA(C).B <= 127 then TRGBA(Result).B := TRGBA(C).B shl 1 else TRGBA(Result).B := 255;
			if TRGBA(C).G <= 127 then TRGBA(Result).G := TRGBA(C).G shl 1 else TRGBA(Result).G := 255;
			if TRGBA(C).R <= 127 then TRGBA(Result).R := TRGBA(C).R shl 1 else TRGBA(Result).R := 255;
		end
		else
		begin
			TRGBA(Result).B := (TRGBA(C).B + 1) shr 1;
			TRGBA(Result).G := (TRGBA(C).G + 1) shr 1;
			TRGBA(Result).R := (TRGBA(C).R + 1) shr 1;
		end;
	end;
	end;
end;}
(*-------------------------------------------------------------------------*)
{function ShadowColor2(C1, C2: TColor): TColor;
begin
	case C1 of
	clNone:
		Result := clNone;
	clBtnShadow:
		Result := clBtnHighlight;
	clBtnHighlight:
		Result := clBtnShadow;
	else
	begin
		C1 := ColorToRGB(C1);
		C2 := ColorToRGB(C2);
		TRGBA(Result).A := 0;
		if (TRGBA(C1).R <= 128) and (TRGBA(C1).G <= 128) and (TRGBA(C1).B <= 128) then
		begin
			if TRGBA(C1).B <= 127 then TRGBA(Result).B := TRGBA(C1).B shl 1 else TRGBA(Result).B := 255;
			if TRGBA(C1).G <= 127 then TRGBA(Result).G := TRGBA(C1).G shl 1 else TRGBA(Result).G := 255;
			if TRGBA(C1).R <= 127 then TRGBA(Result).R := TRGBA(C1).R shl 1 else TRGBA(Result).R := 255;
		end
		else
		begin
			TRGBA(Result).B := (TRGBA(C1).B + TRGBA(C2).B + 1) shr 2;
			TRGBA(Result).G := (TRGBA(C1).G + TRGBA(C2).G + 1) shr 2;
			TRGBA(Result).R := (TRGBA(C1).R + TRGBA(C2).R + 1) shr 2;
		end;
	end;
	end;
end; }
(*-------------------------------------------------------------------------*)
function ColorDiv(Color: TColor; const D: Integer): TColor;
var R, G, B: Integer;
{var
	HLS: THLSColor;
	L: SG;}
begin
	Color := ColorToRGB(Color);
{	HLS := RGBToHLS(TRGBA(Color));
	L := HLS.L * D shr 16;
	if L > 255 then L := 255;
	HLS.L := L;
	Result := HLSToRGB(HLS).L;}
	R := D * TRGBA(Color).R shr 16;
	G := D * TRGBA(Color).G shr 16;
	B := D * TRGBA(Color).B shr 16;
	if R > 255 then R := 255;
	if G > 255 then G := 255;
	if B > 255 then B := 255;
	TRGBA(Result).R := R;
	TRGBA(Result).G := G;
	TRGBA(Result).B := B;
	TRGBA(Result).A := 0;
end;

function LighterColor(Color: TColor): TColor;
begin
	Result := ColorDiv(Color, 4 * 65536 div 3);
end;

function DarkerColor(Color: TColor): TColor;
begin
	Result := ColorDiv(Color, 2 * 65536 div 3);
end;
(*-------------------------------------------------------------------------*)
function ColorRB(C: TColor): TColor;
begin
	TRGBA(Result).R := TRGBA(C).B;
	TRGBA(Result).G := TRGBA(C).G;
	TRGBA(Result).B := TRGBA(C).R;
	TRGBA(Result).A := TRGBA(C).A;
end;
(*-------------------------------------------------------------------------*)
function GrayColor(X: SG): TColor;
begin
	TRGBA(Result).R := X;
	TRGBA(Result).G := X;
	TRGBA(Result).B := X;
	TRGBA(Result).A := 0;
end;

function SpectrumColor(X: SG): TColor;
//0..255..510..765..1020..1275..1529
begin
	if (X < 0) or (X > 1529) then X := X mod 1530;
	TRGBA(Result).A := 0;
	case X of
	0..255:
	begin
		TRGBA(Result).R := 255;
		TRGBA(Result).G := X;
		TRGBA(Result).B := 0;
	end;
	256..510:
	begin
		TRGBA(Result).R := 510 - X;
		TRGBA(Result).G := 255;
		TRGBA(Result).B := 0;
	end;
	511..765:
	begin
		TRGBA(Result).R := 0;
		TRGBA(Result).G := 255;
		TRGBA(Result).B := X - 510;
	end;
	766..1020:
	begin
		TRGBA(Result).R := 0;
		TRGBA(Result).G := 1020 - X;
		TRGBA(Result).B := 255;
	end;
	1021..1275:
	begin
		TRGBA(Result).R := X - 1020;
		TRGBA(Result).G := 0;
		TRGBA(Result).B := 255;
	end;
	else{1276..1529:}
	begin
		TRGBA(Result).R := 255;
		TRGBA(Result).G := 0;
		TRGBA(Result).B := 1530 - X;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function SpectrumColor2(X: Integer): TColor;
//0..255..510..765..1020..1275..1529
begin
	TRGBA(Result).A := 0;
	case X of
	0..127:
	begin
		TRGBA(Result).R := 255;
		TRGBA(Result).G := 128 + X;
		TRGBA(Result).B := 0;
	end;
	128..254:
	begin
		TRGBA(Result).R := 509 - X;
		TRGBA(Result).G := 255;
		TRGBA(Result).B := 0;
	end;
	255..381:
	begin
		TRGBA(Result).R := 0;
		TRGBA(Result).G := 255;
		TRGBA(Result).B := X - 126;
	end;
	382..508:
	begin
		TRGBA(Result).R := 0;
		TRGBA(Result).G := 763 - X;
		TRGBA(Result).B := 255;
	end;
	509..635:
	begin
		TRGBA(Result).R := X - 380;
		TRGBA(Result).G := 0;
		TRGBA(Result).B := 255;
	end;
	else{636..762:}
	begin
		if X > 762 then X := 762;
		TRGBA(Result).R := 255;
		TRGBA(Result).G := 0;
		TRGBA(Result).B := 1017 - X;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function FireColor(X: Integer): TColor;
begin
	TRGBA(Result).A := 0;
	case X of
	Low(X)..255:
	begin
		if X < 0 then X := 0;
		TRGBA(Result).R := X;
		TRGBA(Result).G := 0;
		TRGBA(Result).B := 0;
	end;
	256..510:
	begin
		TRGBA(Result).R := 255;
		TRGBA(Result).G := X - 255;
		TRGBA(Result).B := 0;
	end;
	else
	begin
		if X > 765 then X := 765;
		TRGBA(Result).R := 255;
		TRGBA(Result).G := 255;
		TRGBA(Result).B := X - 510;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function NegColor(C: TColor): TColor;
begin
	C := ColorToRGB(C);
	TRGBA(Result).A := 0;
	if TRGBA(C).R > 127 then TRGBA(Result).R := 0 else TRGBA(Result).R := 255;
	if TRGBA(C).G > 127 then TRGBA(Result).G := 0 else TRGBA(Result).G := 255;
	if TRGBA(C).B > 127 then TRGBA(Result).B := 0 else TRGBA(Result).B := 255;
end;
(*-------------------------------------------------------------------------*)
function NegMonoColor(C: TColor): TColor;
begin
	C := ColorToRGB(C);
	if 2 * TRGBA(C).R + 4 * TRGBA(C).G + 1 * TRGBA(C).B > (8 * 255 div 2) then
		Result := $00000000
	else
		Result := $00FFFFFF;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TColor): TColor; overload;
begin
{	if C1 = C2 then
	begin
		Result := C1;
		Exit;
	end;}
	if ((C1 = clBtnShadow) and (C2 = clBtnHighlight)) or
		((C2 = clBtnShadow) and (C1 = clBtnHighlight)) then
		Result := cl3DLight
	else
	if ((C1 = cl3DDkShadow) and (C2 = cl3DLight)) or
		((C2 = cl3DDkShadow) and (C1 = cl3DLight)) then
		Result := clBtnShadow
	else
	begin
		C1 := ColorToRGB(C1);
		C2 := ColorToRGB(C2);
		TRGBA(Result).R := (TRGBA(C1).R + TRGBA(C2).R) shr 1;
		TRGBA(Result).G := (TRGBA(C1).G + TRGBA(C2).G) shr 1;
		TRGBA(Result).B := (TRGBA(C1).B + TRGBA(C2).B) shr 1;
		TRGBA(Result).A := 0;
	end;
end;

function MixColors(C1, C2: TRGBA): TRGBA; overload;
begin
{	if C1.L = C2.L then
	begin
		Result := C1;
		Exit;
	end;}
	Result.R := (C1.R + C2.R) shr 1;
	Result.G := (C1.G + C2.G) shr 1;
	Result.B := (C1.B + C2.B) shr 1;
	Result.A := (C1.B + C2.B) shr 1;
end;

function MixColors(C1, C2: TRGB): TRGB; overload;
begin
	Result.R := (C1.R + C2.R) shr 1;
	Result.G := (C1.G + C2.G) shr 1;
	Result.B := (C1.B + C2.B) shr 1;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TColor; Per1, Per2: Integer): TColor; overload;
begin
	C1 := ColorToRGB(C1);
	C2 := ColorToRGB(C2);
	TRGBA(Result).R := (Per1 * TRGBA(C1).R + Per2 * TRGBA(C2).R + 32768) shr 16;
	TRGBA(Result).G := (Per1 * TRGBA(C1).G + Per2 * TRGBA(C2).G + 32768) shr 16;
	TRGBA(Result).B := (Per1 * TRGBA(C1).B + Per2 * TRGBA(C2).B + 32768) shr 16;
	TRGBA(Result).A := 0;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TRGBA; Per1, Per2: Integer): TRGBA; overload;
begin
	Assert((Per1 >= 0) and (Per1 <= 65536));
	Assert((Per2 >= 0) and (Per2 <= 65536));
	Result.R := (Per1 * C1.R + Per2 * C2.R + 32768) shr 16;
	Result.G := (Per1 * C1.G + Per2 * C2.G + 32768) shr 16;
	Result.B := (Per1 * C1.B + Per2 * C2.B + 32768) shr 16;
	Result.A := (Per1 * C1.A + Per2 * C2.A + 32768) shr 16;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TRGB; Per1, Per2: Integer): TRGB; overload;
begin
	Assert((Per1 >= 0) and (Per1 <= 65536));
	Assert((Per2 >= 0) and (Per2 <= 65536));
	Result.R := (Per1 * C1.R + Per2 * C2.R + 32768) shr 16;
	Result.G := (Per1 * C1.G + Per2 * C2.G + 32768) shr 16;
	Result.B := (Per1 * C1.B + Per2 * C2.B + 32768) shr 16;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TColor; Per: Integer): TColor; overload;
begin
	Result := MixColors(C1, C2, Per, 65536 - Per);
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TRGBA; Per: Integer): TRGBA; overload;
begin
	Result := MixColors(C1, C2, Per, 65536 - Per);
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TRGB; Per: Integer): TRGB; overload;
begin
	Result := MixColors(C1, C2, Per, 65536 - Per);
end;

end.

