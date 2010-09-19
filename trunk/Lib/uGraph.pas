//* File:     Lib\uGraph.pas
//* Created:  1999-05-01
//* Modified: 2005-09-05
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uGraph;

interface

uses
	uTypes,
	Windows, Graphics, StdCtrls, Classes, SysUtils;

const
	clMoneyGreen = TColor($C0DCC0);
	clSkyBlue = TColor($F0CAA6);
	clCream = TColor($F0FBFF);
	clMedGray = TColor($A4A0A0);

	clFlesh = TColor($98ADFF);
	clBaize = TColor($818000);
	clWater = TColor($D1D856);

	clDepth: array[0..3] of TColor = (cl3DDkShadow{Black}, clBtnShadow{Gray}, cl3DLight{Silver}, clBtnHighlight{White});

	MaxSpectrum2 = 762;
	MaxFireColor = 765;

procedure Rotate(var X, Y: SG; MaxX, MaxY: SG; Angle: SG);

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;
function ColorToHTML(Color: TColor): string;
//function ShadowColor(C: TColor): TColor;
//function ShadowColor2(C1, C2: TColor): TColor;
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

function MixColors(C1, C2: TColor; Per1, Per2: Integer): TColor; overload;
function MixColors(C1, C2: TRGBA; Per1, Per2: Integer): TRGBA; overload;

function MixColors(C1, C2: TColor; Per: Integer): TColor; overload;
function MixColors(C1, C2: TRGBA; Per: Integer): TRGBA; overload;

procedure ShadowText(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const CF, CB: TColor);
procedure GoodText(Canvas: TCanvas; R: TRect; Text: string;
	const C1, C2, C3: TColor; const Alignment: TAlignment; const Layout: TTextLayout);
procedure CanvasLine(Canvas: TCanvas;
	const X1, Y1, X2, Y2: Integer);
procedure CanvasLineTo(Canvas: TCanvas;
	const X, Y, OffsetX, OffsetY: Integer);
procedure Rec(Canvas: TCanvas; const Rect: TRect;
	const Color: TColor; const Width: Integer);
procedure Border(Canvas: TCanvas; const Rect: TRect;
	TopColor, BottomColor: TColor; const Width: Integer);

function CutText(const Canvas: TCanvas; const Text: string; const Width: SG): string;
procedure DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: AnsiString; FontShadow: SG); overload;
procedure DrawShadowText(const Canvas: TCanvas; X, Y: SG; const Text: AnsiString; FontShadow: SG); overload;
procedure DrawCutedText(const Canvas: TCanvas; const Rect: TRect;
	const Alignment: TAlignment; const Layout: TTextLayout; Caption: AnsiString; const WordWrap: BG; FontShadow: SG); overload;

function Over(const SX1, SY1, SX2, SY2: Integer;
	const DX1, DY1, DX2, DY2: Integer): Boolean; overload;
function Over3D(const SX1, SY1, SZ1, SX2, SY2, SZ2: Integer;
	const DX1, DY1, DZ1, DX2, DY2, DZ2: Integer): Boolean; overload;
function OverE(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;
procedure InflatePoint(var P: TPoint; d: SG); overload;
procedure InflatePoint(var P: TPoint; dx, dy: SG); overload;

implementation

uses
	Math,
	uStrings, uError, uGetInt, uMath;

procedure Rotate(var X, Y: SG; MaxX, MaxY: SG; Angle: SG);
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

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;
begin
	Result := (((PixelFormat * X  + 31) and $FFFFFFE0) div 8) * Y;
end;
(*-------------------------------------------------------------------------*)
function ColorToHTML(Color: TColor): string;
var C: TRGBA;
begin
	C.L := ColorToRGB(Color);
	Result := '#' +
		IntToHex(C.R, 2) +
		IntToHex(C.G, 2) +
		IntToHex(C.B, 2);
end;
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
begin
	Color := ColorToRGB(Color);
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
	if 2 * TRGBA(C).R + 4 * TRGBA(C).G + 1 * TRGBA(C).B > 768 then
		Result := $00000000
	else
		Result := $00FFFFFF;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TColor): TColor; overload;
begin
	if C1 = C2 then
	begin
		Result := C1;
		Exit;
	end;
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
	if C1.L = C2.L then
	begin
		Result := C1;
		Exit;
	end;
	Result.R := (C1.R + C2.R) shr 1;
	Result.G := (C1.G + C2.G) shr 1;
	Result.B := (C1.B + C2.B) shr 1;
	Result.A := (C1.B + C2.B) shr 1;
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
procedure ShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: string; const CF, CB: TColor);
var n: SG;
begin
	if CB = clNone then
		Canvas.Brush.Style := bsClear
	else
	begin
		Canvas.Brush.Style := bsSolid;
		Canvas.Brush.Color := CB;
	end;

	Canvas.Font.Color := MixColors(CF, CB);
	n := 1 + Canvas.Font.Size div 16;
	Canvas.TextOut(X + n, Y + n, Text);

	Canvas.Brush.Style := bsClear;
	Canvas.Font.Color := CF;
	Canvas.TextOut(X, Y, Text);
end;
(*-------------------------------------------------------------------------*)
procedure GoodText(Canvas: TCanvas; R: TRect; Text: string;
	const C1, C2, C3: TColor; const Alignment: TAlignment; const Layout: TTextLayout);
begin
	Canvas.Font.Color := MixColors(C1, C2);
	DrawCutedText(Canvas, Rect(R.Left + 1, R.Top - 1, R.Right + 1, R.Bottom - 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := MixColors(C1, C2);
	DrawCutedText(Canvas, Rect(R.Left - 1, R.Top + 1, R.Right - 1, R.Bottom + 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := C1;
	DrawCutedText(Canvas, Rect(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := C2;
	DrawCutedText(Canvas, Rect(R.Left - 1, R.Top - 1, R.Right - 1, R.Bottom - 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := C3;
	DrawCutedText(Canvas, Rect(R.Left, R.Top, R.Right, R.Bottom),
		Alignment, Layout, Text, True, 0);
end;
(*-------------------------------------------------------------------------*)
procedure CanvasLine(Canvas: TCanvas; const X1, Y1, X2, Y2: Integer);
begin
	Canvas.MoveTo(X1, Y1);
	Canvas.LineTo(X2, Y2);
	Canvas.Pixels[X2, Y2] := Canvas.Pen.Color;
end;
(*-------------------------------------------------------------------------*)
procedure CanvasLineTo(Canvas: TCanvas; const X, Y, OffsetX, OffsetY: Integer);
begin
	Canvas.MoveTo(X, Y);
	Canvas.LineTo(X + OffsetX, Y + OffsetY);
	Canvas.Pixels[X + OffsetX, Y + OffsetY] := Canvas.Pen.Color;
end;
(*-------------------------------------------------------------------------*)
procedure Rec(Canvas: TCanvas; const Rect: TRect;
	const Color: TColor; const Width: Integer);
var
	X1, Y1, X2, Y2: Integer;
	i: Integer;
begin
	Canvas.Pen.Width := 1;
	Canvas.Pen.Color := Color;
	X1 := Rect.Left;
	Y1 := Rect.Top;
	X2 := Rect.Right - 1;
	Y2 := Rect.Bottom - 1;
	i := Width;
	while i > 0 do
	begin
		Dec(i);
		CanvasLine(Canvas, X1 + i,   Y1 + i,   X2 - i - 1, Y1 + i); //-
		CanvasLine(Canvas, X1 + i,   Y1 + i + 1, X1 + i,   Y2 - i); //|
		CanvasLine(Canvas, X1 + i + 1, Y2 - i,   X2 - i,   Y2 - i); //-
		CanvasLine(Canvas, X2 - i,   Y1 + i, X2 - i,   Y2 - i - 1); //|
	end;
end;
(*-------------------------------------------------------------------------*)
procedure Border(Canvas: TCanvas; const Rect: TRect;
	TopColor, BottomColor: TColor; const Width: Integer);
var
	X1, Y1, X2, Y2: Integer;
	i: Integer;
begin
	Canvas.Pen.Width := 1;
	X1 := Rect.Left;
	Y1 := Rect.Top;
	X2 := Rect.Right - 1;
	Y2 := Rect.Bottom - 1;
	i := Width;
	while i > 0 do
	begin
		Dec(i);
		Canvas.Pen.Color := TopColor;
		CanvasLine(Canvas, X1 + i,   Y1 + i,   X2 - i - 1, Y1 + i); //-
		CanvasLine(Canvas, X1 + i,   Y1 + i + 1, X1 + i,   Y2 - i - 1); //|}
		Canvas.Pen.Color := BottomColor;
		CanvasLine(Canvas, X1 + i + 1, Y2 - i,   X2 - i,   Y2 - i); //-
		CanvasLine(Canvas, X2 - i,   Y1 + i + 1, X2 - i,   Y2 - i - 1); //|
		TopColor := ColorToRGB(TopColor);
		BottomColor := ColorToRGB(BottomColor);
		Canvas.Pixels[X1 + i, Y2 - i] := MixColors(TopColor, BottomColor);
		Canvas.Pixels[X2 - i, Y1 + i] := MixColors(TopColor, BottomColor);
	end;
end;
(*-------------------------------------------------------------------------*)
function CutText(const Canvas: TCanvas; const Text: string; const Width: SG): string;
var w: SG;
begin
	Result := Text;
	if Canvas.TextWidth(DelCharsF(Result, '&')) > Width then
	begin
		w := Length(Result);
		while w > 1 do
		begin
			if Canvas.TextWidth(DelCharsF(Result, '&') + '…') <= Width then Break;
			Dec(w);
			SetLength(Result, w);
		end;
		Result := Result + '…';
	end;
end;

procedure DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: AnsiString; FontShadow: SG);
var
	C: TColor;
	B: TBrushStyle;
begin
	if FontShadow <> 0 then
	begin
		B := Canvas.Brush.Style;
		Canvas.Brush.Style := bsClear;
		C := Canvas.Font.Color;
		Canvas.Font.Color := $808080; // D??? 50% transparency
		repeat
			OffsetRect(R, FontShadow, FontShadow);
//			ExtTextOut(Canvas.Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
			DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_NOCLIP or DT_SINGLELINE);
			OffsetRect(R, -FontShadow, -FontShadow);
			if FontShadow > 0 then Dec(FontShadow) else Inc(FontShadow);
		until FontShadow = 0;
		Canvas.Font.Color := C;
		Canvas.Brush.Style := B;
	end;
//	ExtTextOut(Canvas.Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
	DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_NOCLIP or DT_SINGLELINE);
end;

procedure DrawShadowText(const Canvas: TCanvas; X, Y: SG; const Text: AnsiString; FontShadow: SG);
var R: TRect;
begin
	R.Left := X;
	R.Top := Y;
	R.Right := High(R.Right);
	R.Bottom := High(R.Bottom);
	DrawShadowText(Canvas, R, Text, FontShadow);
end;

procedure DrawCutedText(const Canvas: TCanvas; const Rect: TRect;
	const Alignment: TAlignment; const Layout: TTextLayout; Caption: AnsiString; const WordWrap: BG; FontShadow: SG);
const Border = 0;
var
	i, LastSpace{, k}: Integer;
	Lines: array of string; // D??? Breaks!
	LineCount: SG;
	Text, TextR: string;
	CurX, CurY: Integer;
	TextHeight: Integer;
	MaxLines: Integer;
	R: TRect;
	NewSize: SG;
begin
	TextHeight := Max(Canvas.TextHeight('W'), 1);
	MaxLines := (Rect.Bottom - Rect.Top) div TextHeight - 1;
	SetLength(Lines, 1);
	LineCount := 0;
	i := 1;
	LastSpace := 0;
	DelChars(Caption, CharCR);
	while i <= Length(Caption) do
	begin
		if Caption[i] = CharSpace then
		begin
			LastSpace := i;
		end;

		if (Caption[i] = CharLF) or
			((LineCount < MaxLines) and (i > 1) and
			(WordWrap and (Canvas.TextWidth(DelCharsF(Copy(Caption, 1, i), '&')) > Rect.Right - Rect.Left))) then
		begin
			if Caption[i] = CharLF then
			begin
				Lines[LineCount] := Copy(Caption, 1, i - 1);
				Delete(Caption, 1, i);
			end
			else
			if LastSpace = 0 then
			begin
				Lines[LineCount] := Copy(Caption, 1, i - 1);
				Delete(Caption, 1, i - 1);
			end
			else
			begin
				Lines[LineCount] := Copy(Caption, 1, LastSpace - 1);
				Delete(Caption, 1, LastSpace);
			end;
			LastSpace := 0;
			NewSize := LineCount + 2;
			if AllocByExp(Length(Lines), NewSize) then
				SetLength(Lines, NewSize);
			Inc(LineCount);
			i := 0;
		end;
		Inc(i);
		if (i = Length(Caption) + 1) then
		begin
			NewSize := LineCount + 2;
			if AllocByExp(Length(Lines), NewSize) then
				SetLength(Lines, NewSize);
			Lines[LineCount] := Caption;
			Inc(LineCount);
			Break;
		end;
	end;

	case Layout of
	tlTop: CurY := Rect.Top;
	tlBottom: CurY := Rect.Bottom + 1 - TextHeight * LineCount;
	else {tlCenter} CurY := Rect.Top + (Rect.Bottom - Rect.Top + 1 - TextHeight * LineCount) div 2;
	end;

	for i := 0 to LineCount - 1 do
	begin
		if Lines[i] <> '' then
		begin
			Text := CutText(Canvas, Lines[i], Rect.Right - Rect.Left - 2 * Border);
			TextR := DelCharsF(Text, '&');
			case Alignment of
			taLeftJustify: CurX := Rect.Left + Border;
			taRightJustify: CurX := Rect.Right - Border + 1 - Canvas.TextWidth(TextR);
			else {taCenter} CurX := Rect.Left + (Rect.Right - Rect.Left + 1 - Canvas.TextWidth(TextR)) div 2;
			end;
			if CurX < Rect.Left + Border then CurX := Rect.Left + Border;

			R.Left := CurX;
			R.Top :=  CurY;
			R.Right := Rect.Right;
			R.Bottom := Rect.Bottom;
			DrawShadowText(Canvas, R, Text, FontShadow);
		end;
		Inc(CurY, TextHeight);
	end;
end;
(*-------------------------------------------------------------------------*)
function Over(const SX1, SY1, SX2, SY2: Integer;
	const DX1, DY1, DX2, DY2: Integer): Boolean; overload;
begin
	Result := False;

	if (SX1 > DX2) and (SX2 > DX2) then Exit;
	if (SX1 < DX1) and (SX2 < DX1) then Exit;
	if (SY1 > DY2) and (SY2 > DY2) then Exit;
	if (SY1 < DY1) and (SY2 < DY1) then Exit;
	Result := True;
end;

function Over3D(const SX1, SY1, SZ1, SX2, SY2, SZ2: Integer;
	const DX1, DY1, DZ1, DX2, DY2, DZ2: Integer): Boolean; overload;
begin
	Result := False;

	if (SX1 > DX2) and (SX2 > DX2) then Exit;
	if (SX1 < DX1) and (SX2 < DX1) then Exit;
	if (SY1 > DY2) and (SY2 > DY2) then Exit;
	if (SY1 < DY1) and (SY2 < DY1) then Exit;
	if (SZ1 > DZ2) and (SZ2 > DZ2) then Exit;
	if (SZ1 < DZ1) and (SZ2 < DZ1) then Exit;
	Result := True;
end;

function OverE(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;
begin
	Result := False;

	if (SX1 > DX2) and (SX2 > DX2) then Exit;
	if (SX1 < DX1) and (SX2 < DX1) then Exit;
	if (SY1 > DY2) and (SY2 > DY2) then Exit;
	if (SY1 < DY1) and (SY2 < DY1) then Exit;
	Result := True;
end;

procedure InflatePoint(var P: TPoint; d: SG);
begin
	Inc(P.X, d);
	Inc(P.Y, d);
end;

procedure InflatePoint(var P: TPoint; dx, dy: SG);
begin
	Inc(P.X, dx);
	Inc(P.Y, dy);
end;

end.
