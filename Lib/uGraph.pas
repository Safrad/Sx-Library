//* File:     Lib\uGraph.pas
//* Created:  1999-05-01
//* Modified: 2004-04-28
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uGraph;

interface
uses uAdd, Windows, Graphics, ExtCtrls, StdCtrls, Classes, Controls, SysUtils;

const
{	clMoneyGreen = TColor($C0DCC0);
	clSkyBlue = TColor($F0CAA6);
	clCream = TColor($F0FBFF);
	clMediumGray = TColor($A4A0A0);}

	MaxSpectrum2 = 762;
	MaxFireColor = 765;

procedure Rotate(var X, Y: SG; MaxX, MaxY: SG; Angle: SG);

function GetBmpSize(const X, Y: LongWord; const PixelFormat: Byte): LongWord;
function GetTransparentColor(const Bmp: TBitmap): TColor;
function ShadowColor(C: TColor): TColor;
function ShadowColor2(C1, C2: TColor): TColor;
function ColorDiv(Color: TColor; const D: Integer): TColor;
function ColorRB(C: TColor): TColor;
function LighterColor(Color: TColor): TColor;
function DarkerColor(Color: TColor): TColor;
function SpectrumColor(X: Integer): TColor;
function SpectrumColor2(X: Integer): TColor;
function FireColor(X: Integer): TColor;
function NegColor(C: TColor): TColor;
function NegMonoColor(C: TColor): TColor;
function DepthColor(const Depth: Byte): TColor;
function MixColors(C1, C2: TColor): TColor;
function MixColorsEx(C1, C2: TColor; Per1, Per2: Integer): TColor;

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
procedure DrawCutedText(const Canvas: TCanvas; const Rect: TRect;
	const Alignment: TAlignment; const Layout: TTextLayout; Caption: string; const WordWrap: BG; FontShadow: SG);

function Over(const SX1, SY1, SX2, SY2: Integer;
	const DX1, DY1, DX2, DY2: Integer): Boolean; overload;
function Over3D(const SX1, SY1, SZ1, SX2, SY2, SZ2: Integer;
	const DX1, DY1, DZ1, DX2, DY2, DZ2: Integer): Boolean; overload;
function OverE(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;

procedure BitmapLoadFromFile(Bitmap: TBitmap; FileName: TFileName);
//procedure InitImage(const Image: TImage; const C: TColor);

implementation

uses
	Jpeg,
	uStrings, uError, uGetInt;


(*
procedure WaitRetrace;
// instruction "in al, dx" do not works in Microsoft Windows NT/2000
begin
	if OS.dwPlatformId <= VER_PLATFORM_WIN32_WINDOWS then
	begin
		asm
		{$ifopt O+}
		push dx
		push ax
		{$endif}
		mov dx, 3DAh
{   @L1:
			in al, dx
			and al, 08h
			jz @L2
			push $01
			call Sleep
		jmp @L1}
		@L2:
			in al, dx
			and al, 08h
		jz @L2
		{$ifopt O+}
		pop ax
		pop dx
		{$endif}
		end;
	end;
end;*)

procedure Rotate(var X, Y: SG; MaxX, MaxY: SG; Angle: SG);
var T: SG;
begin
	case Angle and 3 of
	1: // Left
	begin
		T := X;
		X := Y;
		Y := MaxX - T;
	end;
	2:
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

function GetBmpSize(const X, Y: LongWord; const PixelFormat: Byte): LongWord;
begin
	Result := (((PixelFormat * X  + 31) and $FFFFFFE0) div 8) * Y;
end;
(*-------------------------------------------------------------------------*)
function GetTransparentColor(const Bmp: TBitmap): TColor;
const
	MaxPix = 5;
var
	PixColor: array[0..MaxPix] of TColor;
	i, j: Integer;
begin
	Result := clNone;

	PixColor[4] := Bmp.Canvas.Pixels[0, 1];
	PixColor[5] := Bmp.Canvas.Pixels[Bmp.Width - 1, 1];

	for i := 4 to 5 do
		for j := 4 to 5 do
		begin
			if (i <> j) and (PixColor[i] = PixColor[j]) then
			begin
				Result := PixColor[i];
				Exit;
			end;
		end;

	PixColor[0] := Bmp.Canvas.Pixels[0, 0];
	PixColor[1] := Bmp.Canvas.Pixels[Bmp.Width - 1, 0];
	PixColor[2] := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
	PixColor[3] := Bmp.Canvas.Pixels[Bmp.Width - 1, Bmp.Height - 1];

	for i := 0 to 3 do
		for j := 0 to 3 do
		begin
			if (i <> j) and (PixColor[i] = PixColor[j]) then
			begin
				Result := PixColor[i];
				Exit;
			end;
		end;

end;
(*-------------------------------------------------------------------------*)
function ShadowColor(C: TColor): TColor;
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
		TRColor(Result).T := 0;
		if (TRColor(C).R <= 128) and (TRColor(C).G <= 128) and (TRColor(C).B <= 128) then
		begin
			if TRColor(C).B <= 127 then TRColor(Result).B := TRColor(C).B shl 1 else TRColor(Result).B := 255;
			if TRColor(C).G <= 127 then TRColor(Result).G := TRColor(C).G shl 1 else TRColor(Result).G := 255;
			if TRColor(C).R <= 127 then TRColor(Result).R := TRColor(C).R shl 1 else TRColor(Result).R := 255;
		end
		else
		begin
			TRColor(Result).B := (TRColor(C).B + 1) shr 1;
			TRColor(Result).G := (TRColor(C).G + 1) shr 1;
			TRColor(Result).R := (TRColor(C).R + 1) shr 1;
		end;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function ShadowColor2(C1, C2: TColor): TColor;
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
		TRColor(Result).T := 0;
		if (TRColor(C1).R <= 128) and (TRColor(C1).G <= 128) and (TRColor(C1).B <= 128) then
		begin
			if TRColor(C1).B <= 127 then TRColor(Result).B := TRColor(C1).B shl 1 else TRColor(Result).B := 255;
			if TRColor(C1).G <= 127 then TRColor(Result).G := TRColor(C1).G shl 1 else TRColor(Result).G := 255;
			if TRColor(C1).R <= 127 then TRColor(Result).R := TRColor(C1).R shl 1 else TRColor(Result).R := 255;
		end
		else
		begin
			TRColor(Result).B := (TRColor(C1).B + TRColor(C2).B + 1) shr 2;
			TRColor(Result).G := (TRColor(C1).G + TRColor(C2).G + 1) shr 2;
			TRColor(Result).R := (TRColor(C1).R + TRColor(C2).R + 1) shr 2;
		end;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function ColorDiv(Color: TColor; const D: Integer): TColor;
var R, G, B: Integer;
begin
	Color := ColorToRGB(Color);
	R := D * TRColor(Color).R shr 16;
	G := D * TRColor(Color).G shr 16;
	B := D * TRColor(Color).B shr 16;
	if R > 255 then R := 255;
	if G > 255 then G := 255;
	if B > 255 then B := 255;
	TRColor(Result).T := 0;
	TRColor(Result).R := R;
	TRColor(Result).G := G;
	TRColor(Result).B := B;
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
	TRColor(Result).R := TRColor(C).B;
	TRColor(Result).G := TRColor(C).G;
	TRColor(Result).B := TRColor(C).R;
	TRColor(Result).T := TRColor(C).T;
end;
(*-------------------------------------------------------------------------*)
function SpectrumColor(X: Integer): TColor;
//0..255..510..765..1020..1275..1529
begin
	if X > 1529 then X := X mod 1530;
	TRColor(Result).T := 0;
	case X of
	0..255:
	begin
		TRColor(Result).R := 255;
		TRColor(Result).G := X;
		TRColor(Result).B := 0;
	end;
	256..510:
	begin
		TRColor(Result).R := 510 - X;
		TRColor(Result).G := 255;
		TRColor(Result).B := 0;
	end;
	511..765:
	begin
		TRColor(Result).R := 0;
		TRColor(Result).G := 255;
		TRColor(Result).B := X - 510;
	end;
	766..1020:
	begin
		TRColor(Result).R := 0;
		TRColor(Result).G := 1020 - X;
		TRColor(Result).B := 255;
	end;
	1021..1275:
	begin
		TRColor(Result).R := X - 1020;
		TRColor(Result).G := 0;
		TRColor(Result).B := 255;
	end;
	else{1276..1529:}
	begin
		TRColor(Result).R := 255;
		TRColor(Result).G := 0;
		TRColor(Result).B := 1530 - X;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function SpectrumColor2(X: Integer): TColor;
//0..255..510..765..1020..1275..1529
begin
	TRColor(Result).T := 0;
	case X of
	0..127:
	begin
		TRColor(Result).R := 255;
		TRColor(Result).G := 128 + X;
		TRColor(Result).B := 0;
	end;
	128..254:
	begin
		TRColor(Result).R := 509 - X;
		TRColor(Result).G := 255;
		TRColor(Result).B := 0;
	end;
	255..381:
	begin
		TRColor(Result).R := 0;
		TRColor(Result).G := 255;
		TRColor(Result).B := X - 126;
	end;
	382..508:
	begin
		TRColor(Result).R := 0;
		TRColor(Result).G := 763 - X;
		TRColor(Result).B := 255;
	end;
	509..635:
	begin
		TRColor(Result).R := X - 380;
		TRColor(Result).G := 0;
		TRColor(Result).B := 255;
	end;
	else{636..762:}
	begin
		if X > 762 then X := 762;
		TRColor(Result).R := 255;
		TRColor(Result).G := 0;
		TRColor(Result).B := 1017 - X;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function FireColor(X: Integer): TColor;
begin
	TRColor(Result).T := 0;
	case X of
	Low(X)..255:
	begin
		if X < 0 then X := 0;
		TRColor(Result).R := X;
		TRColor(Result).G := 0;
		TRColor(Result).B := 0;
	end;
	256..510:
	begin
		TRColor(Result).R := 255;
		TRColor(Result).G := X - 255;
		TRColor(Result).B := 0;
	end;
	else
	begin
		if X > 765 then X := 765;
		TRColor(Result).R := 255;
		TRColor(Result).G := 255;
		TRColor(Result).B := X - 510;
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function NegColor(C: TColor): TColor;
begin
	C := ColorToRGB(C);
	TRColor(Result).T := 0;
	if TRColor(C).R > 127 then TRColor(Result).R := 0 else TRColor(Result).R := 255;
	if TRColor(C).G > 127 then TRColor(Result).G := 0 else TRColor(Result).G := 255;
	if TRColor(C).B > 127 then TRColor(Result).B := 0 else TRColor(Result).B := 255;
end;
(*-------------------------------------------------------------------------*)
function NegMonoColor(C: TColor): TColor;
begin
	C := ColorToRGB(C);
	if 2 * TRColor(C).R + 4 * TRColor(C).G + 1 * TRColor(C).B > 768 then
		Result := $00000000
	else
		Result := $00FFFFFF;
end;
(*-------------------------------------------------------------------------*)
function DepthColor(const Depth: Byte): TColor;
begin
	case Depth of
	0: 
	begin
		Result := cl3DDkShadow; // Black
	end;
	1:
	begin
		Result := clBtnShadow; // Gray
	end;
	2: 
	begin
		Result := cl3DLight; // Silver
	end;
	else
	begin
		Result := clBtnHighlight; // White
	end;
	end;
end;
(*-------------------------------------------------------------------------*)
function MixColors(C1, C2: TColor): TColor;
begin
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
		TRColor(Result).T := 0;
		TRColor(Result).B := (TRColor(C1).B + TRColor(C2).B) shr 1;
		TRColor(Result).G := (TRColor(C1).G + TRColor(C2).G) shr 1;
		TRColor(Result).R := (TRColor(C1).R + TRColor(C2).R) shr 1;
	end;
end;
(*-------------------------------------------------------------------------*)
function MixColorsEx(C1, C2: TColor; Per1, Per2: Integer): TColor;
begin
	C1 := ColorToRGB(C1);
	C2 := ColorToRGB(C2);
	TRColor(Result).T := 0;
	TRColor(Result).B := (Per1 * TRColor(C1).B + Per2 * TRColor(C2).B) shr 16;
	TRColor(Result).G := (Per1 * TRColor(C1).G + Per2 * TRColor(C2).G) shr 16;
	TRColor(Result).R := (Per1 * TRColor(C1).R + Per2 * TRColor(C2).R) shr 16;
end;
(*-------------------------------------------------------------------------*)
procedure ShadowText(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const CF, CB: TColor);
begin
	if CB = clNone then
		Canvas.Brush.Style := bsClear
	else
	begin
		Canvas.Brush.Style := bsSolid;
		Canvas.Brush.Color := CB;
	end;

	Canvas.Font.Color := ShadowColor(CF);
	Canvas.TextOut(X + 1, Y + 1, Text);

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
procedure DrawCutedText(const Canvas: TCanvas; const Rect: TRect;
	const Alignment: TAlignment; const Layout: TTextLayout; Caption: string; const WordWrap: BG; FontShadow: SG);
var
	i, LastSpace{, k}: Integer;
	Shadow: SG;
	LineS: array of string;
	LineN: SG;
	CurX, CurY: Integer;
	TextHeight: Integer;
	MaxLines: Integer;

	TextBounds: TRect;
	C: TColor;
begin
	TextHeight := Canvas.TextHeight('W');
	MaxLines := (Rect.Bottom - Rect.Top) div TextHeight - 1;
	SetLength(LineS, Length(Caption));
	LineN := 0;
	i := 1;
	LastSpace := 0;
	DelChars(Caption, CharCR);
	while i <= Length(Caption) do
	begin
		if (Caption[i] = ' ') {or (i=Length(Caption))} then
		begin
			LastSpace := i;
		end;

		if (Caption[i] = CharLF) or
			((LineN < MaxLines) and (i > 1) and
			(WordWrap and (Canvas.TextWidth(DelCharsF(Copy(Caption, 1, i), '&')) > Rect.Right - Rect.Left)))
			then
		begin
			if Caption[i] = CharLF then   
			begin
//				if Caption[i + 1] = CharLF then k := 1 else k := 0;
				LineS[LineN] := Copy(Caption, 1, i - 1);
				Delete(Caption, 1, i{ + k});
			end
			else
			if LastSpace = 0 then
			begin
				LineS[LineN] := Copy(Caption, 1, i - 1);
				Delete(Caption, 1, i - 1);
			end
			else
			begin
				LineS[LineN] := Copy(Caption, 1, LastSpace - 1);
				Delete(Caption, 1, LastSpace);
			end;
			LastSpace := 0;
			Inc(LineN);
			i := 0;
		end;
		Inc(i);
		if (i = Length(Caption) + 1) then
		begin
			LineS[LineN] := Caption;
			Inc(LineN);
			Break;
		end;
	end;

	case Layout of
	tlTop: CurY := Rect.Top;
	tlBottom: CurY := Rect.Bottom - TextHeight * LineN;
	else CurY := (Rect.Top + Rect.Bottom - TextHeight * LineN) div 2;
	end;

	for i := 0 to LineN - 1 do
	begin
		if LineS[i] <> '' then
		begin
			case Alignment of
			taLeftJustify: CurX := Rect.Left;
			taRightJustify: CurX := Rect.Right - Canvas.TextWidth(DelCharsF(LineS[i], '&')) - 1;
			else CurX := (Rect.Right + Rect.Left - Canvas.TextWidth(DelCharsF(LineS[i], '&'))) div 2;
			end;
			TextBounds.Left := CurX;
			TextBounds.Top := CurY;
			TextBounds.Right := Rect.Right;
			TextBounds.Bottom := Rect.Bottom; //CurY + TextHeight;
			if FontShadow <> 0 then
			begin
				C := Canvas.Font.Color;
				Canvas.Font.Color := ShadowColor(C);
				Shadow := FontShadow;
				repeat
					OffsetRect(TextBounds, Shadow, Shadow);
					DrawText(Canvas.Handle, PChar(LineS[i]), Length(LineS[i]), TextBounds,
						DT_BOTTOM or DT_LEFT {or DT_NOCLIP}{ or DrawTextBiDiModeFlags(0)});
					OffsetRect(TextBounds, -Shadow, -Shadow);
					if FontShadow > 0 then Dec(Shadow) else Inc(Shadow);
				until Shadow = 0;
				Canvas.Font.Color := C;
			end;
			DrawText(Canvas.Handle, PChar(LineS[i]), Length(LineS[i]), TextBounds,
				DT_BOTTOM or DT_LEFT {or DT_NOCLIP}{ or DrawTextBiDiModeFlags(0)});
//		Canvas.TextOut(CurX, CurY, LineS[i]);
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

procedure BitmapLoadFromFile(Bitmap: TBitmap; FileName: TFileName);
begin
	Bitmap.LoadFromFile(FileName);
end;

procedure InitImage(const Image: TImage; const C: TColor);
begin
	Image.Picture.Bitmap.PixelFormat := pf24bit;
	Image.Picture.Bitmap.Canvas.OnChange := nil; // Must be !!!
	Image.Picture.Bitmap.Canvas.OnChanging := nil;
	Image.Picture.Bitmap.Width := Image.Width;
	Image.Picture.Bitmap.Height := Image.Height;
	if C <> clNone then
	begin
		Image.Picture.Bitmap.Canvas.Brush.Color := C;
		Image.Picture.Bitmap.Canvas.FillRect(Rect(0, 0,
			Image.Picture.Bitmap.Width, Image.Picture.Bitmap.Height));
	end;
end;

end.
