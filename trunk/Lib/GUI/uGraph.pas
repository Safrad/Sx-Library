//* File:     Lib\GUI\uGraph.pas
//* Created:  1999-05-01
//* Modified: 2007-08-20
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uGraph;

interface

uses
	uTypes, uColor,
	Windows, Graphics, StdCtrls, Classes, SysUtils;

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;

procedure PushFont(const Font: TFont);
procedure PopFont(const Font: TFont);

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
	Math, Menus,
	uStrings, uGetInt, uMath;

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;
begin
	Result := (((PixelFormat * X  + 31) and $FFFFFFE0) div 8) * Y;
end;

var
	FontStack: TFont;

procedure CopyFont(const FD, FS: TFont);
begin
	FD.Name := FS.Name;
	FD.PixelsPerInch := FS.PixelsPerInch;
	FD.Charset := FS.Charset;
	FD.Height := FS.Height;
	FD.Color := FS.Color;
	FD.Pitch := FS.Pitch;
	FD.Style := FD.Style;
end;

procedure PushFont(const Font: TFont);
begin
	CopyFont(FontStack, Font);
//	FontStack.Assign(Font);
end;

procedure PopFont(const Font: TFont);
begin
	CopyFont(Font, FontStack);
//	Font.Assign(FontStack);
end;

procedure ShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: string; const CF, CB: TColor);
var n: SG;
begin
	if CB = clNone then
	begin
		Canvas.Brush.Style := bsClear;
		Canvas.Font.Color := DarkerColor(CF);
	end
	else
	begin
		Canvas.Brush.Style := bsSolid;
		Canvas.Brush.Color := CB;
		Canvas.Font.Color := MixColors(CF, CB);
	end;

	n := 1 + Canvas.Font.Size div 16;
	Canvas.TextOut(X + n, Y + n, Text);

	Canvas.Brush.Style := bsClear;
	Canvas.Font.Color := CF;
	Canvas.TextOut(X, Y, Text);
end;

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

procedure CanvasLine(Canvas: TCanvas; const X1, Y1, X2, Y2: Integer);
begin
	Canvas.MoveTo(X1, Y1);
	Canvas.LineTo(X2, Y2);
	Canvas.Pixels[X2, Y2] := Canvas.Pen.Color;
end;

procedure CanvasLineTo(Canvas: TCanvas; const X, Y, OffsetX, OffsetY: Integer);
begin
	Canvas.MoveTo(X, Y);
	Canvas.LineTo(X + OffsetX, Y + OffsetY);
	Canvas.Pixels[X + OffsetX, Y + OffsetY] := Canvas.Pen.Color;
end;

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

function CutText(const Canvas: TCanvas; const Text: string; const Width: SG): string;
var w: SG;
begin
	Result := Text;
	if Canvas.TextWidth(DelCharsF(Result, '&')) > Width then
	begin
		w := Length(Result);
		while w > 1 do
		begin
			if Canvas.TextWidth(DelCharsF(Result, '&') + cDialogSuffix) <= Width then Break;
			Dec(w);
			SetLength(Result, w);
		end;
		Result := Result + cDialogSuffix;
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
		Canvas.Font.Color := $808080;
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
	Lines: array of string;
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
	while i <= Length(Caption) do
	begin
		if Caption[i] = CharSpace then
		begin
			LastSpace := i;
		end;

		if (Caption[i] in [CharCR, CharLF]) or
			((LineCount < MaxLines) and (i > 1) and
			(WordWrap and (Canvas.TextWidth(DelCharsF(Copy(Caption, 1, i), '&')) > Rect.Right - Rect.Left))) then
		begin
			if Caption[i] in [CharCR, CharLF] then
			begin
				if (i <= Length(Caption)) and (Caption[i] = CharCR) and (Caption[i + 1] = CharLF) then
					NewSize := 2
				else
					NewSize := 1;
				Lines[LineCount] := Copy(Caption, 1, i - 1);
				Delete(Caption, NewSize, i);
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

initialization
	FontStack := TFont.Create;
finalization
	FreeAndNil(FontStack);
end.
