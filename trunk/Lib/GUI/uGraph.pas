unit uGraph;

interface

uses
	uTypes, uColor,
	Windows, Graphics, StdCtrls, Classes, SysUtils;

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;

procedure PushFont(const Font: TFont);
procedure PopFont(const Font: TFont);
procedure SmallFont(const Font: TFont);
procedure IdealFont(const Text: string; const Canvas: TCanvas; const R: TRect; const Font: TFont);

function IdealShadow(const Canvas: TCanvas): SG;
procedure ShadowText(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const CF, CB: TColor);
procedure GoodText(Canvas: TCanvas; R: TRect; Text: string;
	const C1, C2, C3: TColor; const Alignment: TAlignment; const Layout: TTextLayout);
procedure CanvasLine(Canvas: TCanvas;
	const X1, Y1, X2, Y2: Integer); deprecated;
procedure CanvasLineTo(Canvas: TCanvas;
	const X, Y, OffsetX, OffsetY: Integer); deprecated;
procedure Rec(Canvas: TCanvas; const Rect: TRect;
	const Color: TColor; const Width: Integer); deprecated;
procedure Border(Canvas: TCanvas; const Rect: TRect;
	TopColor, BottomColor: TColor; const Width: Integer); deprecated;

function CutText(const Canvas: TCanvas; const Text: string; const Width: SG): string;
function DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: string; const FontShadow: SG = 0; const Alignment: TAlignment = taLeftJustify): BG; overload;
procedure DrawShadowText(const Canvas: TCanvas; X, Y: SG; const Text: string; const FontShadow: SG = 0); overload;
function DrawCuttedText(const Canvas: TCanvas; const Rect: TRect;
	const Alignment: TAlignment; const Layout: TTextLayout; Caption: string; const WordWrap: BG; FontShadow: SG): BG;

function FloPoint(const X, Y: Double): TFloPoint;
function FloPointToPoint(const FloPoint: TFloPoint): TPoint;
function Over(const SX1, SY1, SX2, SY2: Integer;
	const DX1, DY1, DX2, DY2: Integer): Boolean; overload;
function Over(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;
function Over(const SX1, SY1: SG; const DR: TRect): Boolean; overload;
function Over(const S: TPoint; const DR: TRect): Boolean; overload;

function Over3D(const SX1, SY1, SZ1, SX2, SY2, SZ2: Integer;
	const DX1, DY1, DZ1, DX2, DY2, DZ2: Integer): Boolean; overload;
{function OverE(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;}
procedure InflatePoint(var P: TPoint; const d: SG); overload;
procedure InflatePoint(var P: TPoint; const dx, dy: SG); overload;
function CenterOfRect(const R: TRect): TPoint;

implementation

uses
	Math, Menus, {$if CompilerVersion >= 23}System.UITypes,{$ifend}
	uStrings, uMath;

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;
begin
	if PixelFormat = 15 then
		Result := 0
	else
		Result := (((PixelFormat * U8(X) + 31) and $FFFFFFFFFFFFFFE0) div 8) * Y;
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

const
	MinFontSize = 4;

procedure SmallFont(const Font: TFont);
var
	NewHeight: SG;
begin
{	if Font.Height > 0 then
		NewHeight := Font.Height - 3
	else
		NewHeight := Font.Height + 3;}
	NewHeight := 3 * Font.Height div 4;
	if Abs(NewHeight) >= MinFontSize then
		Font.Height := NewHeight
	else
		Font.Height := MinFontSize * Sgn(Font.Height);
end;

procedure IdealFont(const Text: string; const Canvas: TCanvas; const R: TRect; const Font: TFont);
var
	FontSize: SG;
	Size: TSize;
  WidthTouch: BG;
  RectWidth, RectHeight: SG;
begin
  if Text = '' then Exit;
  RectWidth := R.Right - R.Left;
  RectHeight := R.Bottom - R.Top;

	FontSize := Max(MinFontSize, Abs(2 * (R.Right - R.Left) div Length(Text)));
	Canvas.Font.Height := -FontSize;
	Font.Height := -FontSize;

	Size := Canvas.TextExtent(Text);

  WidthTouch := Size.cx * RectHeight >= RectWidth * Size.cy;

  if WidthTouch then
    FontSize := FontSize * RectWidth div Size.cx
  else
    FontSize := FontSize * RectHeight div Size.cy;

	while FontSize >= MinFontSize do
	begin
		Canvas.Font.Height := -FontSize;
		Font.Height := -FontSize;
		Size := Canvas.TextExtent(Text);
		if (Size.cx <= R.Right - R.Left) and (Size.cy <= R.Bottom - R.Top) then Break;
		Dec(FontSize);
	end;
end;

procedure ShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: string; const CF, CB: TColor);
var
	n: SG;
	HLS: THLSColor;
begin
	if CB = clNone then
	begin
		Canvas.Brush.Style := bsClear;
		HLS := RGBToHLS(TRGBA(ColorToRGB(CF)));
		if HLS.L >= 128 then
			HLS.L := HLS.L - 64
//			Canvas.Font.Color := DarkerColor(CF)
		else
			HLS.L := HLS.L + 64;
//			Canvas.Font.Color := LighterColor(CF);
		Canvas.Font.Color := TColor(HLSToRGB(HLS));
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
	DrawCuttedText(Canvas, Rect(R.Left + 1, R.Top - 1, R.Right + 1, R.Bottom - 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := MixColors(C1, C2);
	DrawCuttedText(Canvas, Rect(R.Left - 1, R.Top + 1, R.Right - 1, R.Bottom + 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := C1;
	DrawCuttedText(Canvas, Rect(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := C2;
	DrawCuttedText(Canvas, Rect(R.Left - 1, R.Top - 1, R.Right - 1, R.Bottom - 1),
		Alignment, Layout, Text, True, 0);

	Canvas.Font.Color := C3;
	DrawCuttedText(Canvas, Rect(R.Left, R.Top, R.Right, R.Bottom),
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
	if Canvas.TextWidth(RemoveSingleAmp(Result)) > Width then
	begin
		w := Length(Result);
		while w >= 1 do
		begin
			if Canvas.TextWidth(RemoveSingleAmp(Result) + cDialogSuffix) <= Width then
			begin
				Result := Result + cDialogSuffix;
				Exit;
			end;
			Dec(w);
			SetLength(Result, w);
		end;

		Result := cDialogSuffix;
		w := Length(Result);
		while w >= 1 do
		begin
			if Canvas.TextWidth(Result) <= Width then Exit;
			Dec(w);
			SetLength(Result, w);
		end;
	end;
end;

function IdealShadow(const Canvas: TCanvas): SG;
begin
	Result := Abs(RoundDiv(Canvas.Font.Height, 20));
//	Result := RoundN(Abs(Canvas.Font.Size) div 16);
end;

function DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: string; const FontShadow: SG = 0; const Alignment: TAlignment = taLeftJustify): BG; overload;
var
	C: TColor;
	B: TBrushStyle;
	TextOffset: SG;
	TextWidth: SG;
	CuttedText: string;
begin
	CuttedText := CutText(Canvas, Text, R.Right - R.Left);
	Result := Text <> CuttedText;

	if Alignment <> taLeftJustify then
	begin
		TextWidth := Canvas.TextWidth(RemoveSingleAmp(CuttedText)) + FontShadow;
		case Alignment of
//			taLeftJustify: CurX := R.Left;
		taRightJustify: R.Left := R.Right + 1 - TextWidth;
		else {taCenter} R.Left := R.Left + (R.Right - R.Left + 1 - TextWidth) div 2;
		end;
//		if CurX < Rect.Left + Border then CurX := Rect.Left + Border;
	end;

{	if FontShadow <> 0 then
	begin}
		B := Canvas.Brush.Style;
		C := Canvas.Font.Color;
//	end;
	try
		TextOffset := FontShadow;
		while True do
		begin
			if FontShadow <> 0 then
			begin
				if TextOffset <> 0 then
				begin
					Canvas.Brush.Style := bsClear;
					Canvas.Font.Color := $808080;
				end
				else
				begin
					Canvas.Brush.Style := B;
					Canvas.Font.Color := C;
				end;
			end;

			OffsetRect(R, TextOffset, TextOffset);
	//			ExtTextOut(Canvas.Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
			DrawText(Canvas.Handle, PChar(CuttedText), Length(CuttedText), R, DT_NOCLIP or DT_SINGLELINE);
			OffsetRect(R, -TextOffset, -TextOffset);

			if TextOffset = 0 then Break;

			if TextOffset > 0 then
				Dec(TextOffset)
			else
				Inc(TextOffset);
		end;
	finally
		if FontShadow <> 0 then
		begin
			Canvas.Font.Color := C;
			Canvas.Brush.Style := B;
		end;
	end;
//	ExtTextOut(Canvas.Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
//	DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_NOCLIP or DT_SINGLELINE);
end;

procedure DrawShadowText(const Canvas: TCanvas; X, Y: SG; const Text: string; const FontShadow: SG = 0); overload;
var R: TRect;
begin
	R.Left := X;
	R.Top := Y;
	R.Right := High(R.Right) div 2;
	R.Bottom := High(R.Bottom) div 2;
	DrawShadowText(Canvas, R, Text, FontShadow, taLeftJustify);
end;

function DrawCuttedText(const Canvas: TCanvas; const Rect: TRect;
	const Alignment: TAlignment; const Layout: TTextLayout; Caption: string; const WordWrap: BG; FontShadow: SG): BG;
const Border = 0;
var
	i, LastSpace{, k}: Integer;
	Lines: array of string;
	LineCount: SG;
	CurY: Integer;
	TextHeight: Integer;
	MaxLines: Integer;
	R: TRect;
	NewSize: SG;
begin
	Result := False;
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

		if CharInSet(Caption[i], [CharCR, CharLF]) or
			((LineCount < MaxLines) and (i > 1) and
			(WordWrap and (Canvas.TextWidth(RemoveSingleAmp(Copy(Caption, 1, i))) > Rect.Right - Rect.Left))) then
		begin
			if CharInSet(Caption[i], [CharCR, CharLF]) then
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
			R.Left := Rect.Left;
			R.Top :=  CurY;
			R.Right := Rect.Right;
			R.Bottom := CurY + TextHeight; //Rect.Bottom;
			if DrawShadowText(Canvas, R, Lines[i], FontShadow, Alignment) then
				Result := True;
		end;
		Inc(CurY, TextHeight);
	end;
end;

function FloPoint(const X, Y: Double): TFloPoint;
begin
	Result.X := X;
	Result.Y := Y;
end;

function FloPointToPoint(const FloPoint: TFloPoint): TPoint;
begin
	Result.X := Trunc(FloPoint.X);
	Result.Y := Trunc(FloPoint.Y);
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

function Over(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;
begin
	Result := False;

	if (SX1 > DX2) and (SX2 > DX2) then Exit;
	if (SX1 < DX1) and (SX2 < DX1) then Exit;
	if (SY1 > DY2) and (SY2 > DY2) then Exit;
	if (SY1 < DY1) and (SY2 < DY1) then Exit;
	Result := True;
end;

function Over(const SX1, SY1: SG; const DR: TRect): Boolean; overload;
begin
	Result := False;

	if (SX1 > DR.Right) then Exit;
	if (SX1 < DR.Left) then Exit;
	if (SY1 > DR.Bottom) then Exit;
	if (SY1 < DR.Top) then Exit;
	Result := True;
end;

function Over(const S: TPoint; const DR: TRect): Boolean; overload;
begin
	Result := False;

	if (S.X > DR.Right) then Exit;
	if (S.X < DR.Left) then Exit;
	if (S.Y > DR.Bottom) then Exit;
	if (S.Y < DR.Top) then Exit;
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

{function OverE(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;
begin
	Result := False;

	if (SX1 > DX2) and (SX2 > DX2) then Exit;
	if (SX1 < DX1) and (SX2 < DX1) then Exit;
	if (SY1 > DY2) and (SY2 > DY2) then Exit;
	if (SY1 < DY1) and (SY2 < DY1) then Exit;
	Result := True;
end;}

procedure InflatePoint(var P: TPoint; const d: SG);
begin
	Inc(P.X, d);
	Inc(P.Y, d);
end;

procedure InflatePoint(var P: TPoint; const dx, dy: SG);
begin
	Inc(P.X, dx);
	Inc(P.Y, dy);
end;

function CenterOfRect(const R: TRect): TPoint;
begin
	Result.X := (R.Right + R.Left) div 2;
	Result.Y := (R.Bottom + R.Top) div 2;
end;

initialization
	FontStack := TFont.Create;
finalization
	FreeAndNil(FontStack);
end.
