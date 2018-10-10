unit uGraph;

interface

uses
  uTypes, uColor,
  uTextAlignment,
  Windows, Graphics, StdCtrls, Classes, SysUtils;

type
  TRectArray = array of TRect;

  TCutTextMode = (ctNone, ctEllipsis, ctTriangle);
  // TODO : Implement ctTransparent

var
  CutTextMode: TCutTextMode = ctEllipsis;

function GetBmpSize(const X, Y: UG; const PixelFormat: U1): UG;

procedure PushFont(const Font: TFont);

procedure PopFont(const Font: TFont);

procedure SmallFont(const Font: TFont);

procedure IdealFont(const Text: string; const Canvas: TCanvas; const R: TRect; const Font: TFont);

function IdealShadow(const Canvas: TCanvas): SG;

procedure ShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: string; const CF, CB: TColor);

procedure GoodText(Canvas: TCanvas; R: TRect; Text: string; const C1, C2, C3: TColor; const Alignment: TAlignment; const
  Layout: TTextLayout);

procedure CanvasLine(Canvas: TCanvas; const X1, Y1, X2, Y2: Integer); deprecated;

procedure CanvasLineTo(Canvas: TCanvas; const X, Y, OffsetX, OffsetY: Integer); deprecated;

procedure Rec(Canvas: TCanvas; const Rect: TRect; const Color: TColor; const Width: Integer); deprecated;

procedure Border(Canvas: TCanvas; const Rect: TRect; TopColor, BottomColor: TColor; const Width: Integer); deprecated;

function CutText(const Canvas: TCanvas; const Text: string; const Width: SG): string;

function DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: string; const FontShadow: SG = 0; const Alignment:
  TAlignment = taLeftJustify): BG; overload;

function DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: string; const FontShadow: SG = 0; const AHorizontalAlignment:
  THorizontalAlignment = haLeft): BG; overload;

procedure DrawShadowText(const Canvas: TCanvas; X, Y: SG; const Text: string; const FontShadow: SG = 0); overload;

function DrawCuttedText(const ACanvas: TCanvas; const ARect: TRect; const ATextAlignment: TTextAlignment;
  ACaption: string; const AWordWrap: BG; const AFontShadow: SG): BG; overload;

function DrawCuttedText(const Canvas: TCanvas; const Rect: TRect; const Alignment: TAlignment; const Layout: TTextLayout;
  Caption: string; const WordWrap: BG; const FontShadow: SG): BG; overload;

function FloPoint(const X, Y: Double): TFloPoint;

function FloPointToPoint(const FloPoint: TFloPoint): TPoint;

function Over(const SX1, SY1, SX2, SY2: Integer; const DX1, DY1, DX2, DY2: Integer): Boolean; overload;

function Over(const SX1, SY1, SX2, SY2: Double; const DX1, DY1, DX2, DY2: Double): Boolean; overload;

function Over(const SX1, SY1: SG; const DR: TRect): Boolean; overload;

function Over(const S: TPoint; const DR: TRect): Boolean; overload;

function Over3D(const SX1, SY1, SZ1, SX2, SY2, SZ2: Integer; const DX1, DY1, DZ1, DX2, DY2, DZ2: Integer): Boolean;
  overload;
{function OverE(const SX1, SY1, SX2, SY2: Extended;
	const DX1, DY1, DX2, DY2: Extended): Boolean; overload;}

procedure InflatePoint(var P: TPoint; const d: SG); overload;

procedure InflatePoint(var P: TPoint; const dx, dy: SG); overload;

function CenterOfRect(const R: TRect): TPoint;

function SortRectArray(const ASource: TRectArray; const Selection: TPoint; const FarerFirst: BG): TRectArray;

procedure RandomizeRectArray(const AResult: TRectArray);

procedure RandomizeRangeArray(const AResult: TRangeArray);

implementation

uses
  Math, Menus,
  uStrings, uChar, uMath, uSorts;

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
  if Text = '' then
    Exit;
  RectWidth := R.Right - R.Left + 1;
  RectHeight := R.Bottom - R.Top + 1;

  FontSize := Max(MinFontSize, Abs(2 * RectWidth div Length(Text)));
  Canvas.Font.Height := -FontSize;
  Font.Height := -FontSize;

  Size := Canvas.TextExtent(Text);

  WidthTouch := Size.cx * RectHeight >= RectWidth * Size.cy;

  if WidthTouch then
    FontSize := FontSize * RectWidth div Size.cx + 1
  else
    FontSize := FontSize * RectHeight div Size.cy + 1;

  while FontSize >= MinFontSize do
  begin
    Canvas.Font.Height := -FontSize;
    Font.Height := -FontSize;
    Size := Canvas.TextExtent(Text);
    if (Size.cx <= RectWidth) and (Size.cy <= RectHeight) then
      Break;
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

procedure GoodText(Canvas: TCanvas; R: TRect; Text: string; const C1, C2, C3: TColor; const Alignment: TAlignment; const
  Layout: TTextLayout);
begin
  Canvas.Font.Color := MixColors(C1, C2);
  DrawCuttedText(Canvas, Rect(R.Left + 1, R.Top - 1, R.Right + 1, R.Bottom - 1), Alignment, Layout, Text, True, 0);

  Canvas.Font.Color := MixColors(C1, C2);
  DrawCuttedText(Canvas, Rect(R.Left - 1, R.Top + 1, R.Right - 1, R.Bottom + 1), Alignment, Layout, Text, True, 0);

  Canvas.Font.Color := C1;
  DrawCuttedText(Canvas, Rect(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1), Alignment, Layout, Text, True, 0);

  Canvas.Font.Color := C2;
  DrawCuttedText(Canvas, Rect(R.Left - 1, R.Top - 1, R.Right - 1, R.Bottom - 1), Alignment, Layout, Text, True, 0);

  Canvas.Font.Color := C3;
  DrawCuttedText(Canvas, Rect(R.Left, R.Top, R.Right, R.Bottom), Alignment, Layout, Text, True, 0);
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

procedure Rec(Canvas: TCanvas; const Rect: TRect; const Color: TColor; const Width: Integer);
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
    CanvasLine(Canvas, X1 + i, Y1 + i, X2 - i - 1, Y1 + i); //-
    CanvasLine(Canvas, X1 + i, Y1 + i + 1, X1 + i, Y2 - i); //|
    CanvasLine(Canvas, X1 + i + 1, Y2 - i, X2 - i, Y2 - i); //-
    CanvasLine(Canvas, X2 - i, Y1 + i, X2 - i, Y2 - i - 1); //|
  end;
end;

procedure Border(Canvas: TCanvas; const Rect: TRect; TopColor, BottomColor: TColor; const Width: Integer);
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
    CanvasLine(Canvas, X1 + i, Y1 + i, X2 - i - 1, Y1 + i); //-
    CanvasLine(Canvas, X1 + i, Y1 + i + 1, X1 + i, Y2 - i - 1); //|}
    Canvas.Pen.Color := BottomColor;
    CanvasLine(Canvas, X1 + i + 1, Y2 - i, X2 - i, Y2 - i); //-
    CanvasLine(Canvas, X2 - i, Y1 + i + 1, X2 - i, Y2 - i - 1); //|
    TopColor := ColorToRGB(TopColor);
    BottomColor := ColorToRGB(BottomColor);
    Canvas.Pixels[X1 + i, Y2 - i] := MixColors(TopColor, BottomColor);
    Canvas.Pixels[X2 - i, Y1 + i] := MixColors(TopColor, BottomColor);
  end;
end;

function CutText(const Canvas: TCanvas; const Text: string; const Width: SG): string;
var
  w: SG;
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
      if Canvas.TextWidth(Result) <= Width then
        Exit;
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

procedure DrawLeftArrow(const Canvas: TCanvas; R: TRect);
begin
  Canvas.Font.Color := clRed;
  Canvas.Brush.Style := bsClear;
  DrawTextW(Canvas.Handle, CharLeftawardsArrow, Length(CharLeftawardsArrow), R, DT_NOCLIP or DT_SINGLELINE);
end;

procedure DrawRightArrow(const Canvas: TCanvas; R: TRect);
begin
  Canvas.Font.Color := clRed;
  Canvas.Brush.Style := bsClear;
  DrawTextW(Canvas.Handle, CharRightawardsArrow, Length(CharRightawardsArrow), R, DT_NOCLIP or DT_SINGLELINE or DT_RIGHT);
end;

function DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: string; const FontShadow: SG = 0; const AHorizontalAlignment:
  THorizontalAlignment = haLeft): BG; overload;
var
  Alignment: TAlignment;
begin
  case AHorizontalAlignment of
  haLeft: Alignment := taLeftJustify;
  haCenter: Alignment := taCenter;
  else {haRight:} Alignment := taRightJustify;
  end;
  Result := DrawShadowText(Canvas, R, Text, FontShadow, Alignment);
end;

function DrawShadowText(const Canvas: TCanvas; R: TRect; const Text: string; const FontShadow: SG = 0; const Alignment:
  TAlignment = taLeftJustify): BG; overload;
var
  C: TColor;
  B: TBrushStyle;
  TextOffset: SG;
  TextWidth: SG;
  CuttedText: string;
begin
  case CutTextMode of
    ctEllipsis:
      CuttedText := CutText(Canvas, Text, R.Right - R.Left + 1);
  else
    CuttedText := Text;
  end;

  Result := Text <> CuttedText;

  TextWidth := Canvas.TextWidth(RemoveSingleAmp(CuttedText)) + FontShadow;
  if Alignment <> taLeftJustify then
  begin
    case Alignment of
//			taLeftJustify: CurX := R.Left;
      taRightJustify:
        R.Left := R.Right + 1 - TextWidth;
    else {taCenter}
      R.Left := R.Left + (R.Right - R.Left + 1 - TextWidth) div 2;
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
  	if FontShadow <> 0 then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := $808080;
      OffsetRect(R, TextOffset, TextOffset);
      DrawText(Canvas.Handle, PChar(CuttedText), Length(CuttedText), R, DT_NOCLIP or DT_SINGLELINE);
      OffsetRect(R, -TextOffset, -TextOffset);
    end;
    Canvas.Brush.Style := B;
    Canvas.Font.Color := C;
    DrawText(Canvas.Handle, PChar(CuttedText), Length(CuttedText), R, DT_NOCLIP or DT_SINGLELINE);
(* Font depth
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

      if TextOffset = 0 then
        Break;

      if TextOffset > 0 then
        Dec(TextOffset)
      else
        Inc(TextOffset);
    end;
*)
    if CutTextMode = ctTriangle then
    begin
      if TextWidth > R.Right - R.Left + 1 then
      begin
        case Alignment of
          taLeftJustify:
            DrawRightArrow(Canvas, R);
          taRightJustify:
            DrawLeftArrow(Canvas, R);
          taCenter:
            begin
              DrawLeftArrow(Canvas, R);
              DrawRightArrow(Canvas, R);
            end;
        end;
      end;
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
var
  R: TRect;
begin
  R.Left := X;
  R.Top := Y;
  R.Right := High(R.Right) div 2;
  R.Bottom := High(R.Bottom) div 2;
  DrawShadowText(Canvas, R, Text, FontShadow, taLeftJustify);
end;

function DrawCuttedText(const Canvas: TCanvas; const Rect: TRect; const Alignment: TAlignment; const Layout: TTextLayout;
  Caption: string; const WordWrap: BG; const FontShadow: SG): BG;
var
  TextAlignment: TTextAlignment;
begin
  case Alignment of
    taLeftJustify: TextAlignment.Horizontal := haLeft;
    taCenter: TextAlignment.Horizontal := haCenter;
    else {taRightJustify} TextAlignment.Horizontal := haRight;
  end;

  case Layout of
    tlTop: TextAlignment.Vertical := vaTop;
    tlCenter: TextAlignment.Vertical := vaCenter;
    else {tlBottom} TextAlignment.Vertical := vaBottom;
  end;

  Result := DrawCuttedText(Canvas, Rect, TextAlignment, Caption, WordWrap, FontShadow);
end;

function DrawCuttedText(const ACanvas: TCanvas; const ARect: TRect; const ATextAlignment: TTextAlignment;
  ACaption: string; const AWordWrap: BG; const AFontShadow: SG): BG; overload;
const
  Border = 0;
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
  TextHeight := Max(ACanvas.TextHeight('W'), 1);
  MaxLines := (ARect.Bottom - ARect.Top) div TextHeight - 1;
  SetLength(Lines, 1);
  LineCount := 0;
  i := 1;
  LastSpace := 0;
  while i <= Length(ACaption) do
  begin
    if ACaption[i] = CharSpace then
    begin
      LastSpace := i;
    end;

    if CharInSet(ACaption[i], [CharCR, CharLF]) or ((LineCount < MaxLines) and (i > 1) and (AWordWrap and (ACanvas.TextWidth
      (RemoveSingleAmp(Copy(ACaption, 1, i))) > ARect.Right - ARect.Left + 1))) then
    begin
      if CharInSet(ACaption[i], [CharCR, CharLF]) then
      begin
        if (i <= Length(ACaption)) and (ACaption[i] = CharCR) and (ACaption[i + 1] = CharLF) then
          NewSize := 2
        else
          NewSize := 1;
        Lines[LineCount] := Copy(ACaption, 1, i - 1);
        Delete(ACaption, NewSize, i);
      end
      else if LastSpace = 0 then
      begin
        Lines[LineCount] := Copy(ACaption, 1, i - 1);
        Delete(ACaption, 1, i - 1);
      end
      else
      begin
        Lines[LineCount] := Copy(ACaption, 1, LastSpace - 1);
        Delete(ACaption, 1, LastSpace);
      end;
      LastSpace := 0;
      NewSize := LineCount + 2;
      if AllocByExp(Length(Lines), NewSize) then
        SetLength(Lines, NewSize);
      Inc(LineCount);
      i := 0;
    end;
    Inc(i);
    if (i = Length(ACaption) + 1) then
    begin
      NewSize := LineCount + 2;
      if AllocByExp(Length(Lines), NewSize) then
        SetLength(Lines, NewSize);
      Lines[LineCount] := ACaption;
      Inc(LineCount);
      Break;
    end;
  end;

  case ATextAlignment.Vertical of
    vaTop:
      CurY := ARect.Top;
    vaBottom:
      CurY := ARect.Bottom + 1 - TextHeight * LineCount;
  else {vaCenter}
    CurY := ARect.Top + (ARect.Bottom - ARect.Top + 1 - TextHeight * LineCount) div 2;
  end;

  for i := 0 to LineCount - 1 do
  begin
    if Lines[i] <> '' then
    begin
      R.Left := ARect.Left;
      R.Top := CurY;
      R.Right := ARect.Right;
      R.Bottom := CurY + TextHeight; //Rect.Bottom;
      if DrawShadowText(ACanvas, R, Lines[i], AFontShadow, ATextAlignment.Horizontal) then
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

function Over(const SX1, SY1, SX2, SY2: Integer; const DX1, DY1, DX2, DY2: Integer): Boolean; overload;
begin
  Result := False;

  if (SX1 > DX2) and (SX2 > DX2) then
    Exit;
  if (SX1 < DX1) and (SX2 < DX1) then
    Exit;
  if (SY1 > DY2) and (SY2 > DY2) then
    Exit;
  if (SY1 < DY1) and (SY2 < DY1) then
    Exit;
  Result := True;
end;

function Over(const SX1, SY1, SX2, SY2: Double; const DX1, DY1, DX2, DY2: Double): Boolean; overload;
begin
  Result := False;

  if (SX1 > DX2) and (SX2 > DX2) then
    Exit;
  if (SX1 < DX1) and (SX2 < DX1) then
    Exit;
  if (SY1 > DY2) and (SY2 > DY2) then
    Exit;
  if (SY1 < DY1) and (SY2 < DY1) then
    Exit;
  Result := True;
end;

function Over(const SX1, SY1: SG; const DR: TRect): Boolean; overload;
begin
  Result := False;

  if (SX1 > DR.Right) then
    Exit;
  if (SX1 < DR.Left) then
    Exit;
  if (SY1 > DR.Bottom) then
    Exit;
  if (SY1 < DR.Top) then
    Exit;
  Result := True;
end;

function Over(const S: TPoint; const DR: TRect): Boolean; overload;
begin
  Result := False;

  if (S.X > DR.Right) then
    Exit;
  if (S.X < DR.Left) then
    Exit;
  if (S.Y > DR.Bottom) then
    Exit;
  if (S.Y < DR.Top) then
    Exit;
  Result := True;
end;

function Over3D(const SX1, SY1, SZ1, SX2, SY2, SZ2: Integer; const DX1, DY1, DZ1, DX2, DY2, DZ2: Integer): Boolean;
  overload;
begin
  Result := False;

  if (SX1 > DX2) and (SX2 > DX2) then
    Exit;
  if (SX1 < DX1) and (SX2 < DX1) then
    Exit;
  if (SY1 > DY2) and (SY2 > DY2) then
    Exit;
  if (SY1 < DY1) and (SY2 < DY1) then
    Exit;
  if (SZ1 > DZ2) and (SZ2 > DZ2) then
    Exit;
  if (SZ1 < DZ1) and (SZ2 < DZ1) then
    Exit;
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


procedure RandomizeRectArray(const AResult: TRectArray);
var
  T: TRect;
  Count: SG;
  i, X: SG;
begin
  Count := Length(AResult);
  for i := 0 to Count - 1 do
  begin
    X := Random(Count);
    T := AResult[i];
    AResult[i] := AResult[X];
    AResult[X] := T;
  end;
end;

procedure RandomizeRangeArray(const AResult: TRangeArray);
var
  T: TRange;
  Count: SG;
  i, X: SG;
begin
  Count := Length(AResult);
  for i := 0 to Count - 1 do
  begin
    X := Random(Count);
    T := AResult[i];
    AResult[i] := AResult[X];
    AResult[X] := T;
  end;
end;

function GetSquaredDistance(const A, B: TPoint): SG;
begin
	Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function SortRectArray(const ASource: TRectArray; const Selection: TPoint; const FarerFirst: BG): TRectArray;
var
  AIndex: TArrayOfSG;
  SquaredDistanceFormSelection: TArrayOfS4;
  i: SG;
begin
  SetLength(AIndex, Length(ASource));
  FillOrderUG(AIndex[0], Length(ASource));
  SetLength(SquaredDistanceFormSelection, Length(ASource));

  for i := 0 to Length(ASource) - 1 do
  begin
    SquaredDistanceFormSelection[i] := GetSquaredDistance(Selection, CenterOfRect(ASource[i]));
  end;

  SortS4(False, FarerFirst, PArraySG(AIndex), PArrayS4(SquaredDistanceFormSelection), Length(ASource));

  SetLength(Result, Length(ASource));
  for i := 0 to Length(ASource) - 1 do
  begin
    Result[i] := ASource[AIndex[i]];
  end;
end;

initialization
{$IFNDEF NoInitialization}
  FontStack := TFont.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(FontStack);
{$ENDIF NoFinalization}

end.

