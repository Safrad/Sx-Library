unit uSmallRasterFont;

interface

uses
  UITypes,
  uTypes,
  uDBitmap,
  uDrawStyle;

type
	TRasterFontStyle = (fs6x8, fs8x8, fs8x16);

const
	FontWidth: array[TRasterFontStyle] of SG = (6, 8, 8);
	FontHeight: array[TRasterFontStyle] of SG = (8, 8, 16);

procedure FTextOut(
  const ABitmap: TDBitmap;
  const X, Y: SG;
  const RasterFontStyle: TRasterFontStyle;
  const FontColor, BackColor: TColor;
  const Effect: TEffect;
  const Text: string);

implementation

uses
  SysUtils,
  uColor,
  uSystemPaths;

const
	FontNames: array[TRasterFontStyle] of string = ('06x08', '08x08', '08x16');
var
	FontBitmap: array[TRasterFontStyle] of TDBitmap;
	FontRead: array[TRasterFontStyle] of Boolean;
	Letter: TDBitmap;

procedure FreeFontBitmap;
var i: TRasterFontStyle;
begin
	for i := Low(i) to High(i) do
		if FontRead[i] then FreeAndNil(FontBitmap[i]);
	FreeAndNil(Letter);
end;

procedure FTextOut(
  const ABitmap: TDBitmap;
  const X, Y: SG;
	const RasterFontStyle: TRasterFontStyle;
  const FontColor, BackColor: TColor;
  const Effect: TEffect;
  const Text: string);
var
	c, i, FX: SG;
	CB: TColor;
	FontColorR: TRGBA;
begin
	FontColorR := ABitmap.ColorToRGB(FontColor);
	if FontRead[RasterFontStyle] = False then
	begin
		FontBitmap[RasterFontStyle] := TDBitmap.Create;
		FontBitmap[RasterFontStyle].LoadFromFile(SystemPaths.GraphDir + 'Font' + FontNames[RasterFontStyle] + IconExt);
		FontBitmap[RasterFontStyle].Transparent := False;
		FontRead[RasterFontStyle] := True;
	end;
	if (FontBitmap[RasterFontStyle] = nil) or (FontBitmap[RasterFontStyle].Data = nil) then
    Exit;
	if Letter = nil then
    Letter := TDBitmap.Create;
	Letter.SetSize(FontBitmap[RasterFontStyle].Width * 255, FontHeight[RasterFontStyle], TColorRec.SysNone);
	case BackColor of
	TColorRec.SysNone:
	begin
		Letter.Transparent := False;
		Letter.TransparentColor := BackColor;
	end
	else
	begin
		Letter.Transparent := True;
		Letter.TransparentColor := BackColor;
	end;
	end;

	FX := 0;
	for i := 1 to Length(Text) do
	begin
		c := Ord(Ord(Text[i]) - Ord(' '));
		Letter.Bmp(FX, 0, FontBitmap[RasterFontStyle],
			0, FontHeight[RasterFontStyle] * c,
			FontBitmap[RasterFontStyle].Width, FontHeight[RasterFontStyle] * c + FontHeight[RasterFontStyle] - 1, ef16);
		Inc(FX, FontBitmap[RasterFontStyle].Width);
	end;

	case BackColor of
	TColorRec.SysNone:
	begin
		if FontColor = FontBitmap[RasterFontStyle].TransparentColor then
		begin
			Letter.ChangeColor(FontBitmap[RasterFontStyle].TransparentColor, TColorRec.Silver);
			CB := TColorRec.Silver;
		end
		else
			CB := FontBitmap[RasterFontStyle].TransparentColor;
		if FontColor <> TColorRec.White then
      Letter.ChangeColor(TColorRec.White, FontColor)
	end
	else
	begin
		CB := TColorRec.SysNone;
		if FontColor = FontBitmap[RasterFontStyle].TransparentColor then
		begin
			if BackColor <> FontBitmap[RasterFontStyle].TransparentColor then
        Letter.ChangeColor(FontBitmap[RasterFontStyle].TransparentColor, BackColor);
			Letter.ChangeColor(TColorRec.White, FontColor);
		end
		else
		begin
			if FontColor <> TColorRec.White then
        Letter.ChangeColor(TColorRec.White, FontColor);
			if BackColor <> FontBitmap[RasterFontStyle].TransparentColor then
        Letter.ChangeColor(FontBitmap[RasterFontStyle].TransparentColor, BackColor);
		end;
	end;
	end;
	Letter.Transparent := True;
	Letter.TransparentColor := CB;
	ABitmap.Bmp(X, Y, Letter, 0, 0, FontBitmap[RasterFontStyle].Width * Length(Text) - 1, FontHeight[RasterFontStyle] - 1, Effect);
end;

initialization

finalization
{$IFNDEF NoFinalization}
	FreeFontBitmap;
{$ENDIF NoFinalization}
end.
