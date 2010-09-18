unit uFonts;

interface

uses
	Graphics,
	uGraph24;

type
	TFontStyle = (fs6x8, fs8x8, fs8x16);

procedure FTextOut(Bitmap: TBitmap24; X, Y: Integer;
	FontStyle: TFontStyle; FontColor, BackColor: TColor; Effect: TEffect; Text: string);

implementation

uses
	uFiles, uGraph;
const
	FontNames: array[TFontStyle] of string = ('06x08', '08x08', '08x16');
	FontHeight: array[TFontStyle] of Integer = (8, 8, 16);
var
	FontBitmap: array[TFontStyle] of TBitmap24;
	FontReaded: array[TFontStyle] of Boolean;
	Letter: TBitmap24;

procedure FTextOut(Bitmap: TBitmap24; X, Y: Integer;
	FontStyle: TFontStyle; FontColor, BackColor: TColor; Effect: TEffect; Text: string);
var
	c: Integer;
	i: Integer;
	CB: TColor;
begin
	FontColor := ColorToRGB(FontColor);
	if FontReaded[FontStyle] = False then
	begin
		Bitmap24ReadFromFile(FontBitmap[FontStyle], SharedDir + FontNames[FontStyle] + '.bmp');
		FontReaded[FontStyle] := True;
	end;
	if FontBitmap[FontStyle].PData = nil then Exit;
	ResizeBitmap24(Letter, FontBitmap[FontStyle].Width, FontHeight[FontStyle]);
	for i := 1 to Length(Text) do
	begin
		c := Ord(Ord(Text[i]) - Ord(' '));
		Bmp24(Letter, 0, 0, FontBitmap[FontStyle],
			0, FontHeight[FontStyle] * c,
			FontBitmap[FontStyle].Width, FontHeight[FontStyle] * c + FontHeight[FontStyle] - 1, clNone, ef16);

		case BackColor of
		clNone:
		begin
			if FontColor = clBlack then
			begin
				ChangeColorE24(Letter, clBlack, clSilver);
				CB := clSilver;
			end
			else
				CB := clBlack;
			if FontColor <> clWhite then ChangeColorE24(Letter, clWhite, FontColor)
		end
		else
		begin
			CB := clNone;
			if FontColor = clBlack then
			begin
				if BackColor <> clBlack then ChangeColorE24(Letter, clBlack, BackColor);
				ChangeColorE24(Letter, clWhite, FontColor);
			end
			else
			begin
				if FontColor <> clWhite then ChangeColorE24(Letter, clWhite, FontColor);
				if BackColor <> clBlack then ChangeColorE24(Letter, clBlack, BackColor);
			end;
		end;
		end;

		Bmp24(Bitmap, X, Y, Letter,
			0, 0,
			FontBitmap[FontStyle].Width - 1, FontHeight[FontStyle] - 1, CB, Effect);
		Inc(X, FontBitmap[FontStyle].Width);
	end;
end;

procedure FreeFontBitmap;
var i: TFontStyle;
begin
	for i := Low(i) to High(i) do
		if FontReaded[i] then FontBitmap[i].Free;
	Letter.Free; Letter := nil;
end;

initialization
	Letter := TBitmap24.Create;
finalization
	FreeFontBitmap;
end.
