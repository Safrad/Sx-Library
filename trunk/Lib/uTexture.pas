// Build: 11/1999-11/1999 Author: Safranek David

unit uTexture;

interface

uses
	ExtCtrls, Graphics,
	uGraph24;

procedure FormBitmap24(const Bitmap24: TBitmap24);
procedure FormBitmap(const Bitmap: TBitmap);
procedure FormImage(const Image: TImage);
procedure GenerateTexture(const Image: TImage; const TextureType: Integer);

procedure CreateTexture(const Image: TImage; const Visible: Boolean;
	const TextureType: Integer);
procedure SetVisible(const Image: TImage; const Visible: Boolean;
	const TextureType: Integer);


implementation

uses uGraph, uScreen;

procedure FormBitmap24(const Bitmap24: TBitmap24);
var
	Co: array[0..3] of TColor;
begin
	Co[0] := LighterColor(clBtnFace);
	Co[1] := DarkerColor(clBtnFace);
	Co[2] := Co[0];
	Co[3] := Co[1];
	GenerateERGB(Bitmap24,
		clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
end;

procedure FormBitmap(const Bitmap: TBitmap);
var
	B: TBitmap24;
begin
	B := Conv24(Bitmap);
	FormBitmap24(B);
	B.Free;
end;

procedure FormImage(const Image: TImage);
begin
	InitImage(Image, clNone);
	FormBitmap(Image.Picture.Bitmap);
end;

procedure GenerateTexture(const Image: TImage; const TextureType: Integer);
var
	AC: array[0..3] of TColor;
	B: TBitmap24;
begin
	InitImage(Image, clNone);
	B := Conv24(Image.Picture.Bitmap);
	AC[0] := clRed; AC[1] := clLime; AC[2] := clBlue; AC[3] := clBtnFace;
	GenerateERGB(B, clNone, TGenFunc(TextureType mod 10), AC, ScreenCorectColor, ef16, nil);
	Bar24(B, clNone, 0, 0,
		Image.Picture.Bitmap.Width - 1, Image.Picture.Bitmap.Height - 1,
		clBtnFace, ef12);
	B.Free;
end;

procedure CreateTexture(const Image: TImage; const Visible: Boolean;
	const TextureType: Integer);
begin
	if Visible then GenerateTexture(Image, TextureType);
end;

procedure SetVisible(const Image: TImage; const Visible: Boolean;
	const TextureType: Integer);
begin
	if Visible then
	begin
		if Image.Picture.Width = 0 then GenerateTexture(Image, TextureType);
		Image.Visible := True;
	end
	else
		Image.Visible := False;
end;

end.
