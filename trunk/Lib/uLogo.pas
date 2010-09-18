// Build: 10/1999-10/1999 Author: Safranek David

unit uLogo;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls;

type
	TfLogo = class(TForm)
		ImageLogo: TImage;
	private
		{ private declarations }
	public
		{ public declarations }
		procedure LoadFile(LogoFile: TFileName);
	end;

procedure ShowLogo(const FileName: TFileName);
procedure ShowLogoFull;
procedure HideLogo;

implementation

{$R *.DFM}
uses
	Jpeg,
	uGraph, uGraph24;
var
	fLogo: TfLogo;
	LogoTime: LongWord;

procedure TfLogo.LoadFile(LogoFile: TFileName);
var
	B: TBitmap24;
	Quality: Integer;
begin
	ImageLogo.Picture.Bitmap := TBitmap.Create;
	BitmapLoadFromFile(ImageLogo.Picture.Bitmap, LogoFile, 0, 0, Quality);
	if (ImageLogo.Picture.Bitmap.Width = 0) then Exit;
	ClientWidth := ImageLogo.Picture.Bitmap.Width;
	ClientHeight := ImageLogo.Picture.Bitmap.Height;
	B := Conv24(ImageLogo.Picture.Bitmap);
	BorderE24(B, clWhite, clBlack, 8, ef08);
	B.Free;
end;

procedure ShowLogo(const FileName: TFileName);
begin
	Screen.Cursor := crHourGlass;
	fLogo := TfLogo.Create(Application.MainForm);
	fLogo.Position := poDesktopCenter;
	fLogo.ImageLogo.Align := alClient;

	fLogo.LoadFile(FileName);
	if fLogo.ImageLogo.Picture.Bitmap.Width = 0 then Exit;
	fLogo.Show;
	fLogo.Repaint;
	LogoTime := GetTickCount;
end;

procedure ShowLogoFull;
var
	i, li: Integer;
	BackgroundCanvas: TCanvas;
	BackgroundBmp: TBitmap;
	BackgroundBmp24: TBitmap24;
	OutBmp: TBitmap24;
	XCount, YCount: Integer;
begin
	Screen.Cursor := crHourGlass;
	fLogo := TfLogo.Create(Application.MainForm);
//  Application.CreateForm(TfLogo, fLogo);
//  fLogo.LoadFile(FileName);
//  fLogo.Color := clBlack;
	XCount := Screen.Width;
	YCount := Screen.Height;

	fLogo.Left := 0;
	fLogo.Top := 0;
	fLogo.Width := XCount;
	fLogo.Height := YCount;
	fLogo.ImageLogo.Width := XCount;
	fLogo.ImageLogo.Height := YCount;
	fLogo.ImageLogo.Picture.Bitmap.PixelFormat := pf24bit;
	fLogo.ImageLogo.Picture.Bitmap.Width := XCount;
	fLogo.ImageLogo.Picture.Bitmap.Height := YCount;

//  InitImage(fLogo.ImageLogo, clNone);

	BackgroundBmp := TBitmap.Create;
	BackgroundBmp.PixelFormat := pfDevice;
	BackgroundBmp.Width := XCount;
	BackgroundBmp.Height := YCount;

	BackgroundCanvas := TCanvas.Create;
	BackgroundCanvas.Handle := GetDC(0);
	BackgroundBmp.Canvas.CopyRect(
		Rect(0, 0, XCount, YCount), BackgroundCanvas,
		Rect(0, 0, XCount, YCount));
	BackgroundBmp.PixelFormat := pf24bit;
	BackgroundBmp24 := Conv24(BackgroundBmp);
	OutBmp := Conv24(fLogo.ImageLogo.Picture.Bitmap);

	BmpE24(OutBmp, 0, 0, BackgroundBmp24, clNone, ef16);
	fLogo.Show;
	fLogo.ImageLogo.Repaint;
	Sleep(2000);
	LogoTime := GetTickCount;
	i := 0;
	li := 0;
	while i <= 15 * 256 - 1 do
	begin
		BmpE24(OutBmp, 0, 0, BackgroundBmp24, clNone, ef16);
		BarE24(OutBmp, clNone, clBlack, TEffect(i div 256));
		Inc(i, GetTickCount - LogoTime);
		if i div 256 = li div 256 then
		begin
			li := 256 * (li div 256);
			Sleep(i - li);
			Inc(i, i - li);
		end;
		li := i;
		WaitRetrace;
		fLogo.ImageLogo.Repaint;
	end;
	BarE24(OutBmp, clNone, clBlack, ef16);
	WaitRetrace;
	fLogo.ImageLogo.Repaint;
	OutBmp.Free;
	BackgroundBmp24.Free;

	ReleaseDC(0, BackgroundCanvas.Handle);
	BackgroundCanvas.Free;
	BackgroundBmp.Free;

	LogoTime := GetTickCount;
end;

procedure HideLogo;
const
	MinimumTime = 3000;
begin
	if Assigned(fLogo) then
	begin
		LogoTime := GetTickCount - LogoTime;
		{$ifopt d-}
		if LogoTime < MinimumTime then Sleep(MinimumTime - LogoTime);
		{$endif}
		fLogo.Hide;
		fLogo.Free; fLogo := nil;
		Screen.Cursor := crDefault;
	end;
end;

end.
