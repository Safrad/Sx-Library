// Build: 10/1999-10/1999 Author: Safranek David

unit uLogo;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, uDTimer, uDForm;

type
	TfLogo = class(TDForm)
    Timer1: TDTimer;
		procedure Timer1Timer(Sender: TObject);
		procedure ImageLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
    procedure FormCreate(Sender: TObject);
	private
		{ private declarations }
		MoveCount: SG;
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
	uGraph, uDBitmap;
var
	fLogo: TfLogo;
	LogoTime: LongWord;

procedure TfLogo.LoadFile(LogoFile: TFileName);
begin
	BackBitmap.LoadFromFile(LogoFile);
	if BackBitmap.Empty then Exit;
	ClientWidth := BackBitmap.Width;
	ClientHeight := BackBitmap.Height;
	BackBitmap.BorderE24(clWhite, clBlack, 8, ef08);
end;

procedure ShowLogo(const FileName: TFileName);
begin
//  {$ifopt d-}
	Screen.Cursor := crHourGlass;
	fLogo := TfLogo.Create(Application.MainForm);

	fLogo.LoadFile(FileName);
	if fLogo.BackBitmap.Empty then Exit;
	fLogo.Show;
//	fLogo.BackImage.Repaint;
	fLogo.Paint;
	LogoTime := GetTickCount;
//  {$endif}
end;

procedure ShowLogoFull;
var
	i, li: Integer;
	BackgroundCanvas: TCanvas;
	BackgroundBmp: TDBitmap;
	OutBmp: TDBitmap;
	XCount, YCount: Integer;
begin
	Screen.Cursor := crHourGlass;
	fLogo := TfLogo.Create(Application.MainForm);
//  Application.CreateForm(TfLogo, fLogo);
//  fLogo.LoadFile(FileName);
//  fLogo.Color := clBlack;
	fLogo.FullScreen := True;

	XCount := Screen.Width;
	YCount := Screen.Height;

{	fLogo.BackImage.Width := XCount;
	fLogo.BackImage.Height := YCount;
	fLogo.BackImage.Picture.Bitmap.PixelFormat := pf24bit;
	fLogo.BackImage.Picture.Bitmap.Width := XCount;
	fLogo.BackImage.Picture.Bitmap.Height := YCount;}

//  InitImage(fLogo.ImageLogo, clNone);

	BackgroundBmp := TDBitmap.Create;
	BackgroundBmp.SetSize(XCount, YCount);

	BackgroundCanvas := TCanvas.Create;
	BackgroundCanvas.Handle := GetDC(0);
	BackgroundBmp.Canvas.CopyRect(
		Rect(0, 0, XCount, YCount), BackgroundCanvas,
		Rect(0, 0, XCount, YCount));
	OutBmp := fLogo.BackBitmap;

	OutBmp.BmpE24(0, 0, BackgroundBmp, clNone, ef16);
	fLogo.Show;
	fLogo.Paint;
//	Sleep(2000);
	LogoTime := GetTickCount;
	i := 0;
	li := 0;
	while i <= 15 * 256 - 1 do
	begin
		OutBmp.BmpE24(0, 0, BackgroundBmp, clNone, ef16);
		OutBmp.BarE24(clNone, clBlack, TEffect(i div 256));
		Inc(i, GetTickCount - LogoTime);
		if i div 256 = li div 256 then
		begin
			li := 256 * (li div 256);
			Sleep(i - li);
			Inc(i, i - li);
		end;
		li := i;
		fLogo.Paint;
	end;
	OutBmp.BarE24(clNone, clBlack, ef16);
	fLogo.Paint;
	BitmapFree(BackgroundBmp);

	ReleaseDC(0, BackgroundCanvas.Handle);
	BackgroundCanvas.Free;
	BackgroundBmp.Free;

	LogoTime := GetTickCount;
	fLogo.FullScreen := False;
	fLogo.Close;
end;

procedure HideLogo;
const
	MinimumTime = 3000;
begin
	if Assigned(fLogo) then
	begin
		LogoTime := GetTickCount - LogoTime;
		if LogoTime < MinimumTime then
		begin
			fLogo.Timer1.Interval := MinimumTime - LogoTime;
			fLogo.Timer1.Enabled := True;
		end;
		Screen.Cursor := crDefault;
	end;
end;

procedure TfLogo.Timer1Timer(Sender: TObject);
begin
	Timer1.Enabled := False;
	BackBitmap.SetSize(0, 0);
	Hide;
//	Free; fLogo := nil;
end;

procedure TfLogo.ImageLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
begin
	if MoveCount > 8 then
		Timer1.Interval := 1
	else
		Inc(MoveCount);
end;

procedure TfLogo.FormCreate(Sender: TObject);
begin
	Background := baUser;
end;

end.
