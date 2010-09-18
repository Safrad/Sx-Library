//* File:     Lib\uLogo.pas
//* Created:  1999-10-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

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
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		{ Private declarations }
		FirstX, FirstY: SG;
		MoveCount: SG;
	public
		{ Public declarations }
	end;

procedure ShowLogo(const FileName: TFileName); overload;
procedure ShowLogo; overload;
//procedure ShowLogoFull;
procedure HideLogo;

implementation

{$R *.DFM}
uses
	StdCtrls,
	uGraph, uDBitmap, uFiles;
var
	fLogo: TfLogo;
	LogoTime: LongWord;

procedure ShowLogo(const FileName: TFileName);
var
	x: SG;
	Co: array[0..3] of TColor;
	Bmp, BmpT: TDBitmap;
begin
	Screen.Cursor := crHourGlass;
	if Application.MainForm <> nil then
		fLogo := TfLogo.Create(Application.MainForm)
	else
		fLogo := TfLogo.Create(nil);

	Bmp := fLogo.BackBitmap;

	if (FileName <> '') then
	begin
		fLogo.BackBitmap.LoadFromFile(FileName);
	end;
	if (FileName = '') then
	begin
		x := RoundDiv(2 * Screen.Width, 7);
		Bmp.SetSize(x, RoundDiv(3 * x, 4));
		Co[0] := clRed;
		Co[1] := clGreen;
		Co[2] := clBlue;
		Co[3] := clSilver;
		Bmp.GenerateERGB(clNone, gfTriaHorz, Co, clBlack, ef16, nil);
		Co[0] := clWhite;
		Co[1] := clBlack;
		Co[2] := Co[0];
		Co[3] := Co[1];
		Bmp.GenerateERGB(clNone, gfFade2x, Co, clBlack, ef10, nil);
	end;

	BmpT := TDBitmap.Create;
	BmpT.SetSize(Bmp.Width, Bmp.Height);
	BmpT.BarE24(clNone, clSilver, ef16);
	BmpT.Canvas.Brush.Style := bsClear;
	BmpT.Canvas.Font.Style := [fsBold];
	BmpT.Canvas.Font.Name := 'Times New Roman';
	BmpT.Canvas.Font.Height := -48;

	GoodText(BmpT.Canvas, Rect(16, 16, Bmp.Width - 16, Bmp.Height - 16), Application.Title,
		clBlack, clWhite, clSilver, taCenter, tlCenter);

{ BmpT.Canvas.Font.Name := 'Arial';

	BmpT.Canvas.Font.Height := -16;
	Text := 'Author: Safranek David';
	GoodText(BmpT.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), Text,
		clWhite, clBlack, clPurple, taLeftJustify, tlCenter);

	Text := 'Build: 04/1999-01/2000';
	GoodText(BmpT.Canvas, Rect(0, 96, Bmp.Width, Bmp.Height), Text,
		clBlack, clWhite, clPurple, taLeftJustify, tlCenter);}


	Bmp.BmpE24(0, 0, BmpT, clSilver, ef08);

	Bmp.BorderE24(clWhite, clBlack, 8, ef08);

	fLogo.ClientWidth := fLogo.BackBitmap.Width;
	fLogo.ClientHeight := fLogo.BackBitmap.Height;
	fLogo.Show;
//	fLogo.BackImage.Repaint;
	fLogo.Paint;
	LogoTime := GetTickCount;
end;

procedure ShowLogo;
begin
	if FileExists(GraphDir + 'Logo.jpg') then
		ShowLogo(GraphDir + 'Logo.jpg')
	else
		ShowLogo('');
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
	fLogo.FullScreen := True;

	XCount := Screen.Width;
	YCount := Screen.Height;

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
	if Assigned(fLogo) and fLogo.Visible then
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
//  Timer1.Free; Problem D???
	Close;
	Hide;
//	Free; fLogo := nil;
end;

procedure TfLogo.ImageLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
begin
	if MoveCount = 0 then
	begin
		FirstX := X;
		FirstY := Y;
	end;
	if Sqr(FirstX - X) + Sqr(FirstY - Y) >= 256 {16 pixels} then
	begin
		Close;
		Free; fLogo := nil;
	end;

	Inc(MoveCount);
end;

procedure TfLogo.FormCreate(Sender: TObject);
begin
	Background := baUser;
end;

procedure TfLogo.FormMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	Close;
	Free; fLogo := nil;
end;

procedure TfLogo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if fLogo.Timer1.Enabled then
	begin
		Timer1.Enabled := False;
	end
	else
		Screen.Cursor := crDefault;
	BackBitmap.SetSize(0, 0);
end;

end.
