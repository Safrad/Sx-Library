//* File:     Lib\uSplash.pas
//* Created:  1999-10-01
//* Modified: 2007-05-20
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uSplash;

interface

uses
	uTypes,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, uDTimer, uDForm;

type
	TfSplash = class(TDForm)
		Timer1: TTimer;
		procedure Timer1Timer(Sender: TObject);
		procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure FormCreate(Sender: TObject);
		procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	private
		{ Private declarations }
		FirstX, FirstY: SG;
		MoveCount: SG;

		State: (stWait, stClosing);
		StartTime: U4;

		procedure WantClose;
	public
		{ Public declarations }
	end;

procedure ShowSplashScreen(const ChangeMouseCursor: BG = True); overload; // called as soon as possible
procedure HideSplashScreen(const Promptly: BG = False); // called as late as possible (TfMain.FormShow)

implementation

{$R *.DFM}
uses
	StdCtrls,
	uProjectInfo,
	uGraph, uDBitmap, uFiles, uMath, uSystem, uSimulation, uDIniFile, uOutputFormat, uInputFormat;
var
	fSplash: TfSplash;
const
	MinimumTime = 2 * Second;
	HideTime = 1500 * MiliSecond;
	MaxAlphaBlendValue = 223;

procedure ShowSplashScreen(const FileName: TFileName); overload;
const
	BorderSize = 7;
var
	x, y: SG;
	Co: array[0..3] of TColor;
	Bmp, BmpT: TDBitmap;
	s, LastFontName: string;
//	i: SG;
begin
	if fSplash = nil then
	begin
		if Application.MainForm <> nil then
			fSplash := TfSplash.Create(Application.MainForm)
		else
			fSplash := TfSplash.Create(nil);
	end;
	fSplash.Cursor := crHourGlass;

	Bmp := fSplash.BackBitmap;

	if (FileName <> '') then
	begin
		fSplash.BackBitmap.LoadFromFile(FileName);
		x := fSplash.BackBitmap.Width;
		y := fSplash.BackBitmap.Height;
		if SetNormalSize(x, y, Screen.Width div 2, Screen.Height div 2) then
			fSplash.BackBitmap.Resize(x, y);
	end;
	if (FileName = '') then
	begin
		x := RoundDiv(2 * Screen.Width, 7);
		Bmp.SetSize(x, RoundDiv(3 * x, 4));
		Co[0] := clRed;
		Co[1] := clGreen;
		Co[2] := clBlue;
		Co[3] := clSilver;
		Bmp.GenerateRGB(gfTriaHorz, Co, clBlack, ef16, nil);
		Co[0] := clWhite;
		Co[1] := clBlack;
		Co[2] := Co[0];
		Co[3] := Co[1];
		Bmp.GenerateRGB(gfFade2x, Co, clBlack, ef10, nil);
	end;

	BmpT := TDBitmap.Create;
	BmpT.TransparentColor := clSilver;
	BmpT.SetSize(Bmp.Width, Bmp.Height);
	BmpT.Bar(clSilver, ef16);
	BmpT.Canvas.Brush.Style := bsClear;
	BmpT.Canvas.Font.Style := [fsBold];
	LastFontName := BmpT.Canvas.Font.Name;
	BmpT.Canvas.Font.Name := 'Times New Roman';
	BmpT.Canvas.Font.Height := -38;

	s := GetProjectInfo(piProductVersion);
{	for i := Length(s) downto 2 do
	begin
		if s[i] = '.' then
		begin
			SetLength(s, i - 1);
			Break;
		end;
	end;}
	GoodText(BmpT.Canvas, Rect(16, 16, Bmp.Width - 16, Bmp.Height - 16), GetProjectInfo(piProductName) + ' ' + s,
		clBlack, clWhite, clSilver, taCenter, tlCenter);

	BmpT.Canvas.Font.Name := LastFontName;

	BmpT.Canvas.Font.Style := [];
	BmpT.Canvas.Font.Height := 14;

{	GoodText(BmpT.Canvas, Rect(BorderSize + 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - 4, Bmp.Width - BorderSize, Bmp.Height - BorderSize - 2), 'by ' + GetProjectInfo(piAuthor),
		clNone, clNone, clWhite, taRightJustify, tlCenter);
	GoodText(BmpT.Canvas, Rect(BorderSize + 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - 4, Bmp.Width - BorderSize, Bmp.Height - BorderSize - 2), GetProjectInfo(piRelease),
		clNone, clNone, clWhite, taLeftJustify, tlCenter);}
	ShadowText(BmpT.Canvas, BorderSize + 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - BorderSize - 2, DateToS(SToDate(GetProjectInfo(piRelease), ifIO), ofDisplay),
		clWhite, clNone);
	s := GetProjectInfo(piAuthor);
	if Length(s) > 0 then
		s := 'by ' + s;
	ShadowText(BmpT.Canvas, Bmp.Width - 1 - BmpT.Canvas.TextWidth(s) - BorderSize - 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - BorderSize - 2, s,
		clWhite, clNone);

	Bmp.Bmp(0, 0, BmpT, ef12);
	Bmp.Border(clWhite, clBlack, BorderSize, ef08);

	fSplash.ClientWidth := fSplash.BackBitmap.Width;
	fSplash.ClientHeight := fSplash.BackBitmap.Height;

	// Set alpha after window size (care black blink).
	fSplash.AlphaBlendValue := MaxAlphaBlendValue;
	fSplash.AlphaBlend := True;

	fSplash.Show;
	fSplash.Update;
	Application.HandleMessage; // Process first queue even (change mouse cursor).

	fSplash.State := stWait;
	fSplash.StartTime := GetTickCount;
end;

procedure ShowSplashScreen(const ChangeMouseCursor: BG = True);
var i: SG;
begin
	if ChangeMouseCursor then
		BeginLongOperation;
	for i := Length(AllPictureExt) - 1 downto 0 do
	begin
		if FileExists(GraphDir + 'Logo.' + AllPictureExt[i]) then
		begin
			ShowSplashScreen(GraphDir + 'Logo.' + AllPictureExt[i]);
			Exit;
		end;
	end;
	ShowSplashScreen('');
end;

procedure TfSplash.WantClose;
begin
	if State <> stClosing then
	begin
		State := stClosing;
		StartTime := GetTickCount;
	end;
end;

procedure HideSplashScreen(const Promptly: BG = False);
var
	ElapsedTime: U4;
begin
	if Assigned(fSplash) and fSplash.Visible then
	begin
		fSplash.Cursor := crArrow;
		EndLongOperation(False);

		ElapsedTime := TimeDifference(GetTickCount, fSplash.StartTime);
		if Promptly or (ElapsedTime >= MinimumTime) then
		begin
			fSplash.WantClose;
		end;
	end;
end;

procedure TfSplash.Timer1Timer(Sender: TObject);
var ElapsedTime: U4;
begin
	ElapsedTime := TimeDifference(GetTickCount, StartTime);
	case State of
	stWait:
	begin
		if ElapsedTime >= MinimumTime then
			WantClose;
	end;
	stClosing:
	begin
		if ElapsedTime <= HideTime then
			AlphaBlendValue := RoundDiv(MaxAlphaBlendValue * (HideTime - ElapsedTime), HideTime)
		else
		begin
			Close;
		end;
	end;
	end;
end;

procedure TfSplash.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
begin
	if MoveCount = 0 then
	begin
		FirstX := X;
		FirstY := Y;
	end;
	if Sqr(FirstX - X) + Sqr(FirstY - Y) >= 256 {16 pixels} then
		WantClose;
	Inc(MoveCount);
end;

procedure TfSplash.FormCreate(Sender: TObject);
begin
//	FormStyle := fsNormal;
	Background := baUser;
end;

procedure TfSplash.FormMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	WantClose;
end;

procedure TfSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	EndLongOperation(False);
	BackBitmap.SetSize(0, 0);
end;

procedure TfSplash.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	if State <> stClosing then
	begin
		WantClose;
		CanClose := False;
	end;
end;

end.
