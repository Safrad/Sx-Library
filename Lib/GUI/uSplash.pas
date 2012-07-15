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
    procedure FormDestroy(Sender: TObject);
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

var
	AddSplashScreenProjectName: BG = True;

procedure ShowSplashScreen(const ChangeMouseCursor: BG = True); overload; // called as soon as possible
procedure HideSplashScreen(const Promptly: BG = False); // called as late as possible (TfMain.FormShow)

implementation

{$R *.DFM}
uses
	StdCtrls,
	uProjectInfo, uStrings,
	uGraph, uDBitmap, uFiles, uMath, uSystem, uSimulation, uDIniFile, uOutputFormat, uInputFormat, uDrawStyle;
var
	fSplash: TfSplash;
const
	MinimumTime = 2 * Second;
	HideTime = 1500 * MiliSecond;
	MaxAlphaBlendValue = 240; //223;

procedure ShowSplashScreen(const FileName: TFileName); overload;
const
	BorderSize = 7;
var
	x, y: SG;
	R: TRect;
	Co: array[0..3] of TColor;
	Bmp, BmpT: TDBitmap;
	s, LastFontName: string;
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

	R := Screen.WorkAreaRect;
	if (FileName <> '') then
	begin
		fSplash.BackBitmap.LoadFromFile(FileName);
		x := fSplash.BackBitmap.Width;
		y := fSplash.BackBitmap.Height;
		while (x < LgToPx(200)) or (y < LgToPx(3 * 256 div 4)) do
		begin
			x := x * 2;
			y := y * 2;
		end;
		SetSmallerSize(x, y, 3 * (R.Right - R.Left) div 4, 3 * (R.Bottom - R.Top) div 4);
		fSplash.BackBitmap.Resize(x, y);
	end
	else if (FileName = '') then
	begin
		x := LgToPx(512);
		y := LgToPx(384);
		SetSmallerSize(x, y, 3 * (R.Right - R.Left) div 4, 3 * (R.Bottom - R.Top) div 4);
		Bmp.SetSize(x, y, clNone);
		Co[0] := clRed;
		Co[1] := clGreen;
		Co[2] := clBlue;
		Co[3] := clSilver;
		Bmp.GenerateRGB(gfTriaHorz, Co, ef16, nil);
		Co[0] := clWhite;
		Co[1] := clBlack;
		Co[2] := Co[0];
		Co[3] := Co[1];
		Bmp.GenerateRGB(gfFade2x, Co, ef10, nil);
	end;

	BmpT := TDBitmap.Create;
	try
		BmpT.TransparentColor := clSilver;
		BmpT.SetSize(Bmp.Width, Bmp.Height, clSilver);
		BmpT.Canvas.Brush.Style := bsClear;
		if AddSplashScreenProjectName then
		begin
			PushFont(BmpT.Canvas.Font);
			BmpT.Canvas.Font.Style := [fsBold];
			LastFontName := BmpT.Canvas.Font.Name;
			try
				BmpT.Canvas.Font.Name := 'Times New Roman';
				BmpT.Canvas.Font.Size := 20;

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
			finally
				PopFont(BmpT.Canvas.Font);
//				BmpT.Canvas.Font.Name := LastFontName;
//		BmpT.Canvas.Font.Style := [];
//		BmpT.Canvas.Font.Height := 14;
			end;
		end;


//		BmpT.Canvas.Font.Style := [];
		BmpT.Canvas.Font.Name := 'Tahoma';
		BmpT.Canvas.Font.Style := [fsBold];
		BmpT.Canvas.Font.Size := 10;
	{	GoodText(BmpT.Canvas, Rect(BorderSize + 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - 4, Bmp.Width - BorderSize, Bmp.Height - BorderSize - 2), 'by ' + GetProjectInfo(piAuthor),
			clNone, clNone, clWhite, taRightJustify, tlCenter);
		GoodText(BmpT.Canvas, Rect(BorderSize + 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - 4, Bmp.Width - BorderSize, Bmp.Height - BorderSize - 2), GetProjectInfo(piRelease),
			clNone, clNone, clWhite, taLeftJustify, tlCenter);}
		s := DateToS(SToDate(GetProjectInfo(piRelease), ifIO), ofDisplay);
		if not AddSplashScreenProjectName then
		begin
			s := GetProjectInfo(piProductVersion) + CharSpace + '(' + s + ')';
		end;
		ShadowText(BmpT.Canvas, BorderSize + 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - BorderSize - 2, s,
			clWhite, clNone);

		s := GetProjectInfo(piLegalCopyright) + CharSpace + GetProjectInfo(piCompanyName);
		ShadowText(BmpT.Canvas, Bmp.Width - 1 - BmpT.Canvas.TextWidth(s) - BorderSize - 2, Bmp.Height - BmpT.Canvas.TextHeight('W') - BorderSize - 2, s,
				clWhite, clNone);

		Bmp.Bmp(0, 0, BmpT, ef12);
	finally
		FreeAndNil(BmpT);
	end;
	if AddSplashScreenProjectName then
		Bmp.Border(clWhite, clBlack, BorderSize, ef08);

	fSplash.ClientWidth := fSplash.BackBitmap.Width;
	fSplash.ClientHeight := fSplash.BackBitmap.Height;

	// Set alpha after window size (care black blink).
	if AddSplashScreenProjectName then
		fSplash.AlphaBlendValue := MaxAlphaBlendValue
	else
		fSplash.AlphaBlendValue := High(fSplash.AlphaBlendValue);
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

		if Promptly then
		begin
			fSplash.State := stClosing;
			fSplash.StartTime := GetTickCount;
			fSplash.Close;
		end
		else
		begin
			ElapsedTime := TimeDifference(GetTickCount, fSplash.StartTime);
			if (ElapsedTime >= MinimumTime) then
			begin
				fSplash.WantClose;
			end;
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
	BackBitmap.SetSize(0, 0, clNone);
	Timer1.Enabled := False;
	Action := caFree;
end;

procedure TfSplash.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	if State <> stClosing then
	begin
		WantClose;
		CanClose := False;
	end;
end;

procedure TfSplash.FormDestroy(Sender: TObject);
begin
	fSplash := nil;
end;

initialization

finalization
	FreeAndNil(fSplash);
end.
