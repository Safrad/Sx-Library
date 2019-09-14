unit uSplash;

interface

uses
	uTypes,
  uStopwatch,
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
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
	private
		FirstX, FirstY: SG;
		MoveCount: SG;

		State: (stWait, stClosing);
		FStopwatch: TStopwatch;
    FAddSplashScreenProjectName: BG;
    FChangeMouseCursor: BG;
    FFileName: TFileName;
    FPromptlyClose: BG;

    procedure SetAddSplashScreenProjectName(const Value: BG);
    procedure Initialize;
    procedure SetChangeMouseCursor(const Value: BG);
    procedure AutomaticFileName;
    procedure SetFileName(const Value: TFileName);
    procedure SetPromptlyClose(const Value: BG);
	public
    property Stopwatch: TStopwatch read FStopwatch;
    property AddSplashScreenProjectName: BG read FAddSplashScreenProjectName write SetAddSplashScreenProjectName;
    property FileName: TFileName read FFileName write SetFileName;
    property PromptlyClose: BG read FPromptlyClose write SetPromptlyClose;
		procedure WantClose;
	end;

implementation

{$R *.DFM}

uses
	StdCtrls,
	uProjectInfo, uStrings,
	uGraph, uDBitmap, uFiles, uMath, uDIniFile, uOutputFormat, uInputFormat, uDrawStyle;

// TODO : property
const
	MinimumTimeInMs = 2000;
	HideTimeInMs = 1500;
	MaxAlphaBlendValue = 240;

procedure TfSplash.WantClose;
begin
	if State <> stClosing then
	begin
		State := stClosing;
  	Stopwatch.Restart;
	end;
end;

procedure TfSplash.Timer1Timer(Sender: TObject);
begin
	case State of
	stWait:
	begin
		if Stopwatch.Elapsed.Milliseconds >= MinimumTimeInMs then
			WantClose;
	end;
	stClosing:
	begin
		if Stopwatch.Elapsed.Milliseconds <= HideTimeInMs then
			AlphaBlendValue := Round(MaxAlphaBlendValue * (HideTimeInMs - Stopwatch.Elapsed.Milliseconds) / HideTimeInMs)
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

procedure TfSplash.FormShow(Sender: TObject);
begin
  Initialize;
end;

procedure TfSplash.Initialize;
const
	BorderSize = 7;
var
	x, y: SG;
	R: TRect;
	Co: array[0..3] of TColor;
	Bmp, BmpT: TDBitmap;
	s, LastFontName: string;
begin
  if FFileName = '' then
    AutomaticFileName;

	Cursor := crHourGlass;

	Bmp := BackBitmap;

	R := Screen.WorkAreaRect;
	if (FileName <> '') then
	begin
		BackBitmap.LoadFromFile(FileName);
		x := BackBitmap.Width;
		y := BackBitmap.Height;
		while (x < LgToPx(200)) or (y < LgToPx(3 * 256 div 4)) do
		begin
			x := x * 2;
			y := y * 2;
		end;
		SetSmallerSize(x, y, 3 * (R.Right - R.Left) div 4, 3 * (R.Bottom - R.Top) div 4);
		BackBitmap.Resize(x, y);
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
		if FAddSplashScreenProjectName then
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
		if not FAddSplashScreenProjectName then
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
	if FAddSplashScreenProjectName then
		Bmp.Border(clWhite, clBlack, BorderSize, ef08);

	ClientWidth := BackBitmap.Width;
	ClientHeight := BackBitmap.Height;

	// Set alpha after window size (care black blink).
	if FAddSplashScreenProjectName then
		AlphaBlendValue := MaxAlphaBlendValue
	else
		AlphaBlendValue := High(AlphaBlendValue);
	AlphaBlend := True;

	Show;
	Update;
	Application.HandleMessage; // Process first queue even (change mouse cursor).

	State := stWait;
	Stopwatch.Restart;
end;

procedure TfSplash.SetAddSplashScreenProjectName(const Value: BG);
begin
  FAddSplashScreenProjectName := Value;
end;

procedure TfSplash.SetChangeMouseCursor(const Value: BG);
begin
  FChangeMouseCursor := Value;
end;

procedure TfSplash.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TfSplash.SetPromptlyClose(const Value: BG);
begin
  FPromptlyClose := Value;
end;

procedure TfSplash.FormCreate(Sender: TObject);
begin
//	FormStyle := fsNormal;
	Background := baUser;

  FAddSplashScreenProjectName := True;
  FChangeMouseCursor := True;
  FStopwatch := TStopwatch.Create;
end;

procedure TfSplash.FormMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	WantClose;
end;

procedure TfSplash.AutomaticFileName;
var
  i: SG;
  F: TFileName;
begin
	for i := Length(AllPictureExt) - 1 downto 0 do
	begin
    F := GraphDir + 'Logo.' + AllPictureExt[i];
		if FileExists(F) then
		begin
			FFileName := F;
			Exit;
		end;
	end;
end;

procedure TfSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Timer1.Enabled := False;
	BackBitmap.SetSize(0, 0, clNone);
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
  FStopwatch.Free;
end;

procedure TfSplash.FormHide(Sender: TObject);
begin
  Cursor := crArrow;

  if FPromptlyClose then
  begin
    State := stClosing;
    Close;
  end
  else
  begin
    if (Stopwatch.Elapsed.Milliseconds >= MinimumTimeInMs) then
    begin
      WantClose;
    end;
  end;
end;

end.
