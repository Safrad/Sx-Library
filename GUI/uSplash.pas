unit uSplash;

interface

uses
	uTypes,
  uTimeSpan,
  uStopwatch,
  uDBitmap,

	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, uDTimer, uDForm, StdCtrls, uSxLabel;

type
	TfSplash = class(TDForm)
		Timer1: TTimer;
    LabelState: TSxLabel;
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
    FFileName: TFileName;
    FPromptlyClose: BG;
    FMinimumTime: TTimeSpan;
    FHideTime: TTimeSpan;
    FMaxAlphaBlendValue: U1;
    FBorderSize: SG;

    function GetBitmapSize: TSize;
    function PrepareBitmap: TDBitmap;
    procedure SetAddSplashScreenProjectName(const Value: BG);
    procedure Initialize;
    procedure AutomaticFileName;
    procedure SetFileName(const Value: TFileName);
    procedure SetPromptlyClose(const Value: BG);
    procedure PlaceBitmapWithTexts(const ATargetBitmap: TDBitmap);
    procedure SetHideTime(const Value: TTimeSpan);
    procedure SetMaxAlphaBlendValue(const Value: U1);
    procedure SetMinimumTime(const Value: TTimeSpan);
    procedure SetBorderSize(const Value: SG);
    procedure FillBitmapWithTexts(var BmpT: TDBitmap; const AWidth, AHeight: SG);
	public
    // Input
	  property MinimumTime: TTimeSpan read FMinimumTime write SetMinimumTime;
	  property HideTime: TTimeSpan read FHideTime write SetHideTime;
    property MaxAlphaBlendValue: U1 read FMaxAlphaBlendValue write SetMaxAlphaBlendValue;
    property AddSplashScreenProjectName: BG read FAddSplashScreenProjectName write SetAddSplashScreenProjectName;
    property FileName: TFileName read FFileName write SetFileName;
    property PromptlyClose: BG read FPromptlyClose write SetPromptlyClose;
    property BorderSize: SG read FBorderSize write SetBorderSize;

    // Process
		procedure WantClose;

    // Output
    property Stopwatch: TStopwatch read FStopwatch;
	end;

implementation

{$R *.DFM}

uses
  uReadImageProperties,
	uProjectInfo,
  uStrings,
	uGraph,
  uFiles,
  uMath,
  uOutputFormat,
  uInputFormat,
  uDrawStyle;

procedure TfSplash.WantClose;
begin
	if State <> stClosing then
	begin
		State := stClosing;
  	Stopwatch.Restart;
	end;
end;

procedure TfSplash.FillBitmapWithTexts(var BmpT: TDBitmap; const AWidth, AHeight: SG);
var
  s: string;
begin
  BmpT.TransparentColor := clSilver;
  BmpT.SetSize(AWidth, AHeight, clSilver);
  BmpT.Canvas.Brush.Style := bsClear;

  if FAddSplashScreenProjectName then
  begin
    // Centered text
    BmpT.Canvas.Font.Style := [fsBold];
    BmpT.Canvas.Font.Name := 'Times New Roman';
    BmpT.Canvas.Font.Size := 20;
    s := GetProjectInfo(piProductVersion);
    GoodText(BmpT.Canvas, Rect(16, 16, BmpT.Width - 16, BmpT.Height - 16), GetProjectInfo(piProductName) + ' ' + s, clBlack, clWhite, clSilver, taCenter, tlCenter);
  end;

  BmpT.Canvas.Font.Name := 'Tahoma';
  BmpT.Canvas.Font.Style := [fsBold];
  BmpT.Canvas.Font.Size := 10;
  s := DateToS(SToDate(GetProjectInfo(piRelease), ifIO), ofDisplay);
  if not FAddSplashScreenProjectName then
  begin
    s := GetProjectInfo(piProductVersion) + CharSpace + '(' + s + ')';
  end;
  // Left bottom text
  ShadowText(BmpT.Canvas, FBorderSize + 2, BmpT.Height - BmpT.Canvas.TextHeight('W') - FBorderSize - 2, s, clWhite, clNone);

  // Right bottom text
  s := GetProjectInfo(piLegalCopyright) + CharSpace + GetProjectInfo(piCompanyName);
  ShadowText(BmpT.Canvas, BmpT.Width - 1 - BmpT.Canvas.TextWidth(s) - FBorderSize - 2, BmpT.Height - BmpT.Canvas.TextHeight('W') - FBorderSize - 2, s, clWhite, clNone);
end;

procedure TfSplash.PlaceBitmapWithTexts(const ATargetBitmap: TDBitmap);
var
  BmpT: TDBitmap;
begin
  BmpT := TDBitmap.Create;
  try
    FillBitmapWithTexts(BmpT, ATargetBitmap.Width, ATargetBitmap.Height);
    ATargetBitmap.Bmp(0, 0, BmpT, ef12);
  finally
    FreeAndNil(BmpT);
  end;
end;

procedure TfSplash.Timer1Timer(Sender: TObject);
begin
	case State of
	stWait:
	begin
		if Stopwatch.Elapsed >= MinimumTime then
			WantClose;
	end;
	stClosing:
	begin
		if Stopwatch.Elapsed <= FHideTime then
			AlphaBlendValue := Round(MaxAlphaBlendValue * (FHideTime.Ticks - Stopwatch.Elapsed.Ticks) / HideTime.Ticks)
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

function TfSplash.GetBitmapSize: TSize;
var
	R: TRect;
  ImageProperties: TImageProperties;
begin
	R := Screen.WorkAreaRect;
	if (FFileName <> '') and ReadImageProperties(FFileName, ImageProperties) then
	begin
    Result.cx := ImageProperties.Width;
    Result.cy := ImageProperties.Height;
    Assert(Result.cx <> 0);
    Assert(Result.cy <> 0);
    while (Result.cx < LgToPx(200)) or (Result.cy < LgToPx(3 * 256 div 4)) do
    begin
      Multiply(Result.cx, 2);
      Multiply(Result.cy, 2);
    end;
  end
  else
  begin
		Result.cx := LgToPx(512);
		Result.cy := LgToPx(384);
  end;
  SetSmallerSize(Result.cx, Result.cy, 3 * (R.Right - R.Left) div 4, 3 * (R.Bottom - R.Top) div 4);
end;

procedure TfSplash.Initialize;
var
	Bmp: TDBitmap;
begin
  if FFileName = '' then
    AutomaticFileName;

	Cursor := crHourGlass;

  Bmp := PrepareBitmap;
  PlaceBitmapWithTexts(Bmp);
	if FAddSplashScreenProjectName then
  begin
    LabelState.Left := FBorderSize;
    LabelState.Top := FBorderSize;
		Bmp.Border(clWhite, clBlack, FBorderSize, ef08);
  end;

	// Set alpha after window size (care black blink).
	if FAddSplashScreenProjectName then
		AlphaBlendValue := MaxAlphaBlendValue
	else
		AlphaBlendValue := High(AlphaBlendValue);
	AlphaBlend := True;

	State := stWait;
	Stopwatch.Restart;
end;

function TfSplash.PrepareBitmap: TDBitmap;
var
  Size: TSize;
	Co: array[0..3] of TColor;
begin
  Size := GetBitmapSize;

  SetBounds(Left, Top, Size.cx, Size.cy);
	Result := BackBitmap;

	if (FFileName <> '') then
	begin
		Result.LoadFromFile(FileName);
		Result.Resize(Size.cx, Size.cy);
	end
	else
	begin
		Result.SetSize(Size.cx, Size.cy, clNone);
		Co[0] := clRed;
		Co[1] := clGreen;
		Co[2] := clBlue;
		Co[3] := clSilver;
		Result.GenerateRGB(gfTriaHorz, Co, ef16, nil);
		Co[0] := clWhite;
		Co[1] := clBlack;
		Co[2] := Co[0];
		Co[3] := Co[1];
		Result.GenerateRGB(gfFade2x, Co, ef10, nil);
	end;
end;

procedure TfSplash.SetAddSplashScreenProjectName(const Value: BG);
begin
  FAddSplashScreenProjectName := Value;
end;

procedure TfSplash.SetBorderSize(const Value: SG);
begin
  FBorderSize := Value;
end;

procedure TfSplash.SetHideTime(const Value: TTimeSpan);
begin
  FHideTime := Value;
end;

procedure TfSplash.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TfSplash.SetMaxAlphaBlendValue(const Value: U1);
begin
  FMaxAlphaBlendValue := Value;
end;

procedure TfSplash.SetMinimumTime(const Value: TTimeSpan);
begin
  FMinimumTime := Value;
end;

procedure TfSplash.SetPromptlyClose(const Value: BG);
begin
  FPromptlyClose := Value;
end;

procedure TfSplash.FormCreate(Sender: TObject);
begin
	Background := baUser;

	FMinimumTime.Milliseconds := 2000;
	FHideTime.Milliseconds := 1500;
	FMaxAlphaBlendValue := 240;
  FAddSplashScreenProjectName := True;
  FBorderSize := LgToPx(7);

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
    if Stopwatch.Elapsed >= MinimumTime then
    begin
      WantClose;
    end;
  end;
end;

end.
