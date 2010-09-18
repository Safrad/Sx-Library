// Build: 10/1999-01/2000 Author: Safranek David

unit uAbout;

interface

uses
	uDForm, uAdd, uDBitmap,
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
	ExtCtrls, uDButton, uDLabel, uDTimer, uDImage;

type
	TfAbout = class(TDForm)
		Timer1: TDTimer;
		ButtonOk: TDButton;
		Bevel5: TBevel;
		Image1: TImage;
		Image2: TImage;
		LabelRunCount: TDLabel;
		LabelNowRunTime: TDLabel;
		LabelTotalRunTime: TDLabel;
    PanelBuild: TEdit;
    PanelRC: TEdit;
    PanelTRT: TEdit;
    PanelNRT: TEdit;
		ImageName: TDImage;
		ImageVersion: TDImage;
		LabelAuthor: TDLabel;
		LabelBuild: TDLabel;
		LabelEMail: TDLabel;
		EditAuthor: TEdit;
		EditWeb: TEdit;
		LabelWeb: TDLabel;
		EditEmail: TEdit;
		Bevel6: TBevel;
		ImageAbout: TDImage;
		Image3: TImage;
		LabelIcq: TDLabel;
		EditIcq: TEdit;
		Image4: TImage;
		SysInfo1: TDButton;
    DButtonMemoryStatus: TDButton;
    DLabel1: TDLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure ButtonOkClick(Sender: TObject);
		procedure ImageAboutMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure EditWebClick(Sender: TObject);
		procedure EditEmailClick(Sender: TObject);
		procedure DTimer1Timer(Sender: TObject);
		procedure EditIcqClick(Sender: TObject);
		procedure SysInfo1Click(Sender: TObject);
		procedure ImageAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure ImageNameFill(Sender: TObject);
		procedure ImageVersionFill(Sender: TObject);
		procedure ImageAboutFill(Sender: TObject);
    procedure DButtonMemoryStatusClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
	private
		Effect: Byte;
		Typ: Byte;
		Reset: Boolean;
		BmpAbout: TDBitmap;
		procedure InitNRT;
	public
		ProgramName: string;
		ProgramVersion: string;
		procedure LoadFile(AboutFile: TFileName);
	end;

procedure ReadMe;
procedure ExecuteAbout(AOwner: TComponent; Version, Build: string;
	FileName: TFileName; const Modal: Boolean);
procedure AboutRW(const Save: Boolean);

var
	RunCount: UG;
	RunTime: U8;
	StartProgramTime: U4;

implementation

{$R *.DFM}
uses
	ShellAPI,
	uGraph, uDIni, uScreen, uSysInfo, uFiles, uError, uData, uWave, uMemStatus;
var
	fAbout: TfAbout;

	LMemClock: U8;

	RunProgramTime: U8;
type
	PFlash = ^TFlash;
	TFlash = packed record // 16
		X, Y: S4;
		Power: S4;
		Color: TColor;
	end;
var
	Flashs: TData;
const
	MaxTyp = 13;

procedure ReadMe;
var
	FileName: TFileName;
	ErrorCode: U4;
begin
	FileName := WorkDir + 'ReadMe.htm';
	ErrorCode := ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_ShowNormal);
	if ErrorCode <= 32 then
		IOError(FileName, ErrorCode);
end;

procedure ExecuteAbout(AOwner: TComponent; Version, Build: string;
	FileName: TFileName; const Modal: Boolean);
var OrigCursor: TCursor;
begin
	PlayWinSound(wsExclamation);
	if not Assigned(fAbout) then
	begin
		OrigCursor := Screen.Cursor;
		Screen.Cursor := crHourGlass;
		fAbout := TfAbout.Create(AOwner);
		fAbout.ProgramName := Application.Title;
		fAbout.ProgramVersion := 'Version ' + Version;
		fAbout.ImageVersion.Bitmap.Canvas.Font.Name := 'MS Sans Serif';
		fAbout.PanelBuild.Text := Build;
		fAbout.LoadFile(FileName);
		Screen.Cursor := OrigCursor;
	end;
	if Modal then
	begin
		fAbout.FormStyle := fsNormal;
		fAbout.ShowModal;
	end
	else
	begin
		fAbout.FormStyle := fsStayOnTop;
		fAbout.Show;
	end;
end;

procedure AboutRW(const Save: Boolean);
var
	Tim: TDateTime;
	FileName: TFileName;
	s: string;
begin
	if Save then
		RunTime := U8(GetTickCount - StartProgramTime) + RunProgramTime;

	if Assigned(MainIni) then
	begin
		MainIni.RWUG('Statistics', 'RunCount', RunCount, Save);
		MainIni.RWU8('Statistics', 'RunTime', RunTime, Save);
		if Save = False then
		begin
			Inc(RunCount);
			StartProgramTime := GetTickCount;
			RunProgramTime := RunTime;
		end;
	end;

	if Save then
		s := 'Finished'
	else
		s := 'Started';
	Tim := Now;
	s := s + ' ' + DateToStr(Tim) + ' ' + TimeToStr(Tim) + #13 + #10;
	FileName := DelFileExt(ExeFileName) + '.log';
	WriteStringToFile(FileName, s, True);

{	LogFile := TFile.Create;
	FileName := DelFileExt(ExeFileName) + '.log';
	LRetry:
	if LogFile.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, True) then
	begin
		LogFile.SeekEnd;
		if Save then
			s := 'Finished'
		else
			s := 'Started';
		s := s + ' ' + DateToStr(Now) + ' ' + TimeToStr(Now);
		if not LogFile.Writeln(s) then goto LRetry;
		if not LogFile.Close then goto LRetry;
	end;
	LogFile.Free;}
end;

procedure TfAbout.InitNRT;
begin
	PanelNRT.Text := msToStr(GetTickCount - StartProgramTime + 1000 div 2, diMSD, 0, False);
end;

procedure TfAbout.LoadFile(AboutFile: TFileName);

	procedure GenBmp;
	const
		GenFunc: array[0..4] of TGenFunc = (gfSpecHorz, gfTriaHorz, gfLineHorz, gfFade2x, gfFade2xx);
	var
		AC: array[0..3] of TColor;
	begin
		BmpAbout.SetSize(64, 64);
		AC[0] := clBtnFace; AC[1] := clBlack; AC[2] := clBtnFace; AC[3] := clWhite;
		BmpAbout.GenerateERGB(clNone,
			GenFunc[RunCount mod (High(GenFunc) + 1)], AC, ScreenCorectColor, ef16, nil);
	end;
begin
	if not Assigned(BmpAbout) then
	begin
		BmpAbout := TDBitmap.Create;
	end;
	if AboutFile = '' then
		GenBmp
	else
	begin
		BmpAbout.LoadFromFile(AboutFile);
		if (BmpAbout.Width = 0) or (BmpAbout.Height = 0) then GenBmp;
	end;
	BmpAbout.TransparentColor := GetTransparentColor(BmpAbout);
end;

procedure TfAbout.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	EditEmail.Text := 'safrad@email.cz?subject=' + Application.Title;
	PanelRC.Text := NToS(RunCount);
	PanelTRT.Text := msToStr(RunTime, diDHMSD, 3, False);

	ImageName.Bitmap.Canvas.Brush.Style := bsClear;
	ImageName.Bitmap.Canvas.Font.Style := [fsBold];
	ImageName.Bitmap.Canvas.Font.Size := 12;
	ImageName.Bitmap.Canvas.Font.Name := 'Times New Roman';
	ImageName.Bitmap.Canvas.Font.Color := clBlack;
	ImageVersion.Bitmap.Canvas.Brush.Style := bsClear;
	ImageVersion.Bitmap.Canvas.Font.Style := [fsBold];

	InitNRT;

	if Assigned(MainIni) then
		MainIni.RWFormPos(Self, False);
end;

procedure TfAbout.FormDestroy(Sender: TObject);
begin
	if Assigned(BmpAbout) then
	begin
		BmpAbout.Free; BmpAbout := nil;
	end;
end;

procedure TfAbout.FormShow(Sender: TObject);
begin
	LMemClock := PerformanceCounter;
	Timer1.Enabled := True;
	DTimer1Timer(Sender);
end;

procedure TfAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Timer1.Enabled := False;
end;

procedure TfAbout.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfAbout.ImageAboutMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
	begin
		if Typ >= MaxTyp then Typ := 0 else Inc(Typ);
	end
	else if Button = mbRight then
	begin
		if Typ <= 0 then Typ := MaxTyp else Dec(Typ);
	end;
end;

procedure TfAbout.EditWebClick(Sender: TObject);
var
	FileName: TFileName;
	ErrorCode: U4;
begin
	FileName := PChar(EditWeb.Text);
	ErrorCode := ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_ShowNormal);
	if ErrorCode <= 32 then
		IOError(FileName, ErrorCode);
end;

procedure TfAbout.EditEmailClick(Sender: TObject);
var
	FileName: TFileName;
	ErrorCode: U4;
begin
	FileName := PChar('mailto: ' + EditEMail.Text);
	ErrorCode := ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_ShowNormal);
	if ErrorCode <= 32 then
		IOError(FileName, ErrorCode);
end;

procedure TfAbout.DTimer1Timer(Sender: TObject);
begin
	if NowTime - LMemClock >= PerformanceFrequency then
	begin
		InitNRT;
		if Assigned(fSysInfo) then
			if fSysInfo.Visible then
			begin
				FillSysInfoD(SysInfo);
				fSysInfo.FillComp;
			end;
		while NowTime - LMemClock >= PerformanceFrequency do
		begin
			Inc(LMemClock, PerformanceFrequency);
		end;
	end;

	ImageAbout.Fill;
	ImageName.Fill;
	ImageVersion.Fill;
end;

procedure TfAbout.EditIcqClick(Sender: TObject);
begin
	ShellExecute(0, 'open', PChar('icq.exe'), nil, nil, SW_ShowNormal);
end;

procedure TfAbout.SysInfo1Click(Sender: TObject);
begin
	if not Assigned(fSysInfo) then fSysInfo := TfSysInfo.Create(Self);
//	fSysInfo.FormStyle := fAbout.FormStyle;
	fSysInfo.Left := fAbout.Left;
	fSysInfo.Top := fAbout.Top;
	FillSysInfoS(SysInfo);
	FillSysInfoD(SysInfo);
	fSysInfo.FillComp;
	fSysInfo.Show;
end;

procedure TfAbout.ImageAboutMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
var
	Flash: PFlash;
begin
//	if Random(10) = 0 then
//	begin
		Flash := Flashs.Add;
		Flash.X := X;
		Flash.Y := Y;
		Flash.Power := 128 + Random(128 + 15);
		Flash.Color := FireColor(256 + Random(256)); // SpectrumColor(Random(MaxSpectrum));
//	end;
end;

procedure TfAbout.ImageNameFill(Sender: TObject);
var
	BitmapName: TDBitmap;
begin
	BitmapName := ImageName.Bitmap;
	BitmapName.GenRGB(clNone, gfSpecHorz, (16 * Timer1.Clock div PerformanceFrequency), ef16);
	BitmapName.BarE24(clNone, clBtnFace, ef12);
	BitmapName.Canvas.TextOut(
		(BitmapName.Width -
		BitmapName.Canvas.TextWidth(ProgramName)) div 2,
		(BitmapName.Height -
		BitmapName.Canvas.TextHeight(ProgramName)) div 2,
		ProgramName);
	BitmapName.Border24(0, 0, BitmapName.Width - 1, BitmapName.Height - 1, clBlack, clWhite, 2, ef08);
end;

procedure TfAbout.ImageVersionFill(Sender: TObject);
var
	BitmapVersion: TDBitmap;
begin
	BitmapVersion := ImageVersion.Bitmap;
	BitmapVersion.BarE24(clNone, clBtnFace, ef16);
	BitmapVersion.Canvas.Font.Color := clBlack;
	BitmapVersion.Canvas.TextOut(
		(BitmapVersion.Width -
		BitmapVersion.Canvas.TextWidth(ProgramVersion)) div 2,
		(BitmapVersion.Height -
		BitmapVersion.Canvas.TextHeight(ProgramVersion)) div 2,
		ProgramVersion);
	BitmapVersion.GenRGB(clBtnFace, gfSpecHorz, (32 * Timer1.Clock div PerformanceFrequency), ef16);
	BitmapVersion.Border24(0, 0, BitmapVersion.Width - 1, BitmapVersion.Height - 1, clBlack, clWhite, 2, ef08);
end;

procedure TfAbout.ImageAboutFill(Sender: TObject);
var
	BitmapAbout: TDBitmap;
	HClock: Byte;
	i: UG;
	Flash: ^TFlash;
begin
	BitmapAbout := ImageAbout.Bitmap;
	BitmapAbout.BarE24(clNone, clBtnFace, ef02);
	HClock := (32 * Timer1.Clock div PerformanceFrequency) and $7f;
	if HClock <= 32  then
	begin
		Effect := HClock shr 1;
		Reset := False;
	end
	else if HClock <= 92 then
		Effect := 16
	else if HClock <= 92 + 32 then
		Effect := (92 + 32) div 2 - HClock shr 1
	else
	begin
		Effect := 0;
		if Reset = False then
		begin
			if Typ = MaxTyp then Typ := 0 else Inc(Typ);
			Reset := True;
		end;
	end;

	if Effect > 0 then
		RotateDefE24(BitmapAbout, BmpAbout, Typ, (AngleCount * Timer1.Clock div (4 * PerformanceFrequency)), BmpAbout.TransparentColor,
			TEffect(Effect));

	i := 0;
	while i < Flashs.Count do
	begin
		Flash := Flashs.Get(i);
		Dec(Flash.Power, 10);
		Inc(Flash.Y, 1);
		if (Flash.Y >= SG(BitmapAbout.Height)) or (Flash.Power <= 0) then
		begin
			Flashs.Delete(i);
		end
		else
		begin
{					C.R := RoundDiv(Flashs[i].Color.R * Flashs[i].Power, 256);
			C.G := RoundDiv(Flashs[i].Color.R * Flashs[i].Power, 256);
			C.B := RoundDiv(Flashs[i].Color.R * Flashs[i].Power, 256);
			C.T := 0;}
			Pix24(BitmapAbout.Data, BitmapAbout.ByteX, Flash.X, Flash.Y, TRColor(Flash.Color),
				TEffect(Flash.Power div 16));
			Inc(i);
		end;
	end;
	BitmapAbout.Border24(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, clBlack, clWhite, 3, ef08);
end;

procedure TfAbout.DButtonMemoryStatusClick(Sender: TObject);
begin
	if not Assigned(fMemStatus) then fMemStatus := TfMemStatus.Create(Self);
	fMemStatus.FormStyle := fAbout.FormStyle;
	fMemStatus.Left := fAbout.Left;
	fMemStatus.Top := fAbout.Top;
	fMemStatus.Show;
end;

procedure TfAbout.FormHide(Sender: TObject);
begin
	if Assigned(MainIni) then
		MainIni.RWFormPos(Self, True);
end;

initialization
	Flashs := TData.Create;
	Flashs.ItemSize := SizeOf(TFlash);
finalization
	Flashs.Free; Flashs := nil;
end.
