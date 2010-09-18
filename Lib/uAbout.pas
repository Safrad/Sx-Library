// Build: 10/1999-01/2000 Author: Safranek David

unit uAbout;

interface

uses
	uAdd, uGraph24,
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
	ExtCtrls, uDPanel, uDBitBtn, uDLabel;

type
	TfAbout = class(TForm)
    Timer1: TTimer;
		BitBtnOk: TDBitBtn;
		ImageBackground: TImage;
		Bevel5: TBevel;
		Image1: TImage;
		Image2: TImage;
		LabelRunCount: TDLabel;
		LabelNowRunTime: TDLabel;
		LabelTotalRunTime: TDLabel;
		PanelBuild: TDPanel;
		PanelImage: TDPanel;
		PanelRC: TDPanel;
		PanelTRT: TDPanel;
		PanelNRT: TDPanel;
		PanelName: TDPanel;
		ImageName: TImage;
		PenelVersion: TDPanel;
		ImageVersion: TImage;
		LabelAuthor: TDLabel;
		LabelBuild: TDLabel;
		LabelEMail: TDLabel;
		EditAuthor: TEdit;
		EditWeb: TEdit;
		LabelWeb: TDLabel;
		EditEmail: TEdit;
		Bevel6: TBevel;
		ImageAbout: TImage;
		Image3: TImage;
		LabelIcq: TDLabel;
		EditIcq: TEdit;
		Image4: TImage;
		SysInfo1: TDBitBtn;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure BitBtnOkClick(Sender: TObject);
		procedure ImageAboutMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure EditWebClick(Sender: TObject);
		procedure EditEmailClick(Sender: TObject);
		procedure Timer1Timer(Sender: TObject);
		procedure EditIcqClick(Sender: TObject);
		procedure SysInfo1Click(Sender: TObject);
	private
		{ private declarations }
		BitmapName, BitmapVersion, BitmapAbout: TBitmap24;
		procedure InitNRT;
	public
		{ public declarations }
		ProgramName: string;
		ProgramVersion: string;
		procedure LoadFile(AboutFile: TFileName);
	end;

procedure ExecuteAbout(AOwner: TComponent; Version, Build: string;
	FileName: TFileName; const Modal: Boolean);
procedure AboutRW(const Save: Boolean);

var
	fAbout: TfAbout;

	RunCount: UG;
	RunTime: U64;
	RunProgramTime: U32;

implementation

{$R *.DFM}
uses
	ShellAPI,
	uGraph, uRot24, uDIni, uTexture, uScreen, uSysInfo;
var
	LAboutClock, LMemClock: LongWord;

procedure ExecuteAbout(AOwner: TComponent; Version, Build: string;
	FileName: TFileName; const Modal: Boolean);
var OrigCursor: TCursor;
begin
	if not Assigned(fAbout) then
	begin
		OrigCursor := Screen.Cursor;
		Screen.Cursor := crHourGlass;
		fAbout := TfAbout.Create(AOwner);
		fAbout.ProgramName := Application.Title;
		fAbout.ProgramVersion := 'Version ' + Version;
		fAbout.PanelBuild.Caption := Build;
		fAbout.LoadFile(FileName);
		fAbout.BitmapName := Conv24(fAbout.ImageName.Picture.Bitmap);
		fAbout.BitmapVersion := Conv24(fAbout.ImageVersion.Picture.Bitmap);
		fAbout.BitmapAbout := Conv24(fAbout.ImageAbout.Picture.Bitmap);
		Screen.Cursor := OrigCursor;
	end;
	CorrectPos(fAbout);
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
begin
	if Save then
	begin
		Inc(RunCount);
		RunProgramTime := GetTickCount - RunProgramTime;
		Inc(RunTime, RunProgramTime);
	end;
	MainIni.RWUG('Statistics', 'RunCount', RunCount, Save);
	MainIni.RWU64('Statistics', 'RunTime', RunTime, Save);
end;

var
	Clock: LongWord;
	Effect: Byte;
	Typ: Byte;
	BmpAbout: TBitmap;
	BmpAbout24: TBitmap24;
	AboutImage24: TBitmap24;

procedure TfAbout.InitNRT;
begin
	PanelNRT.Caption := msToStr(GetTickCount - RunProgramTime + 500, diMSD, 0);
end;

procedure TfAbout.LoadFile(AboutFile: TFileName);

	procedure GenBmp;
	const
		GenFunc: array[0..4] of TGenFunc = (gfSpecHorz, gfTriaHorz, gfLineHorz, gfFade2x, gfFade2xx);
	var
		AC: array[0..3] of TColor;
		B: TBitmap24;
	begin
		BmpAbout.Width := 64;
		BmpAbout.Height := 64;
		BmpAbout.PixelFormat := pf24bit;
		AC[0] := clBtnFace; AC[1] := clBlack; AC[2] := clBtnFace; AC[3] := clWhite;
		B := Conv24(BmpAbout);
		GenerateERGB(B, clNone,
			GenFunc[RunCount mod (High(GenFunc) + 1)], AC, ScreenCorectColor, ef16, nil);
		B.Free;                      
	end;
var Quality: Integer;
begin
	if not Assigned(BmpAbout) then
	begin
		BmpAbout := TBitmap.Create;
	end;
	if AboutFile = '' then
		GenBmp
	else
	begin
		BitmapLoadFromFile(BmpAbout, AboutFile, 64, 64, Quality);
		if (BmpAbout.Width = 0) or (BmpAbout.Height = 0) then GenBmp;
	end;
	BmpAbout24 := Conv24(BmpAbout);
	BmpAbout.TransparentColor := GetTransparentColor(BmpAbout);
end;

procedure TfAbout.FormCreate(Sender: TObject);
begin
	EditEmail.Text := 'safrad@email.cz?subject=' + Application.Title;
	PanelRC.Caption := Using('~#,###,###,##0', RunCount);
	PanelTRT.Caption := msToStr(RunTime, diMSD, 3);

	InitImage(ImageAbout, clBtnFace);
	InitImage(ImageName, clBlack);
	ImageName.Picture.Bitmap.Canvas.Brush.Style := bsClear;
	ImageName.Picture.Bitmap.Canvas.Font.Style := [fsBold];
	ImageName.Picture.Bitmap.Canvas.Font.Size := 12;
	ImageName.Picture.Bitmap.Canvas.Font.Name := 'Times New Roman';
	ImageName.Picture.Bitmap.Canvas.Font.Color := clBlack;
	InitImage(ImageVersion, clBtnFace);
	ImageVersion.Picture.Bitmap.Canvas.Brush.Style := bsClear;
	ImageVersion.Picture.Bitmap.Canvas.Font.Style := [fsBold];

	FormImage(ImageBackground);

	AboutImage24 := Conv24(ImageAbout.Picture.Bitmap);
	InitNRT;
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
	LAboutClock := GetTickCount;
	LMemClock := LAboutClock;
	Timer1.Enabled := True;
end;

procedure TfAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Timer1.Enabled := False;
end;

procedure TfAbout.BitBtnOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfAbout.ImageAboutMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
		if Typ < MaxTyp then Inc(Typ) else Typ := 0;
	if Button = mbRight then
		if Typ > 0 then Dec(Typ) else Typ := MaxTyp;
end;

procedure TfAbout.EditWebClick(Sender: TObject);
begin
	ShellExecute(0, 'open', PChar(EditWeb.Text), nil, nil, SW_ShowNormal);
end;

procedure TfAbout.EditEmailClick(Sender: TObject);
begin
	ShellExecute(0, 'open', PChar('mailto: ' + EditEMail.Text), nil, nil, SW_ShowNormal);
end;

procedure TfAbout.Timer1Timer(Sender: TObject);
var
	HClock: Byte;
	NTime, ETime: LongWord;
begin
{	Timer1.Enabled := False;
	while Visible do
	begin}
		NTime := GetTickCount;
		ETime := NTime - LAboutClock;
		LAboutClock := NTime;
		if ETime = 0 then Exit;
		Inc(Clock, ETime);

		if NTime - LMemClock >= 1000 then
		begin
			InitNRT;
			if Assigned(fSysInfo) then
				if fSysInfo.Visible then
				begin
					FillSysInfoD(SysInfo);
					fSysInfo.FillComp;
				end;
			while NTime - LMemClock >= 1000 do
			begin
				Inc(LMemClock, 1000);
			end;
		end;

		if Assigned(ImageName.Picture.Bitmap) then
		begin
			GenRGB(BitmapName, clNone, gfSpecHorz, Clock div 64, ef16);
			BarE24(BitmapName, clNone, clBtnFace, ef12);
			ImageName.Picture.Bitmap.Canvas.TextOut(
				(ImageName.Picture.Bitmap.Width -
				ImageName.Picture.Bitmap.Canvas.TextWidth(ProgramName)) div 2,
				(ImageName.Picture.Bitmap.Height -
				ImageName.Picture.Bitmap.Canvas.TextHeight(ProgramName)) div 2,
				ProgramName);
			ImageName.Repaint;
		end;

		if Assigned(ImageVersion.Picture.Bitmap) then
		begin
			BarE24(BitmapVersion, clNone, clBtnFace, ef16);
			ImageVersion.Picture.Bitmap.Canvas.Font.Color := clBlack;
			ImageVersion.Picture.Bitmap.Canvas.TextOut(
				(ImageVersion.Picture.Bitmap.Width -
				ImageVersion.Picture.Bitmap.Canvas.TextWidth(ProgramVersion)) div 2,
				(ImageVersion.Picture.Bitmap.Height -
				ImageVersion.Picture.Bitmap.Canvas.TextHeight(ProgramVersion)) div 2,
				ProgramVersion);
			GenRGB(BitmapVersion, clBtnFace, gfSpecHorz, Clock div 64, ef16);
			ImageVersion.Repaint;
		end;
		if Assigned(BmpAbout) then
		begin
			BarE24(BitmapAbout, clNone, clBtnFace, ef02);
			HClock := (Clock div 64) and $7f;
			if HClock <= 32  then
				Effect := HClock shr 1
			else if HClock <= 92 then
				Effect := 16
			else if HClock <= 92 + 32 then
				Effect := (92 + 32) div 2 - HClock shr 1
			else
				Effect := 0;

			RotateDefE24(AboutImage24, BmpAbout24, Typ, Clock div 16, BmpAbout.TransparentColor,
				TEffect(Effect));
			if HClock = 127 then
			begin
				if Typ = MaxTyp then Typ := 0 else Inc(Typ);
			end;
			ImageAbout.Repaint;
		end;
{		Application.ProcessMessages;
		Sleep(40);

	end;}
end;

procedure TfAbout.EditIcqClick(Sender: TObject);
begin
	ShellExecute(0, 'open', {PChar('mailto: ' + EditEMail.Text)}PChar('icq.exe'), nil, nil, SW_ShowNormal);
end;

procedure TfAbout.SysInfo1Click(Sender: TObject);
begin
	if not Assigned(fSysInfo) then fSysInfo := TfSysInfo.Create(Self);
	FillSysInfoS(SysInfo);
	FillSysInfoD(SysInfo);
	fSysInfo.FillComp;
	fSysInfo.ShowModal;
end;

initialization
	RunProgramTime := GetTickCount;
end.
