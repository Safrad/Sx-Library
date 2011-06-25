//* File:     Lib\GUI\uAbout.pas
//* Created:  1999-10-01
//* Modified: 2008-01-19
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uAbout;

interface

uses
	uDForm, uTypes, uDBitmap,
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
	ExtCtrls, uDButton, uDLabel, uDTimer, uDImage, uDEdit, uDView,
	uDWinControl;

type     
	TfAbout = class(TDForm)
		Timer1: TDTimer;
		ButtonOk: TDButton;
		ImageAbout: TDImage;
		ButtonSysInfo1: TDButton;
		ButtonMemoryStatus: TDButton;
		DViewAbout: TDView;
		ButtonStat: TDButton;
		Bevel1: TBevel;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure ButtonOkClick(Sender: TObject);
		procedure ImageAboutMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure EditWebClick(Sender: TObject);
		procedure EditEMailClick(Sender: TObject);
		procedure DTimer1Timer(Sender: TObject);
		procedure ButtonSysInfo1Click(Sender: TObject);
		procedure ImageAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure ImageAboutFill(Sender: TObject);
		procedure ButtonMemoryStatusClick(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure DViewAboutGetData(Sender: TObject; var Data: String;
			ColIndex, RowIndex: Integer; Rect: TRect);
		procedure ButtonStatClick(Sender: TObject);
	private
		Effect: U1;
		Typ: U1;
		Reset: Boolean;
		BmpAbout: TDBitmap;
		procedure UpdateView;
		procedure RWOptions(const Save: BG);
		procedure NewFlash;
	public
		procedure LoadFile(AboutFile: TFileName);
	end;

procedure OpenLocalHomepage;
procedure OpenWebHomepage;
procedure ExecuteAbout(AOwner: TComponent; const Modal: Boolean);
procedure AboutRW(const Save: Boolean);
var
	fAbout: TfAbout;

implementation

{$R *.DFM}
uses
	uAPI, uSimulation, uHTML, uStart,
	uProjectInfo,
	uGraph, uDIniFile, uScreen, uSysInfo, uFiles, uFile, uMsg, uData, uWave, uColor,
	{$ifndef LINUX}uMemStatus,{$endif} uStrings, uMath, uSystem, uInputFormat, uOutputFormat, uLog;
var
	LastNowTime: U8;

procedure OpenLocalHomepage;
begin
	APIOpen(WorkDir + 'ReadMe' + PathDelim + IndexFile);
end;

procedure OpenWebHomepage;
begin
	APIOpen(GetProjectInfo(piWeb));
end;

procedure ExecuteAboutEx(AOwner: TComponent; const FileName: TFileName; const Modal: Boolean);
begin
	PlayWinSound(wsExclamation);
	if not Assigned(fAbout) then
	begin
		fAbout := TfAbout.Create(AOwner);
{		fAbout.EditAuthor.Text := GetProjectInfo(piAuthor);
		fAbout.EditVersion.Text := GetProjectInfo(piProductVersion);
		fAbout.EditRelease.Text := DateToS(SToDate(GetProjectInfo(piRelease), ifIO), ofDisplay);
		fAbout.EditCopyright.Text := GetProjectInfo(piLegalCopyright);}
	end;
	fAbout.LoadFile(FileName);
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

procedure ExecuteAbout(AOwner: TComponent; const Modal: Boolean); overload;
const
	Ext: array[0..3] of string = ('png', 'gif', 'jpg', 'jpeg');
var
	i, j: SG;
	FileName: TFileName;
begin
	for j := 0 to 1 do
		for i := 0 to Length(Ext) - 1 do
		begin
			FileName := GraphDir;
			if j <> 0 then
				FileName := FileName + 'Logo'
			else
				FileName := FileName + GetProjectInfo(piInternalName);
			FileName := FileName + '.' + Ext[i];
			if FileExists(FileName) then
			begin
				ExecuteAboutEx(AOwner, FileName, Modal);
				Exit;
			end;
		end;
	ExecuteAboutEx(AOwner, '', Modal);
end;

procedure AboutRW(const Save: Boolean);
begin
	RWStart(MainIni, Save);
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
		BmpAbout.GenerateRGB(GenFunc[GetRunCount mod (High(GenFunc) + 1)], AC, ScreenCorrectColor, ef16, nil);
		BmpAbout.Transparent := False;
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
		if (BmpAbout.Width < ImageAbout.Width div 2) or (BmpAbout.Height < ImageAbout.Height div 2) then
		begin
			BmpAbout.Resize(BmpAbout.Width * 2, BmpAbout.Height * 2);
		end
		else if (BmpAbout.Width > ImageAbout.Width) or (BmpAbout.Height > ImageAbout.Height) then
		begin
			BmpAbout.Resize(BmpAbout.Width div 2, BmpAbout.Height div 2);
		end;
		if BmpAbout.Empty then GenBmp;
	end;
end;

procedure TfAbout.FormCreate(Sender: TObject);
begin
	{$ifdef LINUX}
	ButtonMemoryStatus.Visible := False;
	{$endif}

	Background := baGradient;

{	EditEMail.Text := GetProjectInfo(piEmail) + '?subject=' + GetProjectInfo(piProductName);
	EditWeb.Text := GetProjectInfo(piWeb);
	EditCompany.Text := GetProjectInfo(piCompanyName);}

{	ImageAbout.ShowHint := True;
	ImageAbout.Hint := GetProjectInfo(piFileDescription);}

{	PanelRunCount.Text := NToS(GetRunCount);
	PanelTotalRunTime.Text := }

{	ImageVersion.Bitmap.Canvas.Brush.Style := bsClear;
	ImageVersion.Bitmap.Canvas.Font.Style := [fsBold];}

	UpdateView;

	RWOptions(False);
end;

procedure TfAbout.FormDestroy(Sender: TObject);
begin
	if Assigned(BmpAbout) then
	begin
		FreeAndNil(BmpAbout);
	end;
end;

procedure TfAbout.FormShow(Sender: TObject);
begin
	LastNowTime := PerformanceCounter;
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

type
	PFlash = ^TFlash;
	TFlash = packed record // 16
		X, Y: S4;
		Power: S4;
		Color: TRGBA;
	end;
var
	Flashs: TData;
const
	MaxTyp = 13;

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
begin
	APIOpen(GetProjectInfo(piWeb));
end;

procedure TfAbout.EditEMailClick(Sender: TObject);
begin
	APIOpen('mailto: ' + GetProjectInfo(piEMail));
end;

procedure TfAbout.DTimer1Timer(Sender: TObject);
begin
	if NowTime >= LastNowTime + PerformanceFrequency{1 second interval} then
	begin
//		UpdateNowRunTime;
		FillDynamicInfo(GSysInfo);
		UpdateSysInfo(@GSysInfo);
{		if Assigned(fSysInfo) then
			if fSysInfo.Visible then
			begin
				fSysInfo.FillComp;
			end;}
{		while NowTime - LMemClock >= PerformanceFrequency do
		begin
			Inc(LMemClock, PerformanceFrequency);
		end;}
		LastNowTime := NowTime;
	end;
	NewFlash; // TODO Crash in DGames!
	ImageAbout.Invalidate;
//	ImageName.Invalidate;
end;

procedure TfAbout.ButtonSysInfo1Click(Sender: TObject);
begin
	FillDynamicInfo(GSysInfo);
	DisplaySysInfo(@GSysInfo);
{	if not Assigned(fSysInfo) then fSysInfo := TfSysInfo.Create(Self);
//	fSysInfo.FormStyle := fAbout.FormStyle;
	fSysInfo.SetBounds(fAbout.Left, fAbout.Top, fSysInfo.Width, fSysInfo.Height);
	fSysInfo.FillComp;
	fSysInfo.Show;}
end;

procedure TfAbout.NewFlash;
var
	Flash: PFlash;
begin
	Flash := Flashs.Add;
	Flash.X := ImageAbout.LMouseX;
	Flash.Y := ImageAbout.LMouseY;
	Flash.Power := 128 + 32 + Random(128 + 15 - 32);
	Flash.Color.L := FireColor(256 + Random(256)); // SpectrumColor(Random(MaxSpectrum));
//		Exchange(Flash.Color.R, Flash.Color.B);
end;

procedure TfAbout.ImageAboutMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
begin
//	if Random(10) = 0 then
//	begin

//	end;
end;

procedure TfAbout.ImageAboutFill(Sender: TObject);
var
	BitmapAbout: TDBitmap;
	HClock: U1;
	i: SG;
	Flash: ^TFlash;
	Co: array[0..3] of TColor;
begin
	BitmapAbout := ImageAbout.Bitmap;
//	BitmapAbout.Bar(clBtnFace, ef02);

//	BitmapName.GenRGB(clNone, gfSpecHorz, (16 * Timer1.Clock div PerformanceFrequency), ef16);
	BitmapAbout.GenerateRGBEx(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, TGenFunc(Typ), Co, 0, ef03,
		(16 * Timer1.Clock div PerformanceFrequency), nil);

	HClock := (32 * Timer1.Clock div PerformanceFrequency) and $7f;
	if HClock <= 32 then
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

	if (Effect > 0) and (BmpAbout <> nil) then
	begin
		RotateDef(BitmapAbout, BmpAbout, Typ, (U8(AngleCount) * U8(Timer1.Clock) div (8 * PerformanceFrequency)) and (AngleCount - 1), TEffect(Effect));
	end;
	
	i := 0;
	while i < Flashs.Count do
	begin
		Flash := Flashs.Get(i);
		Dec(Flash.Power, 10);
		Inc(Flash.Y, 2);
		Inc(Flash.X, Random2(2));
		if (Flash.Y > SG(BitmapAbout.Height)) or (Flash.Power <= 0) then
		begin
			Flashs.Delete(i);
		end
		else
		begin
{					C.R := RoundDiv(Flashs[i].Color.R * Flashs[i].Power, 256);
			C.G := RoundDiv(Flashs[i].Color.R * Flashs[i].Power, 256);
			C.B := RoundDiv(Flashs[i].Color.R * Flashs[i].Power, 256);
			C.T := 0;}
			//Pix(BitmapAbout.Data, BitmapAbout.ByteX, Flash.X, Flash.Y, @Flash.Color, TEffect(Flash.Power div 16));

			BitmapAbout.BarBorder(Flash.X - 1, Flash.Y - 1, Flash.X + 2, Flash.Y + 2, Flash.Color.L, TEffect(Flash.Power div 16));
			Inc(i);
		end;
	end;
	BitmapAbout.Border(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, clBlack, clWhite, 3, ef08);


	BitmapAbout.Canvas.Brush.Style := bsClear;
	BitmapAbout.Canvas.Font.Style := [fsBold];
	BitmapAbout.Canvas.Font.Size := -24;
	BitmapAbout.Canvas.Font.Name := 'Times New Roman';
	GoodText(BitmapAbout.Canvas, Rect(16, 16, BitmapAbout.Width - 16, BitmapAbout.Height - 16), GetProjectInfo(piProductName),
		clBlack, clWhite, clSilver, taCenter, tlCenter);
{	DrawCutedText(BitmapAbout.Canvas, Rect(2, 2, BitmapAbout.Width - 2, BitmapAbout.Height - 2), taCenter, tlCenter,
		GetProjectInfo(piProductName), True, 1);}
	BitmapAbout.Border(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, clBlack, clWhite, 2, ef08);

end;

procedure TfAbout.ButtonMemoryStatusClick(Sender: TObject);
begin
{$ifndef LINUX}
	if not Assigned(fMemStatus) then
		fMemStatus := TfMemStatus.Create(Self);
//	fMemStatus.FormStyle := fAbout.FormStyle;
//	fMemStatus.SetBounds(fAbout.Left, fAbout.Top, fMemStatus.Width, fMemStatus.Height);
	fMemStatus.Show;
{$endif}
end;

procedure TfAbout.FormHide(Sender: TObject);
begin
	RWOptions(True);
end;

procedure TfAbout.DViewAboutGetData(Sender: TObject; var Data: String;
	ColIndex, RowIndex: Integer; Rect: TRect);
begin
	if ButtonStat.Down = False then
	begin
		case ColIndex of
		0: Data := AddSpace(ProjectInfoStr[TProjectInfoName(RowIndex)]);
		1: Data := GetProjectInfo(TProjectInfoName(RowIndex));
		end;
	end
	else
	begin
		case ColIndex of
		0:
		begin
			case RowIndex of
			0: Data := 'Run Count';
			1: Data := 'Now Run Time';
			2: Data := 'Total Run Time';
			3: Data := 'I/O Read Count';
			4: Data := 'I/O Read Bytes';
			5: Data := 'I/O Write Count';
			6: Data := 'I/O Write Bytes';
			end;
		end;
		1:
		begin
			case RowIndex of
			0: Data := NToS(GetRunCount);
			1: Data := MsToStr(TimeDifference(GetTickCount, GetStartProgramTime) + Second div 2, diDHMSD, 0, False);
			2: Data := MsToStr(GetRunTime, diDHMSD, 3, False);
//			GetStartProgramTime
			3: Data := NToS(ReadCount);
			4: Data := BToStr(ReadBytes);
			5: Data := NToS(WriteCount);
			6: Data := BToStr(WriteBytes);
			end;
		end;
		end;
	end;
end;

procedure TfAbout.ButtonStatClick(Sender: TObject);
begin
	UpdateView;
end;

procedure TfAbout.UpdateView;
begin
	if ButtonStat.Down = False then
	begin
		DViewAbout.RowCount := Length(ProjectInfoStr);
		DViewAbout.ColumnCount := 0;
		DViewAbout.AddColumn('Item Name', 1 * DViewAbout.Width div 3, taLeftJustify, True);
		DViewAbout.AddColumn('Value', 2 * DViewAbout.Width div 3 - 24, taLeftJustify, True);
	end
	else
	begin
		DViewAbout.RowCount := 7;
		DViewAbout.ColumnCount := 0;
		DViewAbout.AddColumn('Item Name', 1 * DViewAbout.Width div 3, taLeftJustify, True);
		DViewAbout.AddColumn('Value', 2 * DViewAbout.Width div 3 - 24, taLeftJustify, True);
	end;
	DViewAbout.DataChanged;
end;

procedure TfAbout.RWOptions(const Save: BG);
begin
	if Assigned(MainIni) then
	begin
		MainIni.RWFormPos(Self, Save);
		DViewAbout.Serialize(MainIni, Save);
	end;
end;

{$ifopt d+}
{var
	MemCount, MemSize: SG;}
{$endif}
initialization
{$ifopt d+}
{	MemCount := AllocMemCount;
	MemSize := AllocMemSize;}
{$endif}
	Flashs := TData.Create(True);
	Flashs.ItemSize := SizeOf(TFlash);
finalization
	FreeAndNil(Flashs);
end.
