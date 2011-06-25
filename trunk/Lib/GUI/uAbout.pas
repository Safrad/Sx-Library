// * File:     Lib\GUI\uAbout.pas
// * Created:  1999-10-01
// * Modified: 2009-12-05
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

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
		ButtonSysInfo: TDButton;
		ButtonMemoryStatus: TDButton;
		DViewAbout: TDView;
		ButtonStatistics: TDButton;
		ButtonVersionInfo: TDButton;
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
		procedure ButtonSysInfoClick(Sender: TObject);
		procedure ImageAboutFill(Sender: TObject);
		procedure ButtonMemoryStatusClick(Sender: TObject);
		procedure DViewAboutGetData(Sender: TObject; var Data: String;
			ColIndex, RowIndex: Integer; Rect: TRect);
		procedure ButtonXClick(Sender: TObject);
		procedure FormResize(Sender: TObject);
	private
		Effect: U1;
		Typ: U1;
		Reset: Boolean;
		BmpAbout: TDBitmap;
		ViewInfo: SG;
		UsedValues: array of SG;
		UsedValueCount: SG;
		procedure UpdateView;
		procedure RWOptions(const Save: BG);
		procedure NewFlash;
	public
		procedure LoadFile(AboutFile: TFileName);
	end;

function GetLocalHomepage: TFileName;
procedure OpenLocalHomepage;
procedure OpenWebHomepage;
procedure ExecuteAbout(AOwner: TComponent; const Modal: Boolean);
var
	fAbout: TfAbout;

implementation

{$R *.DFM}
uses
	Messages,
	uAPI, uSimulation, uHTML, uStart, uDictionary,
	uProjectInfo,
	uGraph, uDIniFile, uScreen, uSysInfo, uFiles, uFile, uMsg, uData, uWave, uColor, uDrawStyle,
	{$ifndef LINUX}uMemStatus,{$endif} uStrings, uMath, uSystem, uInputFormat, uOutputFormat, uLog;
var
	LastNowTime: U8;

function GetLocalHomepage: TFileName;
begin
	Result := WorkDir + 'ReadMe' + PathDelim + IndexFile
end;

procedure OpenLocalHomepage;
begin
	APIOpen(GetLocalHomepage);
end;

procedure OpenWebHomepage;
begin
	APIOpen(GetProjectInfo(piWeb));
end;

procedure ExecuteAboutEx(AOwner: TComponent; const FileName: TFileName; const Modal: Boolean);
begin
//	PlayWinSound(wsExclamation);
	if not Assigned(fAbout) then
	begin
		fAbout := TfAbout.Create(AOwner);
	end;
	fAbout.LoadFile(FileName);
	if Modal then
	begin
		fAbout.FormStyle := fsNormal;
		fAbout.ShowModal;
		if FreeFormAfterClose then
			FreeAndNil(fAbout);
	end
	else
	begin
		fAbout.FormStyle := fsStayOnTop;
		fAbout.Show;
	end;
end;


function GetFileName(const Base: string): TFileName; overload;
var
	i: SG;
	FileName: TFileName;
begin
	Result := '';
	for i := 0 to Length(PrefferedExt) - 1 do
	begin
		FileName := GraphDir + Base + '.' + PrefferedExt[i];
		if FileExists(FileName) then
		begin
			Result := FileName;
			Exit;
		end;
	end;
end;

function GetFileName: TFileName; overload;
begin
	Result := GetFileName('Logo');
	if Result <> '' then Exit;
	Result := GetFileName(GetProjectInfo(piInternalName));
end;

procedure ExecuteAbout(AOwner: TComponent; const Modal: Boolean); overload;
begin
	ExecuteAboutEx(AOwner, GetFileName, Modal);
end;

procedure TfAbout.LoadFile(AboutFile: TFileName);

	procedure GenBmp;
	const
		GenFunc: array[0..4] of TGenFunc = (gfSpecHorz, gfTriaHorz, gfLineHorz, gfFade2x, gfFade2xx);
	var
		AC: array[0..3] of TColor;
	begin
		BmpAbout.SetSize(64, 64, clNone);
		AC[0] := clBtnFace; AC[1] := clBlack; AC[2] := clBtnFace; AC[3] := clWhite;
		BmpAbout.GenerateRGB(GenFunc[GetRunCount mod (High(GenFunc) + 1)], AC, ef16, nil);
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
var
	Id: SG;
begin
	Dictionary.TranslateForm(Self);
	{$ifdef LINUX}
	ButtonMemoryStatus.Visible := False;
	{$endif}
	{$ifopt d-}
	ButtonMemoryStatus.Visible := False;
	{$endif}

	Background := baGradient;

	UsedValueCount := 0;
	SetLength(UsedValues, 0);
	for Id := 0 to Length(ProjectInfoStr) - 1 do
	begin
		if GetProjectInfo(TProjectInfoName(Id)) <> '' then
		begin
			SetLength(UsedValues, UsedValueCount + 1);
			UsedValues[UsedValueCount] := Id;
			Inc(UsedValueCount);
		end;
	end;
	MainIni.RegisterRW(RWOptions);
end;

procedure TfAbout.FormDestroy(Sender: TObject);
begin
	MainIni.UnregisterRW(RWOptions);
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

procedure TfAbout.ImageAboutMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const kSC_DragMove = $F012 ;
begin
	ReleaseCapture();
	ImageAbout.Perform(WM_SYSCOMMAND, kSC_DragMove, 0);
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
		if FormDraw(fSysInfo) then
		begin
			FillDynamicInfo(GSysInfo);
			UpdateSysInfo(@GSysInfo);
		end;
		LastNowTime := NowTime;
	end;
	NewFlash;
	ImageAbout.Invalidate;
end;

procedure TfAbout.ButtonSysInfoClick(Sender: TObject);
begin
	FillDynamicInfo(GSysInfo);
	DisplaySysInfo(@GSysInfo, Self);
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
	BitmapAbout.GenerateRGBEx(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, TGenFunc(Typ), Co, ef03,
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
			BitmapAbout.BarBorder(Flash.X - 1, Flash.Y - 1, Flash.X + 2, Flash.Y + 2, Flash.Color.L, TEffect(Flash.Power div 16));
			Inc(i);
		end;
	end;
	if Timer1.TimerCount < 200 then
	begin
		BitmapAbout.Canvas.Brush.Style := bsClear;
		BitmapAbout.Canvas.Font.Style := [fsBold];
		BitmapAbout.Canvas.Font.Size := -24;
		BitmapAbout.Canvas.Font.Name := 'Times New Roman';
		GoodText(BitmapAbout.Canvas, Rect(16, 16, BitmapAbout.Width - 16, BitmapAbout.Height - 16), GetProjectInfo(piProductName),
			clBlack, clWhite, clSilver, taCenter, tlCenter);
	end;
	BitmapAbout.Border(2, 2, BitmapAbout.Width - 3, BitmapAbout.Height - 3, clBlack, clWhite, 1, ef04);
	BitmapAbout.Border(1, 1, BitmapAbout.Width - 2, BitmapAbout.Height - 2, clBlack, clWhite, 1, ef08);
	BitmapAbout.Border(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, clBlack, clWhite, 1, ef12);
	if ImageAbout.IsFocused then
		BitmapAbout.Border(clHighlight, clHighlight, 3, ef08);//Border(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, clBlack, clWhite, 5, ef16);
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

procedure TfAbout.DViewAboutGetData(Sender: TObject; var Data: String;
	ColIndex, RowIndex: Integer; Rect: TRect);
begin
	if ViewInfo = 0 then
	begin
		case ColIndex of
		0: Data := Translate(AddSpace(ProjectInfoStr[TProjectInfoName(UsedValues[RowIndex])]));
		1: Data := GetProjectInfo(TProjectInfoName(UsedValues[RowIndex]));
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
			Data := Translate(Data);
		end;
		1:
		begin
			case RowIndex of
			0: Data := NToS(GetRunCount);
			1: Data := MsToStr(TimeDifference(GetTickCount, GetStartProgramTime) + Second div 2, diDHMSD, 0, False);
			2: Data := MsToStr(GetRunTime, diDHMSD, 3, False);
			3: Data := NToS(ReadCount);
			4: Data := BToStr(ReadBytes);
			5: Data := NToS(WriteCount);
			6: Data := BToStr(WriteBytes);
			end;
		end;
		end;
	end;
end;

procedure TfAbout.ButtonXClick(Sender: TObject);
begin
	ViewInfo := TDButton(Sender).Tag;
	UpdateView;
end;

procedure TfAbout.UpdateView;
begin
	ButtonVersionInfo.Down := ViewInfo = 0;
	ButtonStatistics.Down := ViewInfo = 1;

	DViewAbout.ScrollTo(0, 0);
	DViewAbout.DeselectAll;
	if ViewInfo = 0 then
	begin
		DViewAbout.RowCount := UsedValueCount; // Length(ProjectInfoStr);
		DViewAbout.ColumnCount := 0;
	end
	else
	begin
		DViewAbout.RowCount := 7;
		DViewAbout.ColumnCount := 0;
	end;
	DViewAbout.AddColumn(Translate('Item Name'), 1 * DViewAbout.Width div 3, taLeftJustify, True);
	DViewAbout.AddColumn(Translate('Value'), 2 * DViewAbout.Width div 3 - 24, taLeftJustify, True);
	DViewAbout.DataChanged;
end;

procedure TfAbout.RWOptions(const Save: BG);
begin
	if Assigned(MainIni) then
	begin
		MainIni.RWFormPos(Self, Save);
		DViewAbout.Serialize(MainIni, Save);
	end;
	if Save = False then
		UpdateView;
end;

procedure TfAbout.FormResize(Sender: TObject);
begin
	// Height
	ButtonOk.Top := ClientHeight - FormBorder - ButtonOk.Height;
	ButtonSysInfo.Top := ClientHeight - FormBorder - ButtonSysInfo.Height;
	ButtonMemoryStatus.Top := ClientHeight - FormBorder - ButtonMemoryStatus.Height;


	DViewAbout.Height := DViewAbout.IdealHeight;
	DViewAbout.Top := ButtonOk.Top - DViewAbout.Height - FormBorder;
	ButtonVersionInfo.Top := DViewAbout.Top - ButtonVersionInfo.Height;
	ButtonStatistics.Top := DViewAbout.Top - ButtonStatistics.Height;

	ImageAbout.Top := FormBorder;
	ImageAbout.Height := DViewAbout.Top - 2 * FormBorder - ButtonVersionInfo.Height;

	// Width
	ImageAbout.Left := FormBorder;
	ImageAbout.Width := ClientWidth - 2 * ImageAbout.Left;

	DViewAbout.Left := FormBorder;
	DViewAbout.Width := ClientWidth - 2 * DViewAbout.Left;

	ButtonOk.Left := ClientWidth - FormBorder - ButtonOk.Width;
	UpdateView;
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
//	FreeAndNil(fAbout);
end.
