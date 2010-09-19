//* File:     Lib\uAbout.pas
//* Created:  1999-10-01
//* Modified: 2006-01-25
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uAbout;

interface

uses
	uDForm, uTypes, uDBitmap,
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
	ExtCtrls, uDButton, uDLabel, uDTimer, uDImage;

type
	TfAbout = class(TDForm)
		Timer1: TDTimer;
		ButtonOk: TDButton;
    BevelSep: TBevel;
		Image1: TImage;
		Image2: TImage;
    LabelRunCount: TLabel;
    LabelNowRunTime: TLabel;
    LabelTotalRunTime: TLabel;
    EditCreationDate: TEdit;
    PanelRC: TEdit;
    PanelTRT: TEdit;
    PanelNRT: TEdit;
		ImageName: TDImage;
    LabelAuthor: TLabel;
    LabelCreated: TLabel;
    LabelEMail: TLabel;
		EditAuthor: TEdit;
		EditWeb: TEdit;
    LabelWeb: TLabel;
    EditEMail: TEdit;
    Bevel: TBevel;
		ImageAbout: TDImage;
		Image3: TImage;
    LabelIcq: TLabel;
		EditIcq: TEdit;
		Image4: TImage;
		SysInfo1: TDButton;
		DButtonMemoryStatus: TDButton;
    LabelCount: TLabel;
    EditReleaseDate: TEdit;
    LabelModified: TLabel;
    LabelVersion: TLabel;
    EditVersion: TEdit;
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
		procedure EditIcqClick(Sender: TObject);
		procedure SysInfo1Click(Sender: TObject);
		procedure ImageAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure ImageNameFill(Sender: TObject);
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
		procedure LoadFile(AboutFile: TFileName);
	end;

const
	paExit = -1;
	paFile = -2;
var
	ParamFile: TFileName;
	ParamIndex: SG = 0;

procedure AcceptParams(AcceptFile: BG; Params: array of string; DesParams: array of string);
function CompareParams: SG;
procedure CloseParams;
procedure HelpParams;
procedure ReadMe;
procedure Homepage;
procedure Help; overload;
procedure Help(HRef: string); overload;
procedure ExtOpenFile(FileName: TFileName); overload;
procedure ExtOpenFile(FileName: TFileName; Parameters: string); overload;
procedure ExecuteAbout(AOwner: TComponent; const Modal: Boolean);
{procedure ExecuteAbout(AOwner: TComponent; Version, Created, Modified: string;
	FileName: TFileName; const Modal: Boolean);}
procedure AboutRW(const Save: Boolean);

var
	fAbout: TfAbout;

	RunCount: UG;
	RunTime: U8;
	StartProgramTime: U4;

implementation

{$R *.DFM}
uses
	ShellAPI, Dialogs,
	rpVersionInfo,
	uGraph, uDIni, uScreen, uSysInfo, uFiles, uError, uData, uWave, {$ifndef LINUX}uMemStatus,{$endif} uStrings, uMath, uSystem, uFormat;
var
	LMemClock: U8;
	RunProgramTime: U8;

type
	PFlash = ^TFlash;
	TFlash = packed record // 16
		X, Y: S4;
		Power: S4;
		Color: TRColor;
	end;
var
	Flashs: TData;
const
	MaxTyp = 13;

var
	AcceptFile: BG;
	Params: array of string;
	DesParams: array of string;
	IllegalParam: BG = False;

procedure AcceptParams(AcceptFile: BG; Params: array of string; DesParams: array of string);
var i: SG;
begin
	uAbout.AcceptFile := AcceptFile;
	SetLength(uAbout.Params, Length(Params));
	for i := 0 to Length(uAbout.Params) - 1 do
		uAbout.Params[i] := DelCharsF(Params[i], ' ');

	SetLength(uAbout.DesParams, Length(DesParams));
	for i := 0 to Length(uAbout.DesParams) - 1 do
		uAbout.DesParams[i] := DesParams[i];
end;

function CompareParams: SG;
label LAgain;
var
	i: SG;
	Par: string;
	AF: BG;
begin
	LAgain:
	if ParamIndex >= ParamCount then
		Result := paExit
	else
	begin
		Par := ParamStr(ParamIndex + 1);
		AF := AcceptFile;
		if Par[1] = '-' then
		begin
			Delete(Par, 1, 1);
			AF := False;
		end
		else if Par[1] = '/' then
		begin
			Delete(Par, 1, 1);
			AF := False;
		end;
		Result := paFile;
		for i := 0 to Length(Params) - 1 do
		begin
			if StartStr(UpperCase(Par), UpperCase(Params[i])) then
			begin
				Result := i;
				Break;
			end;
		end;
		if Result = paFile then
		begin
			if StartStr(UpperCase(Par), 'HELP') then
			begin
				HelpParams;
				Inc(ParamIndex);
				goto LAgain;
			end;
			if AF then
			begin
				ParamFile := FullDir(Par);
				if (not FileExists(ParamFile)) and (not DirectoryExists(ParamFile)) then
				begin
					MessageD('Illegal command line parameter' + LineSep +
						Par + LineSep +
						'Command line file not found' + LineSep + ParamFile, mtWarning, [mbOK]);
					IllegalParam := True;
					Inc(ParamIndex);
					goto LAgain;
				end;
			end
			else
			begin
				MessageD('Illegal command line parameter' + LineSep + Par, mtWarning, [mbOK]);
				IllegalParam := True;
				Inc(ParamIndex);
				goto LAgain;
			end;

		end;
	end;
	Inc(ParamIndex)
end;

procedure CloseParams;
begin
	if IllegalParam then HelpParams;
end;

procedure HelpParams;
var
	i: SG;
	s: string;
begin
	s := 'Param.' + CharTab + 'Description' + LineSep;
	s := s + InsChar(96, '-') + LineSep;
	s := s + 'Help' + CharTab + 'Dislay this help dialog' + LineSep;
	for i := 0 to Length(Params) - 1 do
	begin
		s := s + Params[i] + CharTab + DesParams[i] + LineSep;
	end;
	MessageD(s, mtInformation, [mbOK]);
end;

procedure ReadMe;
begin
	ExtOpenFile(WorkDir + 'ReadMe.htm');
end;

procedure Homepage;
begin
	ExtOpenFile(MyWeb + '/Software.html');
end;

procedure Help;
begin
	ExtOpenFile(WorkDir + 'Help.rtf');
end;

procedure Help(HRef: string);
begin
	ExtOpenFile(WorkDir + 'Help.htm#' + HRef);
end;

procedure ExtOpenFile(FileName: TFileName);
var
	ErrorCode: U4;
begin
	ErrorCode := ShellExecute(0, 'open', PChar('"' + FileName + '"'), nil, nil, SW_ShowNormal);
	if ErrorCode <= 32 then
		IOError(FileName, ErrorCode);
end;

procedure ExtOpenFile(FileName: TFileName; Parameters: string);
var
	ErrorCode: U4;
begin
	ErrorCode := ShellExecute(0, 'open', PChar('"' + FileName + '"'), PChar(Parameters), nil, SW_ShowNormal);
	if ErrorCode <= 32 then
		IOError(FileName, ErrorCode);
end;

procedure ExecuteAbout2(AOwner: TComponent; FileName: TFileName; const Modal: Boolean);
var
	VersionInfo: TrpVersionInfo;
begin
	PlayWinSound(wsExclamation);
	if not Assigned(fAbout) then
	begin
		VersionInfo := TrpVersionInfo.Create(nil);
		BeginLongOperation;
		fAbout := TfAbout.Create(AOwner);
		fAbout.EditAuthor.Text := VersionInfo.Author;
//		fAbout.ImageVersion.Bitmap.Canvas.Font.Name := 'MS Sans Serif';
		fAbout.EditVersion.Text := VersionInfo.FileVersion;
		fAbout.EditCreationDate.Text := VersionInfo.LegalCopyright;
		fAbout.EditReleaseDate.Text := VersionInfo.ReleaseDate;
		fAbout.LoadFile(FileName);
		EndLongOperation(False);
		VersionInfo.Free;
	end
	else
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
				FileName := FileName + Application.Title;
			FileName := FileName + '.' + Ext[i];
			if FileExists(FileName) then
			begin
				ExecuteAbout2(AOwner, FileName, Modal);
				Exit;
			end;
		end;
	ExecuteAbout2(AOwner, '', Modal);
end;

procedure AboutRW(const Save: Boolean);
var
	FileName: TFileName;
	s: string;
begin
	if Save then
		RunTime := U8(GetTickCount - StartProgramTime) + RunProgramTime;

	if Save then
		s := 'F'
	else
		s := 'S';

	s := s + CharTab + DateTimeToS(Now) + CharCR + CharLF;
	FileName := DelFileExt(ExeFileName) + '.log';
	WriteStringToFile(FileName, s, True);

	if Assigned(MainIni) then
	begin
		MainIni.RWNum('Statistics', 'RunCount', RunCount, Save);
		MainIni.RWNum('Statistics', 'RunTime', RunTime, Save);
		MainIni.RWNum('Statistics', 'ReadCount', ReadCount, Save);
		MainIni.RWNum('Statistics', 'ReadBytes', ReadBytes, Save);
		MainIni.RWNum('Statistics', 'WriteCount', WriteCount, Save);
		MainIni.RWNum('Statistics', 'WriteBytes', WriteBytes, Save);
		if Save = False then
		begin
			Inc(RunCount);
			StartProgramTime := GetTickCount;
			RunProgramTime := RunTime;
		end;
	end;
	
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
		s := s + ' ' + DateTimeToS(Now);
		if not LogFile.Writeln(s) then goto LRetry;
		if not LogFile.Close then goto LRetry;
	end;
	LogFile.Free;}
end;

procedure TfAbout.InitNRT;
begin
	PanelNRT.Text := msToStr(GetTickCount - StartProgramTime + 1000 div 2, diDHMSD, 0, False);
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
		BmpAbout.GenerateRGB(GenFunc[RunCount mod (High(GenFunc) + 1)], AC, ScreenCorrectColor, ef16, nil);
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
	DButtonMemoryStatus.Visible := False;
	{$endif}

	Background := baGradient;
	EditEMail.Text := MyEMail + '?subject=' + Application.Title;
	EditWeb.Text := MyWeb;

	PanelRC.Text := NToS(RunCount);
	PanelTRT.Text := msToStr(RunTime, diDHMSD, 3, False);

	ImageName.Bitmap.Canvas.Brush.Style := bsClear;
	ImageName.Bitmap.Canvas.Font.Style := [fsBold];
	ImageName.Bitmap.Canvas.Font.Size := 12;
	ImageName.Bitmap.Canvas.Font.Name := 'Times New Roman';
	ImageName.Bitmap.Canvas.Font.Color := clBlack;
{	ImageVersion.Bitmap.Canvas.Brush.Style := bsClear;
	ImageVersion.Bitmap.Canvas.Font.Style := [fsBold];}

	InitNRT;

	if Assigned(MainIni) then
		MainIni.RWFormPos(Self, False);
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

procedure TfAbout.EditEMailClick(Sender: TObject);
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
				fSysInfo.FillComp;
			end;
		while NowTime - LMemClock >= PerformanceFrequency do
		begin
			Inc(LMemClock, PerformanceFrequency);
		end;
	end;

	ImageAbout.Fill;
	ImageName.Fill;
//	ImageVersion.Fill;
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
		Flash.Color.L := FireColor(256 + Random(256)); // SpectrumColor(Random(MaxSpectrum));
//	end;
end;

procedure TfAbout.ImageNameFill(Sender: TObject);
var
	BitmapName: TDBitmap;
	Co: array[0..3] of TColor;
begin
	BitmapName := ImageName.Bitmap;
//	BitmapName.GenRGB(clNone, gfSpecHorz, (16 * Timer1.Clock div PerformanceFrequency), ef16);

	BitmapName.GenerateRGBEx(0, 0, BitmapName.Width - 1, BitmapName.Height - 1, TGenFunc(Typ), Co, 0, ef16,
		(16 * Timer1.Clock div PerformanceFrequency), nil);

	BitmapName.Bar(clBtnFace, ef12);
	BitmapName.Canvas.Font.Color := clWindowText;
	DrawCutedText(BitmapName.Canvas, Rect(2, 2, BitmapName.Width - 2, BitmapName.Height - 2), taCenter, tlCenter,
		Application.Title, True, 1);
	{
	BitmapName.Canvas.TextOut(
		(BitmapName.Width -
		BitmapName.Canvas.TextWidth(ProgramName)) div 2,
		(BitmapName.Height -
		BitmapName.Canvas.TextHeight(ProgramName)) div 2,
		ProgramName);
	}
	BitmapName.Border(0, 0, BitmapName.Width - 1, BitmapName.Height - 1, clBlack, clWhite, 2, ef08);
end;
{
procedure TfAbout.ImageVersionFill(Sender: TObject);
var
	BitmapVersion: TDBitmap;
	Co: array[0..3] of TColor;
	s: string;
begin
	BitmapVersion := ImageVersion.Bitmap;
	BitmapVersion.Bar(clBtnFace, ef16);
	BitmapVersion.Canvas.Font.Color := clBlack;
	s := 'Version ' + GSysInfo.ProgramVersion;
	BitmapVersion.Canvas.TextOut(
		(BitmapVersion.Width -
		BitmapVersion.Canvas.TextWidth(s)) div 2,
		(BitmapVersion.Height -
		BitmapVersion.Canvas.TextHeight(s)) div 2,
		s);
//	BitmapVersion.GenRGB(clBtnFace, gfSpecHorz, (32 * Timer1.Clock div PerformanceFrequency), ef16);

	BitmapVersion.GenerateRGBEx(0, 0, BitmapVersion.Width - 1, BitmapVersion.Height - 1, TGenFunc(Typ), Co, 0, ef08,
		(32 * Timer1.Clock div PerformanceFrequency), nil);

	BitmapVersion.Border(0, 0, BitmapVersion.Width - 1, BitmapVersion.Height - 1, clBlack, clWhite, 2, ef08);
end;}

procedure TfAbout.ImageAboutFill(Sender: TObject);
var
	BitmapAbout: TDBitmap;
	HClock: Byte;
	i: UG;
	Flash: ^TFlash;
begin
	BitmapAbout := ImageAbout.Bitmap;
	BitmapAbout.Bar(clBtnFace, ef02);
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
	begin
		RotateDef(BitmapAbout, BmpAbout, Typ, (U8(AngleCount) * U8(Timer1.Clock) div (4 * PerformanceFrequency)) and (AngleCount - 1), TEffect(Effect));
	end;
	
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
			Pix(BitmapAbout.Data, BitmapAbout.ByteX, Flash.X, Flash.Y, Flash.Color, TEffect(Flash.Power div 16));
			Inc(i);
		end;
	end;
	BitmapAbout.Border(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, clBlack, clWhite, 3, ef08);
end;

procedure TfAbout.DButtonMemoryStatusClick(Sender: TObject);
begin
{$ifndef LINUX}
	if not Assigned(fMemStatus) then fMemStatus := TfMemStatus.Create(Self);
	fMemStatus.FormStyle := fAbout.FormStyle;
	fMemStatus.Left := fAbout.Left;
	fMemStatus.Top := fAbout.Top;
	fMemStatus.Show;
{$endif}
end;

procedure TfAbout.FormHide(Sender: TObject);
begin
	if Assigned(MainIni) then
		MainIni.RWFormPos(Self, True);
end;

{$ifopt d+}
{var
	MemCount, MemSize: SG;}
{$endif}
//P: Pointer;
initialization
{$ifopt d+}
{	MemCount := AllocMemCount;
	MemSize := AllocMemSize;}
{$endif}
	StartProgramTime := GetTickCount;
	Flashs := TData.Create(True);
	Flashs.ItemSize := SizeOf(TFlash);
//	GetMem(P, 1024);
finalization
	SetLength(Params, 0);
	SetLength(DesParams, 0);
	FreeAndNil(Flashs);
{$ifopt d+}
{	if (MemCount + 22 < AllocMemCount) {or
		(MemSize + 6508 < AllocMemSize) then
			Nop;}
//			MessageD('Memory Allocation Problem', mtWarning, [mbOk]);
{$endif}
end.
