unit uAbout;

interface

uses
  SysUtils,
  Types,
  Classes,
	Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
	Vcl.ExtCtrls,

  uDButton,
  uDLabel,
  uDTimer,
  uDImage,
  uDEdit,
  uDView,
	uDForm,
  uTypes,
  uDBitmap,
  uDWinControl,
  uSparks;

type
	TfAbout = class(TDForm)
    TimerFlash: TDTimer;
		ButtonOk: TDButton;
		ImageAbout: TDImage;
		ButtonSysInfo: TDButton;
		DViewAbout: TDView;
		ButtonStatistics: TDButton;
		ButtonVersionInfo: TDButton;
    ButtonBuildParams: TDButton;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure ButtonOkClick(Sender: TObject);
		procedure ImageAboutMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure EditWebClick(Sender: TObject);
		procedure EditEMailClick(Sender: TObject);
		procedure DTimerFlashTimer(Sender: TObject);
		procedure ButtonSysInfoClick(Sender: TObject);
		procedure ImageAboutFill(Sender: TObject);
		procedure DViewAboutGetData(Sender: TObject; var Data: String;
			ColIndex, RowIndex: Integer; Rect: TRect);
		procedure ButtonXClick(Sender: TObject);
		procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonBuildParamsClick(Sender: TObject);
    procedure DViewAboutCellClick(Sender: TObject; ColumnIndex,
      RowIndex: Integer; Shift: TShiftState);
	private
		Effect: U1;
		Typ: U1;
		Reset: Boolean;
		BmpAbout: TDBitmap;
		ViewInfo: SG;
		UsedValues: array of SG;
		UsedValueCount: SG;
    FSparks: TSparks;
		procedure UpdateView;
		procedure RWOptions(const Save: BG);
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
  Winapi.Windows,
  Winapi.Messages,

  uCommonApplication,
  uMainTimer,
	uAPI,
  uHTML,
  uDictionary,
	uProjectInfo,
	uGraph,
  uMainCfg,
  uGUIMainCfg,
  ufSysInfo,
  uSystemPaths,
  uFileStatistics,
  uMsg,
  uDrawStyle,
	uStrings,
  uOutputFormat,
  uLgToPx;

function GetLocalHomepage: TFileName;
begin
	Result := SystemPaths.WorkDir + 'ReadMe' + PathDelim + IndexFile;
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
	if not Assigned(fAbout) then
	begin
		fAbout := TfAbout.Create(AOwner);
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

function GetFileName(const Base: string): TFileName; overload;
var
	i: SG;
	FileName: TFileName;
begin
	Result := '';
	for i := 0 to Length(PrefferedExt) - 1 do
	begin
		FileName := SystemPaths.GraphDir + Base + '.' + PrefferedExt[i];
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
	if Result <> '' then
    Exit;
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
		BmpAbout.GenerateRGB(GenFunc[CommonApplication.Statistics.RunCount mod (High(GenFunc) + 1)], AC, ef16, nil);
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
	ButtonBuildParams.Visible := IsDebug;

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
	MainCfg.RegisterRW(RWOptions);

  FSparks := TSparks.Create;
  FSparks.MaxY := ImageAbout.Height;
end;

procedure TfAbout.FormDestroy(Sender: TObject);
begin
  FSparks.Free;

	MainCfg.UnregisterRW(RWOptions);
	if Assigned(BmpAbout) then
	begin
		FreeAndNil(BmpAbout);
	end;
end;

procedure TfAbout.FormShow(Sender: TObject);
begin
	TimerFlash.Enabled:= True;
end;

procedure TfAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	TimerFlash.Enabled:= False;
end;

procedure TfAbout.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfAbout.ImageAboutMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  kSC_DragMove = $F012 ;
begin
	ReleaseCapture;
	ImageAbout.Perform(WM_SYSCOMMAND, kSC_DragMove, 0);
	if Button = mbLeft then
	begin
		if Typ >= MaxTyp then
      Typ := 0
    else
      Inc(Typ);
	end
	else if Button = mbRight then
	begin
		if Typ <= 0 then
      Typ := MaxTyp
    else
      Dec(Typ);
	end;
end;

procedure TfAbout.EditWebClick(Sender: TObject);
begin
	OpenWebHomepage;
end;

procedure TfAbout.EditEMailClick(Sender: TObject);
begin
	APIOpen('mailto: ' + GetProjectInfo(piEMail));
end;

procedure TfAbout.DTimerFlashTimer(Sender: TObject);
begin
	FSparks.New(ImageAbout.LMouseX, ImageAbout.LMouseY);
  FSparks.Update;
	ImageAbout.Invalidate;
end;

procedure TfAbout.ButtonSysInfoClick(Sender: TObject);
begin
	DisplaySysInfo(Self);
end;

procedure TfAbout.ImageAboutFill(Sender: TObject);
var
	BitmapAbout: TDBitmap;
	HClock: U1;
	i: SG;
	Co: array[0..3] of TColor;
begin
	BitmapAbout := ImageAbout.Bitmap;
	BitmapAbout.GenerateRGBEx(0, 0, BitmapAbout.Width - 1, BitmapAbout.Height - 1, TGenFunc(Typ), Co, ef03,
		(16 * TimerFlash.Clock div MainTimer.Frequency), nil);

	HClock := (32 * TimerFlash.Clock div MainTimer.Frequency) and $7f;
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
		RotateDef(BitmapAbout, BmpAbout, Typ, (U8(AngleCount) * U8(TimerFlash.Clock) div (8 * MainTimer.Frequency)) and (AngleCount - 1), TEffect(Effect));
	end;

  FSparks.RenderToBitmap(BitmapAbout);

	if TimerFlash.TimerCount < 200 then
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
			0: Data := NToS(CommonApplication.Statistics.RunCount);
			1: Data := MsToStr(CommonApplication.Statistics.ElapsedTime.Milliseconds, TDisplay.diDHMSD, 0, False);
			2: Data := MsToStr(CommonApplication.Statistics.TotalElapsedTime.Milliseconds, TDisplay.diDHMSD, 3, False);
			3: Data := NToS(FileStatistics.ReadCount);
			4: Data := BToStr(FileStatistics.ReadBytes);
			5: Data := NToS(FileStatistics.WriteCount);
			6: Data := BToStr(FileStatistics.WriteBytes);
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
	if Assigned(GUIMainCfg) then
	begin
		GUIMainCfg.RWFormPos(Self, Save);
		DViewAbout.Serialize(MainCfg, Save);
	end;
	if Save = False then
		UpdateView;
end;

procedure TfAbout.FormResize(Sender: TObject);
begin
	// Height
	ButtonOk.Top := ClientHeight - FormBorder - ButtonOk.Height;
	ButtonSysInfo.Top := ClientHeight - FormBorder - ButtonSysInfo.Height;
  ButtonBuildParams.Top := ClientHeight - FormBorder - ButtonBuildParams.Height;


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

procedure TfAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl, ssAlt])then
  begin
    if Char(Key) = 'B' then
      ButtonBuildParamsClick(Sender);
  end;
end;

procedure TfAbout.ButtonBuildParamsClick(Sender: TObject);
var
  s: string;

  procedure Append(const ABuildParam: string; const ATurnedOn: BG);
  begin
    s := s + ABuildParam;
    if ATurnedOn then
      s := s + '+'
    else
      s := s + '-';
    s := s + LineSep;
  end;

begin
  s := '';

  s := s + 'Code generation:' + LineSep;
  Append('o (Optimization)', {$ifopt o+}True{$else}False{$endif});
  Append('w (Stack frames)', {$ifopt w+}True{$else}False{$endif});
  Append('u (Pentium-safe FDIV)', {$ifopt u+}True{$else}False{$endif});
  Append('a (Record field alignment)', {$ifopt a+}True{$else}False{$endif});

  s := s + 'Runtime errors:' + LineSep;
  Append('r (Range checking)', {$ifopt r+}True{$else}False{$endif});
  Append('i (I/O checking)', {$ifopt i+}True{$else}False{$endif});
  Append('q (Overflow checking)', {$ifopt q+}True{$else}False{$endif});

  s := s + 'Debugging:' + LineSep;
  Append('d (Debug information)', {$ifopt d+}True{$else}False{$endif});
  Append('l (Local symbols)', {$ifopt l+}True{$else}False{$endif});
  Append('y (Reference info)', {$ifopt y+}True{$else}False{$endif});
  Append('c (Assertions)', {$ifopt c+}True{$else}False{$endif});
  Append('m (Runtime type information)', {$ifopt m+}True{$else}False{$endif});
  Append('h (Show Hints)', {$ifopt h+}True{$else}False{$endif});

  Information(s);
end;

procedure TfAbout.DViewAboutCellClick(Sender: TObject; ColumnIndex,
  RowIndex: Integer; Shift: TShiftState);
var
  CellData: string;
  Rect: TRect;
begin
  DViewAboutGetData(Sender, CellData, ColumnIndex, RowIndex, Rect);
  if StartStr('http', CellData) then
  begin
    APIOpen(CellData);
  end;
end;

end.
