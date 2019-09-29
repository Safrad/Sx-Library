unit uGColor;

interface

uses
	uParserMsg,

	Types,
  SysUtils, Classes, Graphics, Controls, Forms,
	StdCtrls, ComCtrls, ExtCtrls, Menus, uGraph, uDButton,
	uDLabel, ImgList, uDForm, uDBitmap, uDImage, uTypes, uMath, uColor, uDEdit,
	uDWinControl, uNamedColors, uSxLabel, uSxColor, System.ImageList;

type
	TOnApplyColor = procedure(Color: TColor);

	TfGColor = class(TDForm)
		LabelR: TDLabel;
		EditR: TDEdit;
		ButtonR: TDButton;
		ButtonOk: TDButton;
		ButtonApply: TDButton;
		ButtonCancel: TDButton;
		LabelG: TDLabel;
		EditG: TDEdit;
		ButtonG: TDButton;
		LabelB: TDLabel;
		EditB: TDEdit;
		ButtonB: TDButton;
		EditS: TDEdit;
		PopupMenu1: TPopupMenu;
		clScrollBar1: TMenuItem;
		clBackground: TMenuItem;
		clActiveCaption1: TMenuItem;
		clInactiveCaption1: TMenuItem;
		clMenu1: TMenuItem;
		clWindow1: TMenuItem;
		clWindowFrame1: TMenuItem;
		clMenuText1: TMenuItem;
		clWindowText1: TMenuItem;
		clCaptionText1: TMenuItem;
		clActiveBorder1: TMenuItem;
		clInactiveBorder1: TMenuItem;
		clAppWorkSpace1: TMenuItem;
		clHighlight1: TMenuItem;
		clHighlightText1: TMenuItem;
		clBtnFace1: TMenuItem;
		clBtnShadow1: TMenuItem;
		clGrayText1: TMenuItem;
		clBtnText1: TMenuItem;
		clInactiveCaptionText1: TMenuItem;
		clBtnHighlight1: TMenuItem;
		cl3DDkShadow1: TMenuItem;
		cl3DLight1: TMenuItem;
		clInfoText1: TMenuItem;
		clInfoBk1: TMenuItem;
		clNone1: TMenuItem;
		PanelH: TPanel;
		PanelL: TPanel;
    PanelPrevious: TSxColor;
    PanelCurrent: TSxColor;
		Bevel1: TBevel;
		ImageH: TDImage;
		ImageL: TDImage;
    PanelNowBitColor: TSxColor;
    PanelDefaultColor: TSxColor;
    LabelPrevious: TSxLabel;
    LabelNowXBit: TSxLabel;
    LabelDefault: TSxLabel;
    LabelCurrent: TSxLabel;
		ImageList1: TImageList;
		BevelBasicColors: TBevel;
		ShapeBorder: TShape;
		ComboBoxBitDepth: TComboBox;
		LabelH: TDLabel;
		EditL: TDEdit;
		LabelS: TDLabel;
		LabelL: TDLabel;
		PanelR: TPanel;
		ImageR: TDImage;
		PanelG: TPanel;
		ImageG: TDImage;
		PanelB: TPanel;
		ImageB: TDImage;
		EditH: TDEdit;
		PanelS: TPanel;
		ImageS: TDImage;
		ComboBoxNF: TComboBox;
		EditRGBA: TDEdit;
    LabelRGB: TSxLabel;
    LabelFormat: TSxLabel;
    Bevel3: TBevel;
		procedure FormDestroy(Sender: TObject);
		procedure ColorClick(Sender: TObject);
		procedure ButtonRGBAClick(Sender: TObject);
		procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
		procedure EditChange(Sender: TObject);
		procedure PanelDefaultColorClick(Sender: TObject);
		procedure PanelNowBitColorClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure AdvancedDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
		procedure FormCreate(Sender: TObject);
		procedure ImageFill(Sender: TObject);
		procedure ComboBoxBitDepthChange(Sender: TObject);
		procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure ComboBoxNFChange(Sender: TObject);
		procedure PanelPreviousClick(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	private
		{ Private declarations }
		Messages: TParserMessages;

		CurColor, DefColor: TColor;
		OnApply: TOnApplyColor;

		FNowColor: TColor;
		NowRGB: TRGBA;
		NowHLS: THLSColor;
		PanelColor: array of TDLabel;

		procedure RWOptions(const Save: BG);
		procedure InitReadOnly;
		procedure InitEdits(const SkipEdit: TDEdit);
		procedure ChangeColor;
		procedure InitAll;
		procedure SetNowColor(Color: TColor);
		function SetNowRGB(Color: TRGBA): BG;
		function SetNowHLS(HLS: THLSColor): BG;

		procedure PanelColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure PanelColorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);
		procedure PanelColorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

    function ColorToString(const AColor: TColor): string;
    function ColorToStringNamed(const AColor: TColor): string;
	end;

function GetColor(const prompt: string; var CurrentColor: TColor; const DefaultColor: TColor;
	OnApply: TOnApplyColor): Boolean;

implementation

{$R *.DFM}

uses
	Math,
	uMenus, uInputFormat, uOutputFormat, uDIniFile, uLayout, uDrawStyle, uDictionary, uStrings;

const
	SpectrumPixel = 4;
	LightPixel = 1;

var
	fGColor: TfGColor;

function IntToColor(const i: Integer): TRGBA;
var
	a: Integer;
begin
	case i of
	0 .. 23:
		begin
			Result.L := SpectrumColor((255 * (i mod 12) + 1 - ((i div 2) and 1)) div 2);
			if i > 11 then
			begin
				Result.R := (Result.R + 1) div 2;
				Result.G := (Result.G + 1) div 2;
				Result.B := (Result.B + 1) div 2;
			end;
		end;
	24:
		Result.L := 0;
	25 .. 31:
		begin
			a := 256 * (i - 23) div 8;
			if a > 255 then
				a := 255;
			Result.R := a;
			Result.G := Result.R;
			Result.B := Result.R;
			Result.a := 0;
		end;
  else
		Result.L := TNamedColors.GetColor(TNamedColorEnum(i - 32));
	end;
end;

function AbsoluteColor(C: TColor): TColor;
var
	CA: array [0 .. 2] of U1;
	i: Integer;
begin
	Result := ColorToRGB(C) and $00FFFFFF;
	CA[0] := TRGBA(Result).R;
	CA[1] := TRGBA(Result).G;
	CA[2] := TRGBA(Result).B;
	for i := 0 to 2 do
	begin
		if (CA[i] >= CA[(i + 1) mod 3]) and (CA[i] >= CA[(i + 2) mod 3]) then
		begin
			if CA[i] = 0 then
			begin

			end
			else if CA[i] < 255 then
			begin
				CA[(i + 1) mod 3] := 255 * CA[(i + 1) mod 3] div CA[i];
				CA[(i + 2) mod 3] := 255 * CA[(i + 2) mod 3] div CA[i];
				CA[i] := 255;
				TRGBA(Result).R := CA[0];
				TRGBA(Result).G := CA[1];
				TRGBA(Result).B := CA[2];
			end;
			Break;
		end;
	end;
end;

function GetVGAPalete(C: TColor): TColor;
const
	VGAColors: array [0 .. 15] of TColor = (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple,
		clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
var
	i: Integer;
	Dif, BestDif: Integer;
begin
	BestDif := High(BestDif);
	C := ColorToRGB(C) and $00FFFFFF;
	Result := clBlack;
	for i := 0 to 15 do
	begin
		Dif := Abs(TRGBA(VGAColors[i]).R - TRGBA(C).R) + Abs(TRGBA(VGAColors[i]).G - TRGBA(C).G) + Abs
			(TRGBA(VGAColors[i]).B - TRGBA(C).B);
		if Dif <= BestDif then
		begin
			Result := VGAColors[i];
			BestDif := Dif;
		end;
	end;
end;

function BitColor(const C: TRGBA; const Bits: U1): TRGBA;
begin
	Result.a := 0;
	case Bits of
	1:
		Result.L := NegMonoColor(NegMonoColor(C.L));
	4:
		begin
			Result.L := GetVGAPalete(C.L);
		end;
	15:
		begin
			Result.R := C.R shr 3;
			Result.R := 255 * Result.R div 31;
			Result.G := C.G shr 3;
			Result.G := 255 * Result.G div 31;
			Result.B := C.B shr 3;
			Result.B := 255 * Result.B div 31;
		end;
	18:
		begin
			Result.R := C.R shr 2;
			Result.R := 255 * Result.R div 63;
			Result.G := C.G shr 2;
			Result.G := 255 * Result.G div 63;
			Result.B := C.B shr 2;
			Result.B := 255 * Result.B div 63;
		end;
	24:
		begin
			Result.L := C.L and $00FFFFFF;
		end;
	else
		Result.L := C.L;
	end;
end;

procedure CreateBox(const i, L, T: SG);
begin
	fGColor.PanelColor[i].BevelOuter := bvNone;
	fGColor.PanelColor[i].BorderStyle := bsSingle;
	fGColor.PanelColor[i].SetBounds(L, T, LgToPx(16), LgToPx(16));
	fGColor.PanelColor[i].Tag := i;
	fGColor.PanelColor[i].OnMouseDown := fGColor.PanelColorMouseDown;
	fGColor.PanelColor[i].OnMouseUp := fGColor.PanelColorMouseUp;
	fGColor.PanelColor[i].OnMouseMove := fGColor.PanelColorMouseMove;
end;

function GetColor(const prompt: string; var CurrentColor: TColor; const DefaultColor: TColor;
	OnApply: TOnApplyColor): Boolean;
const
  XCount = 12;
var
	i, L, T: Integer;
  ColorCount: SG;
begin
	if not Assigned(fGColor) then
	begin
		fGColor := TfGColor.Create(Application.MainForm);
    ColorCount := 32 + SG(High(TNamedColorEnum)) + 1;
    SetLength(fGColor.PanelColor, ColorCount);
		for i := 0 to ColorCount - 1 do
		begin
			fGColor.PanelColor[i] := TDLabel.Create(fGColor);
			case i of
			0 .. 23:
				begin
					L := 8 + 20 * (i mod XCount);
					T := 4 + 18 * (i div XCount);
				end;
			24 .. 31:
				begin
					L := 8 + 20 * (i - 24);
					T := 4 + 18 * 2 + 9;
				end;
			else
				begin
					L := 8 + 20 * ((i - 32) mod XCount);
					T := 4 + 18 * (4 + (i - 32) div XCount);
				end;
			end;
			CreateBox(i, fGColor.BevelBasicColors.Left + LgToPx(L), fGColor.BevelBasicColors.Top + LgToPx(T));
			fGColor.PanelColor[i].Color := IntToColor(i).L;
			fGColor.PanelColor[i].Hint := fGColor.ColorToStringNamed(fGColor.PanelColor[i].Color);
			fGColor.PanelColor[i].ShowHint := True;
			fGColor.InsertControl(fGColor.PanelColor[i]);
		end;
	end;
	fGColor.OnApply := OnApply;
	fGColor.ButtonApply.Enabled := Assigned(OnApply);

	fGColor.CurColor := CurrentColor;
	fGColor.DefColor := DefaultColor;
	fGColor.Caption := Translate(prompt);
	fGColor.SetNowColor(CurrentColor);

	fGColor.PanelPrevious.Color := fGColor.CurColor;
	fGColor.PanelDefaultColor.Color := fGColor.DefColor;

	fGColor.InitAll;

	if Assigned(OnApply) then
	begin
		fGColor.FormStyle := fsStayOnTop;
		fGColor.Show;
		Result := True;
	end
	else
	begin
		fGColor.FormStyle := fsNormal;
		if fGColor.ShowModal = mrOK then
		begin
			CurrentColor := fGColor.FNowColor;
			Result := True;
		end
		else
		begin
			Result := False;
		end;
	end;
end;

// TfGColor

const
	ABits: array [0 .. 5] of U1 = (1, 4, 15, 18, 24, 32);

procedure TfGColor.InitReadOnly;
var
	C: TRGBA;
	i: Integer;
	Vis: Boolean;
  CShapeBorder: SG;
begin
	PanelCurrent.Color := FNowColor; // NowRGB.L;
	PanelCurrent.Update;

	PanelNowBitColor.Color := BitColor(NowRGB, ABits[ComboBoxBitDepth.ItemIndex]).L;
	ImageR.Invalidate;
	ImageG.Invalidate;
	ImageB.Invalidate;
	ImageH.Invalidate;
	ImageL.Invalidate;
	ImageS.Invalidate;

	Vis := False;
  CShapeBorder := LgToPx(2);
	for i := 0 to Length(PanelColor) - 1 do
	begin
		if C.L = IntToColor(i).L then
		begin
			Vis := True;
			ShapeBorder.SetBounds(
        PanelColor[i].Left - CShapeBorder,
        PanelColor[i].Top - CShapeBorder,
        PanelColor[i].Width + 2 * CShapeBorder - 1,
        PanelColor[i].Height + 2 * CShapeBorder - 1);
			Break;
		end;
	end;
	ShapeBorder.Visible := Vis;
	ShapeBorder.Update;
end;

procedure TfGColor.InitEdits(const SkipEdit: TDEdit);
var
	NumericPref: string;
begin
	EditR.OnChange := nil;
	EditG.OnChange := nil;
	EditB.OnChange := nil;
	EditRGBA.OnChange := nil;
	EditH.OnChange := nil;
	EditL.OnChange := nil;
	EditS.OnChange := nil;

  if SkipEdit <> EditRGBA then
  begin
    EditRGBA.Text := ColorToString(FNowColor);
    EditRGBA.Update;
  end;

  case ComboBoxNF.ItemIndex of
    0:
    begin
      NumericPref := '#'
    end
    else
    begin
      NumericPref := '$'
    end;
  end;
  if SkipEdit <> EditR then
  begin
    EditR.Text := NumericPref + IntToHex(NowRGB.R);
    EditR.Update;
  end;
  if SkipEdit <> EditG then
  begin
    EditG.Text := NumericPref + IntToHex(NowRGB.G);
    EditG.Update;
  end;
  if SkipEdit <> EditB then
  begin
    EditB.Text := NumericPref + IntToHex(NowRGB.B);
    EditB.Update;
  end;
  if SkipEdit <> EditH then
  begin
    EditH.Text := NumericPref + IntToHex(NowHLS.H);
    EditH.Update;
  end;
  if SkipEdit <> EditL then
  begin
    EditL.Text := NumericPref + IntToHex(NowHLS.L);
    EditL.Update;
  end;
  if SkipEdit <> EditS then
  begin
    EditS.Text := NumericPref + IntToHex(NowHLS.S);
    EditS.Update;
  end;

	EditR.OnChange := EditChange;
	EditG.OnChange := EditChange;
	EditB.OnChange := EditChange;
	EditRGBA.OnChange := EditChange;
	EditH.OnChange := EditChange;
	EditL.OnChange := EditChange;
	EditS.OnChange := EditChange;
end;

procedure TfGColor.ChangeColor;
begin
	if Assigned(OnApply) then
		OnApply(FNowColor);
end;

procedure TfGColor.InitAll;
begin
	InitReadOnly;
	InitEdits(nil);
	ChangeColor;
end;

procedure TfGColor.SetNowColor(Color: TColor);
begin
	FNowColor := Color;
	NowRGB.L := ColorToRGB(FNowColor) and $00FFFFFF;
	NowHLS := RGBToHLS(NowRGB);
end;

function TfGColor.SetNowRGB(Color: TRGBA): BG;
begin
	if (NowRGB.L <> Color.L) or (FNowColor <> NowRGB.L) then
	begin
		NowRGB := Color;
		FNowColor := NowRGB.L;
		NowHLS := RGBToHLS(NowRGB);
		Result := True;
	end
	else
		Result := False;
end;

function TfGColor.SetNowHLS(HLS: THLSColor): BG;
begin
	if (NowHLS.A <> HLS.A) or (FNowColor <> NowHLS.L) then
	begin
		NowHLS := HLS;
		NowRGB := HLSToRGB(HLS);
		FNowColor := NowRGB.L;
		Result := True;
	end
	else
		Result := False;
end;

procedure TfGColor.PanelColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	SetNowColor(IntToColor(TComponent(Sender).Tag).L);
	InitAll;
end;

procedure TfGColor.PanelColorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
end;

procedure TfGColor.PanelColorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfGColor.FormDestroy(Sender: TObject);
var
	i: Integer;
begin
	for i := 0 to Length(PanelColor) - 1 do
	begin
		if PanelColor[i] <> nil then
		begin
			RemoveControl(PanelColor[i]);
			FreeAndNil(PanelColor[i]);
		end;
	end;
end;

procedure TfGColor.ColorClick(Sender: TObject);
begin
	if TMenuItem(Sender).Tag < 0 then
		SetNowColor(clNone)
	else
		SetNowColor(TColor(U4(TMenuItem(Sender).Tag) or clSystemColor));
	InitAll;
end;

procedure TfGColor.ButtonRGBAClick(Sender: TObject);
begin
	case TButton(Sender).Tag of
	0:
		NowRGB.R := 255 - NowRGB.R;
	1:
		NowRGB.G := 255 - NowRGB.G;
	2:
		NowRGB.B := 255 - NowRGB.B;
	end;
	if SetNowRGB(NowRGB) then
		InitAll;
end;

procedure TfGColor.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	B: U1;
	Changed: BG;
	NewHLS: THLSColor;
begin
	if TDImage(Sender).MouseL then
	begin
		X := Range(0, X, TDImage(Sender).Width - 1);
		B := Range(0, RoundDiv(256 * X, (TDImage(Sender).Bitmap.Width)), 255);
		Changed := False;
		case TDImage(Sender).Tag of
		0:
			begin
				NowRGB.R := B;
				Changed := SetNowRGB(NowRGB);
			end;
		1:
			begin
				NowRGB.G := B;
				Changed := SetNowRGB(NowRGB);
			end;
		2:
			begin
				NowRGB.B := B;
				Changed := SetNowRGB(NowRGB);
			end;
		3:
			begin
				NewHLS := NowHLS;
				NewHLS.H := RoundDiv((MaxSpectrum + 1) * X, TDImage(Sender).Bitmap.Width);
				Changed := SetNowHLS(NewHLS);
			end;
		4:
			begin
				NewHLS := NowHLS;
				NewHLS.L := B;
				Changed := SetNowHLS(NewHLS);
			end;
		5:
			begin
				NewHLS := NowHLS;
				NewHLS.S := B;
				Changed := SetNowHLS(NewHLS);
			end;
		end;
		if Changed then
			InitAll;
	end;
end;

procedure TfGColor.EditChange(Sender: TObject);
var
	Changed: BG;
  s: string;
begin
  s := TDEdit(Sender).Text;
  s := ReplaceF(s, '#', '$');
	case TComponent(Sender).Tag of
	- 1:
  begin
		NowRGB.L := StrToValS8(s, True, MinInt, NowRGB.L, High(U4), 1, Messages);
    if ComboBoxNF.ItemIndex = 0 then
      Exchange(NowRGB.R, NowRGB.B);
  end;
	0:
		NowRGB.R := StrToValU1(s, True, NowRGB.R, Messages);
	1:
		NowRGB.G := StrToValU1(s, True, NowRGB.G, Messages);
	2:
		NowRGB.B := StrToValU1(s, True, NowRGB.B, Messages);
	3:
		NowHLS.H := StrToValI(s, True, -1, NowHLS.H, MaxSpectrum, 1, Messages);
	4:
		NowHLS.L := StrToValU1(s, True, NowHLS.L, Messages);
	5:
		NowHLS.S := StrToValU1(s, True, NowHLS.S, Messages);
	end;
	TDEdit(Sender).Hint := Messages.ToString;
	TDEdit(Sender).ShowHint := True;
	Messages.Clear;
	if TDEdit(Sender).Tag <= 2 then
		Changed := SetNowRGB(NowRGB)
	else
		Changed := SetNowHLS(NowHLS);

	if Changed then
	begin
		InitReadOnly;
		InitEdits(TDEdit(Sender));
		ChangeColor;
	end;
end;

procedure TfGColor.PanelDefaultColorClick(Sender: TObject);
begin
	SetNowColor(DefColor);
	InitAll;
end;

procedure TfGColor.PanelNowBitColorClick(Sender: TObject);
begin
	SetNowColor(PanelNowBitColor.Color);
	InitAll;
end;

procedure TfGColor.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		if FNowColor <> CurColor then
			OnApply(CurColor);
		Close;
	end;
end;

procedure TfGColor.ButtonOkClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		OnApply(FNowColor);
		Close;
	end;
end;

procedure TfGColor.AdvancedDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
	State: TOwnerDrawState);
var
	Bmp: TBitmap;
	Rec: TRect;
	C: TColor;
begin
	Bmp := TBitmap.Create;
	Bmp.Width := 16;
	Bmp.Height := 16;
	Rec.Left := 0;
	Rec.Top := 0;
	Rec.Right := Bmp.Width;
	Rec.Bottom := Bmp.Height;

	C := TColor(U4(TMenuItem(Sender).Tag) or $80000000);
	Bmp.Canvas.Brush.Color := NegMonoColor(C);
	Bmp.Canvas.FrameRect(Rec);
	InflateRect(Rec, -1, -1);
	Bmp.Canvas.Brush.Color := C;
	Bmp.Canvas.FillRect(Rec);
	ImageList1.Clear;
	ImageList1.Add(Bmp, nil);
	Bmp.Free;
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State);
end;

procedure TfGColor.RWOptions(const Save: BG);
begin
	if Assigned(MainIni) then
	begin
		MainIni.RWComboBox('ColorDialog', ComboBoxBitDepth, Save);
		MainIni.RWComboBox('ColorDialog', ComboBoxNF, Save);
	end;
end;

procedure TfGColor.FormCreate(Sender: TObject);
var
	i: SG;
begin
	Background := baGradient;
	LabelR.Color := MixColors(clBtnFace, clRed);
	LabelG.Color := MixColors(clBtnFace, clLime);
	LabelB.Color := MixColors(clBtnFace, clBlue);
	LabelR.Font.Color := NegMonoColor(LabelR.Color);
	LabelG.Font.Color := NegMonoColor(LabelG.Color);
	LabelB.Font.Color := NegMonoColor(LabelB.Color);

	ComboBoxBitDepth.Items.BeginUpdate;
	try
		for i := 0 to Length(ABits) - 1 do
			ComboBoxBitDepth.Items.Add(NToS(ABits[i]) + ' bit');
		ComboBoxBitDepth.ItemIndex := Length(ABits) - 1;
	finally
		ComboBoxBitDepth.Items.EndUpdate;
	end;
	RWOptions(False);
end;

procedure TfGColor.ImageFill(Sender: TObject);
var
	BmpD: TDBitmap;
	i: Integer;
	B: U1;
	C: TRGBA;
	HLS: THLSColor;
begin
	BmpD := TDImage(Sender).Bitmap;
	C := NowRGB;
	HLS := NowHLS;
	for i := 0 to BmpD.Width - 1 do
	begin
		B := RoundDiv(SG(255 * i), (BmpD.Width - 1));
		case TDImage(Sender).Tag of
		0 .. 2:
			C.i[TDImage(Sender).Tag] := B;
		3:
			begin
				if HLS.S <> 0 then
					HLS.H := RoundDiv(MaxSpectrum * i, (BmpD.Width - 1))
				else
					HLS.H := -1;
				C := HLSToRGB(HLS);
			end;
		4:
			begin
				HLS.L := B;
				C := HLSToRGB(HLS);
			end;
		5:
			begin
				HLS.S := B;
				C := HLSToRGB(HLS);
			end;
		end;
		BmpD.Line(i, 0, i, BmpD.Height - 1, C.L, ef16, LgToPx(1));
	end;

	i := -1;
	case TDImage(Sender).Tag of
	0:
		i := RoundDiv(SG(BmpD.Width * NowRGB.R), (256 - 1));
	1:
		i := RoundDiv(SG(BmpD.Width * NowRGB.G), (256 - 1));
	2:
		i := RoundDiv(SG(BmpD.Width * NowRGB.B), (256 - 1));
	3:
		begin
			if NowHLS.H >= 0 then
				i := RoundDiv(SG((BmpD.Width) * NowHLS.H), (MaxSpectrum + 1));
		end;
	4:
		i := RoundDiv(SG(BmpD.Width * NowHLS.L), (256 - 1));
	5:
		i := RoundDiv(SG(BmpD.Width * NowHLS.S), (256 - 1));
	end;
	if i >= 0 then
		BmpD.Line(i, 0, i, BmpD.Height - 1, NegMonoColor(FNowColor), ef16, LgToPx(1));
end;

procedure TfGColor.ComboBoxBitDepthChange(Sender: TObject);
begin
	InitAll;
end;

procedure TfGColor.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	ImageMouseMove(Sender, Shift, X, Y);
end;

procedure TfGColor.ComboBoxNFChange(Sender: TObject);
begin
	InitEdits(nil);
end;

procedure TfGColor.PanelPreviousClick(Sender: TObject);
begin
	SetNowColor(CurColor);
	InitAll;
end;

procedure TfGColor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	RWOptions(True);
end;

constructor TfGColor.Create(AOwner: TComponent);
begin
	inherited;
	Messages := TParserMessages.Create;
	LayoutControls([ButtonOk, ButtonCancel, ButtonApply], ClientWidth, ClientHeight);
end;

destructor TfGColor.Destroy;
begin
	FreeAndNil(Messages);
	inherited;
end;

function TfGColor.ColorToString(const AColor: TColor): string;
begin
  case ComboBoxNF.ItemIndex of
  0: Result := ColorToHTMLString(AColor);
  1: Result := '$' + IntToHex(TRGBA(AColor).B, 2) + IntToHex(TRGBA(AColor).G, 2) + IntToHex(TRGBA(AColor).R, 2);
  else
  begin
    Result := NToS(U4(AColor));
  end;
  end;
end;

function TfGColor.ColorToStringNamed(const AColor: TColor): string;
begin
	Result := TNamedColors.GetName(AColor);
  if Result = '' then
    Result := ColorToString(AColor);
end;

end.
