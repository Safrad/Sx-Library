//* File:     Lib\uGColor.pas
//* Created:  1999-09-01
//* Modified: 2005-08-28
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uGColor;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls, ExtCtrls, Menus, uGraph, uDButton,
	uDLabel, ImgList, uDForm, uDBitmap, uDImage, uTypes, uMath;

const
	MaxColor = 6 * 4 + 3 * 4 - 1;
type
	TOnApplyColor = procedure(Color: TColor);

	TfGColor = class(TDForm)
    LabelR: TDLabel;
		EditR: TEdit;
		ButtonR: TDButton;
		ButtonOk: TDButton;
		ButtonApply: TDButton;
		ButtonCancel: TDButton;
    LabelG: TDLabel;
		EditG: TEdit;
    ButtonG: TDButton;
    LabelB: TDLabel;
		EditB: TEdit;
    ButtonB: TDButton;
    EditS: TEdit;
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
    PanelNowColor: TDButton;
    PanelCurColor: TDButton;
		Bevel1: TBevel;
    ImageH: TDImage;
		ImageL: TDImage;
    PanelNowBitColor: TDButton;
    PanelDefaultColor: TDButton;
		LabelNow: TDLabel;
		LabelNowXBit: TDLabel;
		LabelDefault: TDLabel;
		LabelCurrent: TDLabel;
		Bevel2: TBevel;
		ImageList1: TImageList;
    BevelBasicColors: TBevel;
    ShapeBorder: TShape;
		ComboBoxBitDepth: TComboBox;
    DLabelH: TDLabel;
    EditL: TEdit;
    DLabelS: TDLabel;
    DLabelL: TDLabel;
    PanelR: TPanel;
		ImageR: TDImage;
    PanelG: TPanel;
    ImageG: TDImage;
    PanelB: TPanel;
    ImageB: TDImage;
    EditH: TEdit;
    PanelS: TPanel;
    ImageS: TDImage;
    ComboBoxNF: TComboBox;
		procedure FormDestroy(Sender: TObject);
		procedure ColorClick(Sender: TObject);
		procedure PanelCurColorClick(Sender: TObject);
		procedure ButtonRGBAClick(Sender: TObject);
		procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure EditRGBAChange(Sender: TObject);
		procedure PanelDefaultColorClick(Sender: TObject);
		procedure PanelNowBitColorClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure AdvancedDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
			State: TOwnerDrawState);
		procedure FormCreate(Sender: TObject);
		procedure ImageFill(Sender: TObject);
		procedure ComboBoxBitDepthChange(Sender: TObject);
		procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ComboBoxNFChange(Sender: TObject);
	private
		{ Private declarations }
		CurColor, DefColor: TColor;
		OnApply: TOnApplyColor;

		NumericPref: string;
		FNowColor: TColor;
		NowRGB: TRColor;
		NowHLS: THLSColor;
		PanelColor: array[0..MaxColor] of TPanel;

		procedure InitReadOnly;
		procedure InitEditsRGB;
		procedure InitEditsHLS;
		procedure ChangeColor;
		procedure InitAll;
		procedure SetNowColor(Color: TColor);
		procedure SetNowRGB(Color: TRColor);
		procedure SetNowHLS(HLS: THLSColor);

		procedure PanelColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure PanelColorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure PanelColorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
	public
		{ Public declarations }
	end;

procedure InitButton(Button: TDButton);

function GetColor(const prompt: string;
	var CurrentColor: TColor; const DefaultColor: TColor; OnApply: TOnApplyColor): Boolean;

implementation

{$R *.DFM}
uses
	Math,
	uMenus, uInput, uFormat;

procedure InitButton(Button: TDButton);
begin
	if Button.Color = clNone then
	begin
		Button.Font.Color := clWindowText;
		Button.Caption := ColorToString(clNone);
	end
	else
	begin
		Button.Font.Color := NegMonoColor(Button.Color);
		Button.Caption := ColorToString(Button.Color);
	end;
end;

const
	SpectrumPixel = 4;
	LightPixel = 1;
var
	fGColor: TfGColor;

function IntToColor(const i: Integer): TRColor;
var
	C: TRColor;
	a: Integer;
begin
		case i of
		0..23:
		begin
			C.L := SpectrumColor(255 * (i mod 12) div 2);
			if i > 11 then
			begin
				C.R := (C.R + 1) div 2;
				C.G := (C.G + 1) div 2;
				C.B := (C.B + 1) div 2;
			end;
		end;
		24:
			C.L := 0;
		25..31:
		begin
			a := 256 * (i - 23) div 8;
			if a > 255 then a := 255;
			C.R := a;
			C.G := C.R;
			C.B := C.R;
			C.A := 0;
		end;
		32..35:
		begin
			case i of
			32: C.L := clMoneyGreen;
			33: C.L := clSkyBlue;
			34: C.L := clCream;
			35: C.L := clMedGray;
			end;
		end;
		end;
		Result := C;
end;

function AbsoluteColor(C: TColor): TColor;
var
	RC: TRColor;
	CA: array[0..2] of Byte;
	i: Integer;
begin
	RC.L := ColorToRGB(C) and $00ffffff;
	Result := RC.L;
	CA[0] := RC.R;
	CA[1] := RC.G;
	CA[2] := RC.B;
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
				TRColor(Result).R := CA[0];
				TRColor(Result).G := CA[1];
				TRColor(Result).B := CA[2];
			end;
			Break;
		end;
	end;
end;

function GetVGAPalete(C: TColor): TColor;
const
	VGAColors: array[0..15] of TColor =
		(clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
		clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
var
	i: Integer;
	Dif, BestDif: Integer;
begin
	BestDif := High(BestDif);
	C := ColorToRGB(C) and $00ffffff;
	Result := clBlack;
	for i := 0 to 15 do
	begin
		Dif :=
			Abs(TRColor(VGAColors[i]).R - TRColor(C).R) +
			Abs(TRColor(VGAColors[i]).G - TRColor(C).G) +
			Abs(TRColor(VGAColors[i]).B - TRColor(C).B);
		if Dif <= BestDif then
		begin
			Result := VGAColors[i];
			BestDif := Dif;
		end;
	end;
end;

function BitColor(const C: TRColor; const Bits: Byte): TRColor;
begin
	Result.A := 0;
	case Bits of
	1: Result.L := NegMonoColor(NegMonoColor(C.L));
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
	else
		Result.L := C.L;
	end;
end;

function GetColor(const prompt: string;
	var CurrentColor: TColor; const DefaultColor: TColor; OnApply: TOnApplyColor): Boolean;

	procedure CreateBox(const i: Integer);
	begin
		fGColor.PanelColor[i].BevelOuter := bvNone;
		fGColor.PanelColor[i].BorderStyle := bsSingle;
		fGColor.PanelColor[i].Width := 16;
		fGColor.PanelColor[i].Height := 16;
		fGColor.PanelColor[i].Tag := i;
		fGColor.PanelColor[i].OnMouseDown := fGColor.PanelColorMouseDown;
		fGColor.PanelColor[i].OnMouseUp := fGColor.PanelColorMouseUp;
		fGColor.PanelColor[i].OnMouseMove := fGColor.PanelColorMouseMove;
	end;

var i: Integer;
begin
	if not Assigned(fGColor) then
	begin
		fGColor := TfGColor.Create(Application.MainForm);
		for i := 0 to MaxColor do
		begin
			fGColor.PanelColor[i] := TPanel.Create(fGColor);
			CreateBox(i);
			case i of
			0..23:
			begin
				fGColor.PanelColor[i].Left := fGColor.BevelBasicColors.Left + 8 + 20 * (i mod 12);
				fGColor.PanelColor[i].Top := fGColor.BevelBasicColors.Top + 8 + 20 * (i div 12);
			end;
			24..31:
			begin
				fGColor.PanelColor[i].Left := fGColor.BevelBasicColors.Left + 8 + 20 * (i - 24);
				fGColor.PanelColor[i].Top := fGColor.BevelBasicColors.Top + 64 - 8;
			end;
			32..35:
			begin
				fGColor.PanelColor[i].Left := fGColor.BevelBasicColors.Left + 8 + 20 * (i - 24);
				fGColor.PanelColor[i].Top := fGColor.BevelBasicColors.Top + 64 - 8;
			end;
			end;
			fGColor.PanelColor[i].Color := IntToColor(i).L;
			fGColor.InsertControl(fGColor.PanelColor[i]);
		end;
	end;
	fGColor.OnApply := OnApply;
	fGColor.ButtonApply.Enabled := Assigned(OnApply);

	fGColor.CurColor := CurrentColor;
	fGColor.DefColor := DefaultColor;
	fGColor.Caption := prompt;
	fGColor.SetNowColor(CurrentColor);

	fGColor.PanelCurColor.Color := fGColor.CurColor;
	InitButton(fGColor.PanelCurColor);

	fGColor.PanelDefaultColor.Color := fGColor.DefColor;
	InitButton(fGColor.PanelDefaultColor);

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

const ABits: array[0..4] of Byte = (1, 4, 15, 18, 24);

procedure TfGColor.InitReadOnly;
var
	C: TRColor;
	i: Integer;
	Vis: Boolean;
begin
	PanelNowColor.Color := NowRGB.L;
	InitButton(PanelNowColor);
	PanelNowColor.Repaint;

	PanelNowBitColor.Color := BitColor(NowRGB, ABits[ComboBoxBitDepth.ItemIndex]).L;
	InitButton(PanelNowBitColor);
	PanelNowBitColor.Repaint;
	ImageR.Fill;
	ImageG.Fill;
	ImageB.Fill;
	ImageH.Fill;
	ImageL.Fill;
	ImageS.Fill;

	Vis := False;
	for i := 0 to MaxColor do
	begin
		if C.L = IntToColor(I).L then
		begin
			Vis := True;
			ShapeBorder.Left := PanelColor[i].Left - 2;
			ShapeBorder.Top := PanelColor[i].Top - 2;
			Break;
		end;
	end;
	ShapeBorder.Visible := Vis;
	ShapeBorder.Update;

end;

procedure TfGColor.InitEditsRGB;
begin
	EditR.OnChange := nil;
	EditG.OnChange := nil;
	EditB.OnChange := nil;

	if ComboBoxNF.ItemIndex = 1 then
		NumericBase := 16;
	EditR.Text := NumericPref + NToS(NowRGB.R);
	EditG.Text := NumericPref + NToS(NowRGB.G);
	EditB.Text := NumericPref + NToS(NowRGB.B);
	NumericBase := 10;
	EditR.Repaint;
	EditG.Repaint;
	EditB.Repaint;

	EditR.OnChange := EditRGBAChange;
	EditG.OnChange := EditRGBAChange;
	EditB.OnChange := EditRGBAChange;
end;

procedure TfGColor.InitEditsHLS;
begin
	EditH.OnChange := nil;
	EditL.OnChange := nil;
	EditS.OnChange := nil;

	if ComboBoxNF.ItemIndex = 1 then
		NumericBase := 16;
	EditH.Text := NumericPref + NToS(NowHLS.H);
	EditL.Text := NumericPref + NToS(NowHLS.L);
	EditS.Text := NumericPref + NToS(NowHLS.S);
	NumericBase := 10;
	EditH.Repaint;
	EditL.Repaint;
	EditS.Repaint;

	EditH.OnChange := EditRGBAChange;
	EditL.OnChange := EditRGBAChange;
	EditS.OnChange := EditRGBAChange;
end;

procedure TfGColor.ChangeColor;
begin
	if Assigned(OnApply) then OnApply(FNowColor);
end;

procedure TfGColor.InitAll;
begin
	InitReadOnly;
	InitEditsRGB;
	InitEditsHLS;
	ChangeColor;
end;

procedure TfGColor.SetNowColor(Color: TColor);
begin
	FNowColor := Color;
	NowRGB.L := ColorToRGB(FNowColor) and $00ffffff;
	NowHLS := RGBToHLS(NowRGB);
end;

procedure TfGColor.SetNowRGB(Color: TRColor);
begin
	NowRGB := Color;
	FNowColor := NowRGB.L;
	NowHLS := RGBToHLS(NowRGB);
end;

procedure TfGColor.SetNowHLS(HLS: THLSColor);
begin
	NowHLS := HLS;
	NowRGB := HLSToRGB(HLS);
	FNowColor := NowRGB.L;
end;

{var
	MouseL: BG;}

procedure TfGColor.PanelColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	SetNowColor(IntToColor(TPanel(Sender).Tag).L);
	InitAll;
//	MouseL := True;
end;

procedure TfGColor.PanelColorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//	MouseL := False;
end;

procedure TfGColor.PanelColorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
{	if MouseL then
		PanelColorMouseDown(Sender, mbLeft, Shift, X, Y);}
end;

procedure TfGColor.FormDestroy(Sender: TObject);
var i: Integer;
begin
	for i := 0 to MaxColor do
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
		SetNowColor(TColor(LongWord(TMenuItem(Sender).Tag) or $80000000));
	InitAll;
end;

procedure TfGColor.PanelCurColorClick(Sender: TObject);
begin
	SetNowColor(CurColor);
	InitAll;
end;
{
procedure TfGColor.TrackBarRGBAChange(Sender: TObject);
begin
	NowColor := ColorToRGB(TColor(NowColor)) and $00ffffff;
	case TTrackBar(Sender).Tag of
	0: TRColor(NowColor).R := TrackBarR.Position;
	1: TRColor(NowColor).G := TrackBarG.Position;
	2: TRColor(NowColor).B := TrackBarB.Position;
	3:
	begin
		TRColor(NowColor).R := TrackBarA.Position;
		TRColor(NowColor).G := TrackBarA.Position;
		TRColor(NowColor).B := TrackBarA.Position;
	end;
	end;
	NowHLS := RGBToHLS(TRColor(NowColor));
//	NowHSV := RGBToHSV(TRColor(NowColor));
	InitReadOnly;
	ChangeLightC;
//	InitTrackBar;
	InitEdits;
	ChangeColor;
end;
}
procedure TfGColor.ButtonRGBAClick(Sender: TObject);
begin
	case TButton(Sender).Tag of
	0: NowRGB.R := 255 - NowRGB.R;
	1: NowRGB.G := 255 - NowRGB.G;
	2: NowRGB.B := 255 - NowRGB.B;
	end;
	SetNowRGB(NowRGB);

	InitAll;
end;

procedure TfGColor.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
var B: U1;
begin
	if TDImage(Sender).MouseL then
	begin
		X := Range(0, X, TDImage(Sender).Width - 1);
		B := Range(0, RoundDiv(255 * X, (TDImage(Sender).Bitmap.Width - 1)), 255);
		case TDImage(Sender).Tag of
		0:
		begin
			NowRGB.R := B;
			SetNowRGB(NowRGB);
		end;
		1:
		begin
			NowRGB.G := B;
			SetNowRGB(NowRGB);
		end;
		2:
		begin
			NowRGB.B := B;
			SetNowRGB(NowRGB);
		end;
		3:
		begin
			NowHLS.H := RoundDiv(MaxSpectrum * X, TDImage(Sender).Bitmap.Width - 1);
			SetNowHLS(NowHLS);
		end;
		4:
		begin
			NowHLS.L := B;
			SetNowHLS(NowHLS);
		end;
		5:
		begin
			NowHLS.S := B;
			SetNowHLS(NowHLS);
		end;
		end;
		InitAll;
	end;
end;

procedure TfGColor.EditRGBAChange(Sender: TObject);
begin
	case TEdit(Sender).Tag of
	0: NowRGB.R := StrToValU1(TEdit(Sender).Text, True, NowRGB.R);
	1: NowRGB.G := StrToValU1(TEdit(Sender).Text, True, NowRGB.G);
	2: NowRGB.B := StrToValU1(TEdit(Sender).Text, True, NowRGB.B);
	3: NowHLS.H := StrToValI(TEdit(Sender).Text, True, -1, NowHLS.H, MaxSpectrum, 1);
	4: NowHLS.L := StrToValU1(TEdit(Sender).Text, True, NowHLS.L);
	5: NowHLS.S := StrToValU1(TEdit(Sender).Text, True, NowHLS.S);
	end;
  ClearErrors;
	if TEdit(Sender).Tag <= 2 then
		SetNowRGB(NowRGB)
	else
		SetNowHLS(NowHLS);

	InitReadOnly;
	if TEdit(Sender).Tag <= 2 then
		InitEditsHLS
	else
		InitEditsRGB;
	ChangeColor;
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
		if FNowColor <> CurColor then OnApply(CurColor);
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

procedure TfGColor.AdvancedDraw(Sender: TObject; ACanvas: TCanvas;
	ARect: TRect; State: TOwnerDrawState);
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

	C := TColor(LongWord(TMenuItem(Sender).Tag) or $80000000);
	Bmp.Canvas.Brush.Color := NegColor(C);
	Bmp.Canvas.FrameRect(Rec);
	InflateRect(Rec, -1, -1);
	Bmp.Canvas.Brush.Color := C;
	Bmp.Canvas.FillRect(Rec);
	ImageList1.Clear;
	ImageList1.Add(Bmp, nil);
	Bmp.Free;
	MenuAdvancedDrawItem(Sender, ACanvas, ARect, State);
end;

procedure TfGColor.FormCreate(Sender: TObject);
var i: SG;
begin
	Background := baGradient;
	for i := 0 to Length(ABits) - 1 do
		ComboBoxBitDepth.Items.Add(NToS(ABits[i]) + ' bit');
	ComboBoxBitDepth.ItemIndex := Length(ABits) - 1;
end;

procedure TfGColor.ImageFill(Sender: TObject);
var
	BmpD: TDBitmap;
	i: Integer;
	B: U1;
	C: TRColor;
	HLS: THLSColor;
begin
	BmpD := TDImage(Sender).Bitmap;
	C := NowRGB;
	HLS := NowHLS;
	for i := 0 to BmpD.Width - 1 do
	begin
		B := RoundDiv(SG(255 * i), (BmpD.Width - 1));
		case TDImage(Sender).Tag of
		0:
		begin
			C.R := B;
		end;
		1:
		begin
			C.G := B;
		end;
		2:
		begin
			C.B := B;
		end;
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
		BmpD.Line(i, 0, i, 15, C.L, ef16);
	end;

	i := -1;
	case TDImage(Sender).Tag of
	0: i := RoundDiv(SG(255 * NowRGB.R), (BmpD.Width - 1));
	1: i := RoundDiv(SG(255 * NowRGB.G), (BmpD.Width - 1));
	2: i := RoundDiv(SG(255 * NowRGB.B), (BmpD.Width - 1));
	3: i := RoundDiv(SG((BmpD.Width - 1) * NowHLS.H), MaxSpectrum);
	4: i := RoundDiv(SG(255 * NowHLS.L), (BmpD.Width - 1));
	5: i := RoundDiv(SG(255 * NowHLS.S), (BmpD.Width - 1));
	end;
	BmpD.Line(i, 0, i, 15, NegMonoColor(FNowColor), ef16);
end;

procedure TfGColor.ComboBoxBitDepthChange(Sender: TObject);
begin
	InitAll;
end;

procedure TfGColor.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	ImageMouseMove(Sender, Shift, X, Y);
end;

procedure TfGColor.ComboBoxNFChange(Sender: TObject);
begin
	if ComboBoxNF.ItemIndex = 1 then
		NumericPref := '$'
	else
		NumericPref := '';
	InitEditsRGB;
	InitEditsHLS;
end;

end.


