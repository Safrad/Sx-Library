//* File:     Lib\uGColor.pas
//* Created:  1999-09-01
//* Modified: 2004-12-30
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

{
	Hue = Shade = Odstin
	Sat(iety) = Sytost
	Lum{inary, inous) = Svetelnost (0-239)
}

unit uGColor;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls, ExtCtrls, Menus, uGraph, uDButton,
	uDLabel, ImgList, uDForm, uDBitmap, uAdd, uDImage;

const
	MaxColor = 6 + 6 + 6 + 6 + 4 + 4 + 4 - 1;
type
	TOnApplyColor = procedure(Color: TColor);

	TfGColor = class(TDForm)
    LabelR: TDLabel;
		EditR: TEdit;
		TrackBarR: TTrackBar;
		ButtonR: TDButton;
		ButtonOk: TDButton;
		ButtonApply: TDButton;
		ButtonCancel: TDButton;
    LabelG: TDLabel;
		EditG: TEdit;
		TrackBarG: TTrackBar;
    ButtonG: TDButton;
    LabelB: TDLabel;
		EditB: TEdit;
		TrackBarB: TTrackBar;
    ButtonB: TDButton;
    LabelA: TDLabel;
		EditA: TEdit;
		TrackBarA: TTrackBar;
    ButtonA: TDButton;
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
		PanelS: TPanel;
		PanelL: TPanel;
    PanelNowColor: TDButton;
    PanelCurColor: TDButton;
		Bevel1: TBevel;
		ImageS: TDImage;
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
    DLabel1: TDLabel;
    EditHue: TEdit;
    TrackBarHue: TTrackBar;
    DButtonH: TDButton;
    DLabel2: TDLabel;
    DLabel3: TDLabel;
		procedure FormDestroy(Sender: TObject);
		procedure ColorClick(Sender: TObject);
		procedure PanelCurColorClick(Sender: TObject);
		procedure TrackBarRGBAChange(Sender: TObject);
		procedure ButtonRGBAClick(Sender: TObject);
		procedure ImageSMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ImageSMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ImageSMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure EditRGBAChange(Sender: TObject);
		procedure ImageLMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ImageLMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure ImageLMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure PanelDefaultColorClick(Sender: TObject);
		procedure PanelNowBitColorClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure AdvancedDraw(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
			State: TOwnerDrawState);
		procedure FormCreate(Sender: TObject);
		procedure ImageSFill(Sender: TObject);
		procedure ImageLFill(Sender: TObject);
		procedure ComboBoxBitDepthChange(Sender: TObject);
	private
		{ Private declarations }
		CurColor, DefColor: TColor;
		OnApply: TOnApplyColor;
		SpectrumDown: Boolean;
		LightDown: Boolean;
		SpectrumPos, LightPos: Integer;
		SpectrumC, LightC: TRColor;

		NowColor: TColor;
		PanelColor: array[0..MaxColor] of TPanel;

		procedure InitReadOnly;
		procedure ChangeLightC;
		procedure InitTrackBar;
		procedure InitEdits;
		procedure ChangeColor;

		procedure InitAll;

		procedure PanelColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
	uMenus, uInput;

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

function ColorToSpectrum(var SpectrumPos: Integer; const C: TColor): Boolean;
begin
	Result := False;
{
var
	RC: TRColor;
	CA: array[0..2] of Byte;
	i: Integer;
begin
	RC.L := ColorToRGB(C);
	CA[0] := RC.R;
	CA[1] := RC.G;
	CA[2] := RC.B;
	Result := False
	for i := 0 to 2 do
	begin
		if (CA[i] < CA[(i + 1) mod 3]) and (CA[i] < CA[(i + 2) mod 3]) then
		begin
			Result := True;
			Dec(CA[(i + 1) mod 3], CA[i]);
			Dec(CA[(i + 2) mod 3], CA[i]);
			CA[i] := 0;
			SpectrumPos :=
			Break;
		end;
	end;}
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

function BitColor(const C: TColor; const Bits: Byte): TRColor;
var
	RC: TRColor;
begin
	RC.L := ColorToRGB(C) and $00ffffff;
	Result.A := 0;
	case Bits of
	1: Result.L := NegMonoColor(NegMonoColor(RC.L));
	4:
	begin
		Result.L := GetVGAPalete(RC.L);
	end;
	15:
	begin
		Result.R := RC.R shr 3;
		Result.R := 255 * Result.R div 31;
		Result.G := RC.G shr 3;
		Result.G := 255 * Result.G div 31;
		Result.B := RC.B shr 3;
		Result.B := 255 * Result.B div 31;
	end;
	18:
	begin
		Result.R := RC.R shr 2;
		Result.R := 255 * Result.R div 63;
		Result.G := RC.G shr 2;
		Result.G := 255 * Result.G div 63;
		Result.B := RC.B shr 2;
		Result.B := 255 * Result.B div 63;
	end;
	else
		Result.L := RC.L;
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
	fGColor.NowColor := CurrentColor;
	fGColor.DefColor := DefaultColor;
	fGColor.Caption := prompt;

	fGColor.PanelCurColor.Color := fGColor.CurColor;
	InitButton(fGColor.PanelCurColor);

	fGColor.PanelDefaultColor.Color := fGColor.DefColor;
	InitButton(fGColor.PanelDefaultColor);

	fGColor.InitReadOnly;
	fGColor.ChangeLightC;
	fGColor.InitTrackBar;
	fGColor.InitEdits;

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
			CurrentColor := fGColor.NowColor;
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
	C.L := ColorToRGB(NowColor) and $00ffffff;
	PanelNowColor.Color := C.L;
	InitButton(PanelNowColor);
	PanelNowColor.Repaint;

	PanelNowBitColor.Color := BitColor(NowColor, ABits[ComboBoxBitDepth.ItemIndex]).L;
	InitButton(PanelNowBitColor);
	PanelNowBitColor.Repaint;
	ImageS.Fill;
	ImageL.Fill;

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

procedure TfGColor.ChangeLightC;
begin
	LightC.L := AbsoluteColor(NowColor);
end;

procedure TfGColor.InitTrackBar;
var C: TRColor;
begin
	C.L := ColorToRGB(TColor(NowColor)) and $00ffffff;
	TrackBarR.OnChange := nil;
	TrackBarG.OnChange := nil;
	TrackBarB.OnChange := nil;
	TrackBarA.OnChange := nil;

	TrackBarR.Position := C.R;
	TrackBarG.Position := C.G;
	TrackBarB.Position := C.B;
	TrackBarA.Position := (C.R + C.G + C.B) div 3;

	TrackBarR.OnChange := TrackBarRGBAChange;
	TrackBarG.OnChange := TrackBarRGBAChange;
	TrackBarB.OnChange := TrackBarRGBAChange;
	TrackBarA.OnChange := TrackBarRGBAChange;

	EditR.OnChange := nil;
	EditG.OnChange := nil;
	EditB.OnChange := nil;
	EditA.OnChange := nil;
end;

procedure TfGColor.InitEdits;
var C: TRColor;
begin
	C.L := ColorToRGB(TColor(NowColor)) and $00ffffff;

	EditR.OnChange := nil;
	EditG.OnChange := nil;
	EditB.OnChange := nil;
	EditA.OnChange := nil;

	EditR.Text := NToS(C.R, '000');
	EditR.Repaint;
	EditG.Text := NToS(C.G, '000');
	EditG.Repaint;
	EditB.Text := NToS(C.B, '000');
	EditB.Repaint;
	EditA.Text := NToS((C.R + C.G + C.B) div 3, '000');
	EditA.Repaint;

	EditR.OnChange := EditRGBAChange;
	EditG.OnChange := EditRGBAChange;
	EditB.OnChange := EditRGBAChange;
	EditA.OnChange := EditRGBAChange;
end;

procedure TfGColor.ChangeColor;
begin
	if Assigned(OnApply) then OnApply(NowColor);
end;

procedure TfGColor.InitAll;
begin
	InitReadOnly;
	ChangeLightC;
	InitTrackBar;
	InitEdits;
	ChangeColor;
end;

procedure TfGColor.PanelColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	NowColor := IntToColor(TPanel(Sender).Tag).L;
	InitAll;
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
		NowColor := clNone
	else
		NowColor := TColor(LongWord(TMenuItem(Sender).Tag) or $80000000);
	InitAll;
end;

procedure TfGColor.PanelCurColorClick(Sender: TObject);
begin
	NowColor := CurColor;
	InitAll;
end;

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
	ColorToSpectrum(SpectrumPos, NowColor);
	InitReadOnly;
	ChangeLightC;
//	InitTrackBar;
	InitEdits;
	ChangeColor;
end;

procedure TfGColor.ButtonRGBAClick(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	case TButton(Sender).Tag of
	0: TRColor(NowColor).R := 255 - TRColor(NowColor).R;
	1: TRColor(NowColor).G := 255 - TRColor(NowColor).G;
	2: TRColor(NowColor).B := 255 - TRColor(NowColor).B;
	3:
	begin
		TRColor(NowColor).R := 255 - TRColor(NowColor).R;
		TRColor(NowColor).G := 255 - TRColor(NowColor).G;
		TRColor(NowColor).B := 255 - TRColor(NowColor).B;
	end;
	end;
	InitAll;
end;

procedure TfGColor.ImageSMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
	begin
		SpectrumDown := True;
		ImageSMouseMove(Sender, Shift, X, Y);
	end;
end;

procedure TfGColor.ImageSMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then SpectrumDown := False;
end;

procedure TfGColor.ImageSMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
begin
	if SpectrumDown then
	begin
		X := SpectrumPixel * X;
		if X < 0 then
			SpectrumPos := 0
		else if X > MaxSpectrum then
			SpectrumPos := MaxSpectrum
		else
		begin
			SpectrumPos := X;
		end;
		NowColor := SpectrumColor(SpectrumPos);
		InitAll;
	end;
end;

procedure TfGColor.EditRGBAChange(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	case TEdit(Sender).Tag of
	0: TRColor(NowColor).R := StrToValU1(TEdit(Sender).Text, True, TRColor(NowColor).R);
	1: TRColor(NowColor).G := StrToValU1(TEdit(Sender).Text, True, TRColor(NowColor).G);
	2: TRColor(NowColor).B := StrToValU1(TEdit(Sender).Text, True, TRColor(NowColor).B);
	3:
	begin
		TRColor(NowColor).R := StrToValU1(EditA.Text, True, TRColor(NowColor).R);
		TRColor(NowColor).G := StrToValU1(EditA.Text, True, TRColor(NowColor).G);
		TRColor(NowColor).B := StrToValU1(EditA.Text, True, TRColor(NowColor).B);
		TRColor(NowColor).A := 0;
	end;
	end;
	InitReadOnly;
	ChangeLightC;
	InitTrackBar;
//	InitEdits;
	ChangeColor;
end;

procedure TfGColor.ImageLMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
	begin
		LightDown := True;
		ImageLMouseMove(Sender, Shift, X, Y);
	end;
end;

procedure TfGColor.ImageLMouseMove(Sender: TObject; Shift: TShiftState; X,
	Y: Integer);
var
	i: Integer;
begin
	if LightDown then
	begin
		i := 765 * {(LightC.R + LightC.G + LightC.B) *} X div (ImageL.Width - 1);
		if i < 0 then
			i := 0
		else if i > 765 then
			i := 765;
		LightPos := X;

		TRColor(NowColor).R := LightC.R * i div 765;
		TRColor(NowColor).G := LightC.G * i div 765;
		TRColor(NowColor).B := LightC.B * i div 765;
		TRColor(NowColor).A := 0;

		//ChangeColor;
		InitAll;
	end;
end;

procedure TfGColor.ImageLMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then LightDown := False;
end;

procedure TfGColor.PanelDefaultColorClick(Sender: TObject);
begin
	NowColor := DefColor;
	InitAll;
end;

procedure TfGColor.PanelNowBitColorClick(Sender: TObject);
begin
	NowColor := PanelNowBitColor.Color;
	InitAll;
end;

procedure TfGColor.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		if NowColor <> CurColor then OnApply(CurColor);
		Close;
	end;
end;

procedure TfGColor.ButtonOkClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		OnApply(NowColor);
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

procedure TfGColor.ImageSFill(Sender: TObject);
var
	BmpD: TDBitmap;
	i: Integer;
begin
	BmpD := ImageS.Bitmap;
	for i := 0 to ImageS.Width - 1 do
	begin
		BmpD.Line(i, 0, i, 15, SpectrumColor(SpectrumPixel * i), ef16);
	end;
	SpectrumC.L := SpectrumColor(SpectrumPos);
	if SpectrumPos >= 0 then
		BmpD.Line(SpectrumPos div SpectrumPixel, 0, SpectrumPos div SpectrumPixel, 15, clNone, efXor);
end;

procedure TfGColor.ImageLFill(Sender: TObject);
var
	BmpD: TDBitmap;
	i, X: Integer;
	C: TRColor;
begin
	BmpD := ImageL.Bitmap;
	for i := 0 to ImageL.Width - 1 do
	begin
		C.R := LightC.R * SG(i) div (BmpD.Width - 1);
		C.G := LightC.G * SG(i) div (BmpD.Width - 1);
		C.B := LightC.B * SG(i) div (BmpD.Width - 1);
		C.A := 0;
		BmpD.Line(i, 0, i, 15, C.L, ef16);
	end;
	C.L := ColorToRGB(NowColor)  and $00ffffff;
	LightPos := C.R + C.G + C.B;
	if (LightC.R + LightC.G + LightC.B) > 0 then
		X := LightPos * Integer(BmpD.Width - 1) div (LightC.R + LightC.G + LightC.B)
	else
		X := 0;
	if LightPos >= 0 then
		BmpD.Line(X, 0, X, 15, clNone, efNeg);
end;

procedure TfGColor.ComboBoxBitDepthChange(Sender: TObject);
begin
	InitAll;
end;


{
Nìco k pøevodu RGB -> YUV, RGB -> YCbCr
Oba pøevody (RGB -> YUV i RGB -> YCbCr) jsou jednoduše vyjádøitelné maticemi:
|Y|   |0.299  0.587  0.114  | |R|
|U| = |-0.141  -0.289 0.437 | |G|
|V|   |0.615 -0.515 -0.1    | |B|


|Y |   |0.299  0.587  0.114   | |R|
|Cb| = |-0.1687  -0.3313 -0.5 | |G|
|Cr|   |0.5 -0.4187 -0.0813   | |B|

Zpìtný pøevod se provádí pomocí inverzní matice.


Model HSV vykazuje nìkteré nedostatky, které sice nejsou zásadního charakteru,
nicménì mohou ztìžovat práci s definováním barvy v prostoru HSV.
Jedním z nedostatkù je jehlanovitý tvar, který zpùsobuje,
že ve øezu se musí bod o konstantní hodnotì s pohybovat pøi zmìnì h po dráze ve tvaru šestiúhelníku,
nikoliv po kružnici, jak by bylo pøirozené.
Dalším záporným jevem je nesymetrie modelu z hlediska pøechodù ve stupních šedi od èerné k bílé.
Tyto nedostatky odstraòuje model HLS zavedený firmou Tektronix
}

function RGBToHLS(C: TRColor): THLSColor;
var
	MaxC, MinC, delta, H: SG;
begin
	Result.H := 0;
	Result.L := 0;
	Result.S := 0;

	MaxC := max(max(C.R, C.G), C.B);
	MinC := min(min(C.R, C.G), C.B);

	Result.L := (maxC + minC) div 2;

	if (maxC = minC) then
	begin
		Result.S := 0;
		Result.H := -1;
	end
	else
	begin
		if (Result.L < 128) then
			Result.S := (maxC-minC) div (maxC+minC)
		else
			Result.S := (maxC-minC) div (2-maxC-minC);

		delta := maxC - minC;
		H := 0;
		if (C.R = maxC) then H := 60*(C.G-C.B) div delta
		else if (C.G = maxC) then H := 60*(2+(C.B-C.R)) div delta
		else if (C.B = maxC) then H := 60*(4+(C.R-C.G)) div delta;
		if (H < 0) then Inc(H, 360);
		Result.H := H;
	end;
end;

function RGBtoHSV(C: TRColor): THSVColor;
var
	MaxC, MinC, delta, H: SG;
begin
	maxC := Math.max(Math.max(C.r,C.g),C.b);
	minC := Math.min(Math.min(C.r,C.g),C.b);

	Result.v := maxC;
	Result.h := 0;
	h := 0;

	if (maxC <> 0) then
		Result.s := (maxC - minC) div maxC
	else
		Result.s := 0;

	if(Result.s = 0) then
		Result.h := -1
	else
	begin
		delta := maxC - minC;
		if(C.r = maxC) then h := 60*(C.g-C.b) div delta
		else if(C.g = maxC) then h := 60*(2+(C.b-C.r)) div delta
		else if(C.b = maxC) then h := 60*(4+(C.r-C.g)) div delta;
		if(h<0) then Inc(h, 360);
		Result.H := H;
	end;
end;

function HLSRGBValue(n1, n2, hue: SG): U1;
begin
	if(hue>360) then Dec(hue, 360)
	else if(hue<0) then Inc(hue, 360);
	if(hue<60) then Result := n1+(n2-n1)*hue div 60
	else if (hue<180) then Result := n2
	else if (hue<240) then Result := n1+(n2-n1)*(240-hue) div 60
	else Result := n1;
end;

function HSVtoRGB(C: THSVColor): TRColor;
var i, f, p, q, t: SG;
begin
	Result.L := 0;
	if(C.s = 0) then
	begin
		if(C.h = -1) then
		begin
			Result.r := C.v;
			Result.g := C.v;
			Result.b := C.v;
		end
		else
		begin
{							rIndex.setText("xxx");
			gIndex.setText("xxx");
			bIndex.setText("xxx"); D???}
		end;
	end
	else
	begin
		if(C.h = 360) then C.h := 0;

		C.h:=C.h div 60; // D???
//            i := (int)Math.floor((double)h); D???
  
		f := C.h - i;
		p := C.v*(1-C.s);
		q := C.v*(1-(C.s*f));
		t := C.v*(1-(C.s*(1-f)));

		case i of
		0:
		begin
			Result.r := C.v;
			Result.g := t;
			Result.b := p;
		end;
		1:
		begin
			Result.r := q;
			Result.g := C.v;
			Result.b := p;
		end;
		2:
		begin
			Result.r := p;
			Result.g := C.v;
			Result.b := t;
		end;
		3:
		begin
			Result.r := p;
			Result.g := q;
			Result.b := C.v;
		end;
		4:
		begin
			Result.r := t;
			Result.g := p;
			Result.b := C.v;
		end;
		5:
		begin
			Result.r := C.v;
			Result.g := p;
			Result.b := q;
		end;
		end;
	end;
end;

function HLStoRGB(C: THLSColor): TRColor;
var m2, m1: SG;
begin
	Result.R := 0;
	Result.G := 0;
	Result.B := 0;

	if(C.l<128) then m2 := C.l*(1+C.s)
	else m2 := C.l+C.s-C.l*C.s;
	m1 := 2*C.l-m2;
	if(C.s = 0) then
	begin
		{if(C.h = Float.NaN)}
		Result.r := 255;
		Result.g := 255;
		Result.b := 255;
		//else System.err.println("Doslo k chybe pri prevodu HLS -> RGB");
	end
	else
	begin
		Result.r := HLSRGBValue(m1,m2,C.h+120);
		Result.g := HLSRGBValue(m1,m2,C.h);
		Result.b := HLSRGBValue(m1,m2,C.h-120);
	end;
end;
  

end.


