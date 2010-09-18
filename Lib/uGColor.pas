//* File:     Lib\uGColor.pas
//* Created:  1999-09-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
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
		RadioGroup1: TRadioGroup;
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
		GroupBoxColors: TGroupBox;
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
    PanelNowColor: TDLabel;
    PanelCurColor: TDLabel;
		Bevel1: TBevel;
		ImageS: TDImage;
		ImageL: TDImage;
		ShapeBorder: TShape;
    PanelNowBitColor: TDLabel;
    PanelDefaultColor: TDLabel;
		LabelNow: TDLabel;
		LabelNowXBit: TDLabel;
		LabelDefault: TDLabel;
		LabelCurrent: TDLabel;
		Bevel2: TBevel;
		ImageList1: TImageList;
		procedure FormDestroy(Sender: TObject);
		procedure ColorClick(Sender: TObject);
		procedure PanelCurColorClick(Sender: TObject);
		procedure TrackBarRChange(Sender: TObject);
		procedure TrackBarGChange(Sender: TObject);
		procedure TrackBarBChange(Sender: TObject);
		procedure ButtonRClick(Sender: TObject);
		procedure ButtonGClick(Sender: TObject);
		procedure ButtonBClick(Sender: TObject);
		procedure ButtonAClick(Sender: TObject);
		procedure TrackBarAChange(Sender: TObject);
		procedure ImageSMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ImageSMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ImageSMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure EditRChange(Sender: TObject);
		procedure EditGChange(Sender: TObject);
		procedure EditBChange(Sender: TObject);
		procedure EditAChange(Sender: TObject);
		procedure RadioGroup1Click(Sender: TObject);
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
	private
		{ Private declarations }
		CurColor, DefColor: TColor;
		SchemeChanged: Boolean;
		OnApply: TOnApplyColor;
		SpectrumDown: Boolean;
		LightDown: Boolean;
		SpectrumPos, LightPos: Integer;
		SpectrumC, LightC: TRColor;

		NowColor: TColor;
		PanelColor: array[0..MaxColor] of TPanel;
		procedure ChangeColor;
		procedure PanelColorClick(Sender: TObject);
		procedure ChangeLightC;
		procedure InitEdits;
		procedure InitBorder;
	public
		{ Public declarations }
	end;

function GetColor(const prompt: string;
	var CurrentColor: TColor; const DefaultColor: TColor; OnApply: TOnApplyColor): Boolean;

implementation

{$R *.DFM}
uses uMenus, uInput;

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
			C.T := 0;
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

function GetColor(const prompt: string;
	var CurrentColor: TColor; const DefaultColor: TColor; OnApply: TOnApplyColor): Boolean;

	procedure CreateBox(const i: Integer);
	begin
		fGColor.PanelColor[i].BevelOuter := bvNone;
		fGColor.PanelColor[i].BorderStyle := bsSingle;
		fGColor.PanelColor[i].Width := 16;
		fGColor.PanelColor[i].Height := 16;
		fGColor.PanelColor[i].Tag := i;
		fGColor.PanelColor[i].OnClick := fGColor.PanelColorClick;
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
				fGColor.PanelColor[i].Left := 16 + 20 * (i mod 12);
				fGColor.PanelColor[i].Top := 16 + 20 * (i div 12);
			end;
			24..31:
			begin
				fGColor.PanelColor[i].Left := 16 + 20 * (i - 24);
				fGColor.PanelColor[i].Top := 64;
			end;
			32..35:
			begin
				fGColor.PanelColor[i].Left := 16 + 20 * (i - 24);
				fGColor.PanelColor[i].Top := 64;
			end;
			end;
			fGColor.PanelColor[i].Color := IntToColor(i).L;
			fGColor.GroupBoxColors.InsertControl(fGColor.PanelColor[i]);
		end;
	end;
	fGColor.OnApply := OnApply;
	fGColor.ButtonApply.Enabled := Assigned(OnApply);

	fGColor.CurColor := CurrentColor;
	fGColor.NowColor := CurrentColor;
	fGColor.DefColor := DefaultColor;
	fGColor.Caption := prompt;

	fGColor.PanelCurColor.Color := fGColor.CurColor;
	fGColor.PanelCurColor.Font.Color := NegMonoColor(fGColor.PanelCurColor.Color);
	fGColor.PanelCurColor.Caption := ColorToString(fGColor.PanelCurColor.Color);

	fGColor.PanelDefaultColor.Color := fGColor.DefColor;
	fGColor.PanelDefaultColor.Font.Color := NegMonoColor(fGColor.PanelDefaultColor.Color);
	fGColor.PanelDefaultColor.Caption := ColorToString(fGColor.PanelDefaultColor.Color);

	fGColor.ChangeLightC;
	fGColor.ChangeColor;
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

procedure TfGColor.PanelColorClick(Sender: TObject);
begin
	NowColor := IntToColor(TPanel(Sender).Tag).L;
	ChangeLightC;
	ChangeColor;
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

procedure TfGColor.FormDestroy(Sender: TObject);
var i: Integer;
begin
	for i := 0 to MaxColor do
	begin
		if PanelColor[i] <> nil then
		begin
			GroupBoxColors.RemoveControl(PanelColor[i]);
			PanelColor[i].Free; PanelColor[i] := nil;
		end;
	end;
end;

procedure TfGColor.InitEdits;
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

	TrackBarR.OnChange := TrackBarRChange;
	TrackBarG.OnChange := TrackBarGChange;
	TrackBarB.OnChange := TrackBarBChange;
	TrackBarA.OnChange := TrackBarAChange;

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

	EditR.OnChange := EditRChange;
	EditG.OnChange := EditGChange;
	EditB.OnChange := EditBChange;
	EditA.OnChange := EditAChange;
end;

procedure TfGColor.InitBorder;
var
	C: TRColor;
	i: Integer;
	Vis: Boolean;
begin
	C.L := ColorToRGB(NowColor) and $00ffffff;
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
	Result.T := 0;
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

procedure TfGColor.ChangeLightC;
begin
	LightC.L := AbsoluteColor(NowColor);
end;

procedure TfGColor.ChangeColor;
const ABits: array[0..4] of Byte = (1, 4, 15, 18, 24);
begin
	SchemeChanged := True;
	PanelNowColor.Color := ColorToRGB(TColor(NowColor)) and $00ffffff;
	PanelNowColor.Font.Color := NegMonoColor(PanelNowColor.Color);
	PanelNowColor.Caption := ColorToString(TColor(NowColor));
	PanelNowColor.Repaint;

	PanelNowBitColor.Color := BitColor(NowColor, ABits[RadioGroup1.ItemIndex]).L;
	PanelNowBitColor.Font.Color := NegMonoColor(PanelNowBitColor.Color);
	PanelNowBitColor.Caption := ColorToString(PanelNowBitColor.Color);
	PanelNowBitColor.Repaint;
	InitEdits;
	ImageS.Fill;
	ImageL.Fill;
	InitBorder;
	if Assigned(OnApply) then OnApply(NowColor);
end;

procedure TfGColor.ColorClick(Sender: TObject);
begin
	if TMenuItem(Sender).Tag < 0 then
		NowColor := clNone
	else
		NowColor := TColor(LongWord(TMenuItem(Sender).Tag) or $80000000);
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.PanelCurColorClick(Sender: TObject);
begin
	NowColor := CurColor;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.TrackBarRChange(Sender: TObject);
begin
	NowColor := ColorToRGB(TColor(NowColor)) and $00ffffff;
	TRColor(NowColor).R := TrackBarR.Position;
	ColorToSpectrum(SpectrumPos, NowColor);
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.TrackBarGChange(Sender: TObject);
begin
	NowColor := ColorToRGB(TColor(NowColor)) and $00ffffff;
	TRColor(NowColor).G := TrackBarG.Position;
	ColorToSpectrum(SpectrumPos, NowColor);
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.TrackBarBChange(Sender: TObject);
begin
	NowColor := ColorToRGB(TColor(NowColor)) and $00ffffff;
	TRColor(NowColor).B := TrackBarB.Position;
	ColorToSpectrum(SpectrumPos, NowColor);
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.TrackBarAChange(Sender: TObject);
begin
	NowColor := ColorToRGB(TColor(NowColor)) and $00ffffff;
	TRColor(NowColor).R := TrackBarA.Position;
	TRColor(NowColor).G := TrackBarA.Position;
	TRColor(NowColor).B := TrackBarA.Position;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.ButtonRClick(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).R := 255 - TRColor(NowColor).R;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.ButtonGClick(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).G := 255 - TRColor(NowColor).G;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.ButtonBClick(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).B := 255 - TRColor(NowColor).B;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.ButtonAClick(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).R := 255 - TRColor(NowColor).R;
	TRColor(NowColor).G := 255 - TRColor(NowColor).G;
	TRColor(NowColor).B := 255 - TRColor(NowColor).B;
	ChangeLightC;
	ChangeColor;
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
//      if X>0 then Dec(X);
			SpectrumPos := X;
		end;
		NowColor := SpectrumColor(SpectrumPos);
		ChangeLightC;
		ChangeColor;
	end;
end;

procedure TfGColor.EditRChange(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).R := StrToValU1(EditR.Text, True, TRColor(NowColor).R);
	InitEdits;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.EditGChange(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).G := StrToValU1(EditR.Text, True, TRColor(NowColor).G);
	InitEdits;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.EditBChange(Sender: TObject);
begin
	NowColor := ColorToRGB(NowColor) and $00ffffff;
	TRColor(NowColor).B := StrToValU1(EditB.Text, True, TRColor(NowColor).B);
	InitEdits;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.EditAChange(Sender: TObject);
begin
	TRColor(NowColor).T := 0;
	TRColor(NowColor).R := StrToValU1(EditA.Text, True, TRColor(NowColor).R);
	TRColor(NowColor).G := StrToValU1(EditA.Text, True, TRColor(NowColor).G);
	TRColor(NowColor).B := StrToValU1(EditA.Text, True, TRColor(NowColor).B);
	InitEdits;
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.RadioGroup1Click(Sender: TObject);
begin
	ChangeLightC;
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
		TRColor(NowColor).T := 0;

		ChangeColor;
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
	ChangeLightC;
	ChangeColor;
end;

procedure TfGColor.PanelNowBitColorClick(Sender: TObject);
begin
	NowColor := PanelNowBitColor.Color;
	ChangeLightC;
	ChangeColor;
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
begin
	Background := baGradient;
end;

procedure TfGColor.ImageSFill(Sender: TObject);
var
	BmpD: TDBitmap;
	i: Integer;
begin
	BmpD := ImageS.Bitmap;
	for i := 0 to ImageS.Width - 1 do
	begin
		BmpD.Lin24(i, 0, i, 15, SpectrumColor(SpectrumPixel * i), ef16);
	end;
	SpectrumC.L := SpectrumColor(SpectrumPos);
	if SpectrumPos >= 0 then
		BmpD.Lin24(SpectrumPos div SpectrumPixel, 0, SpectrumPos div SpectrumPixel, 15, clNone, efXor);
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
		C.T := 0;
		BmpD.Lin24(i, 0, i, 15, C.L, ef16);
	end;
	C.L := ColorToRGB(NowColor)  and $00ffffff;
	LightPos := C.R + C.G + C.B;
	if (LightC.R + LightC.G + LightC.B) > 0 then
		X := LightPos * Integer(BmpD.Width - 1) div (LightC.R + LightC.G + LightC.B)
	else
		X := 0;
	if LightPos >= 0 then
		BmpD.Lin24(X, 0, X, 15, clNone, efNeg);
end;

end.
