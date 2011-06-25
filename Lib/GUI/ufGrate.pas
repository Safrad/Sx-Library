//* File:     Lib\GUI\ufGrate.pas
//* Created:  1999-08-01
//* Modified: 2007-05-20
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit ufGrate;

interface

uses
	uTypes,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, Menus, uDImage, uDForm;

type
	TfGrate = class(TDForm)
		PopupMenu1: TPopupMenu;
		Close1: TMenuItem;
		Color1: TMenuItem;
		Size1: TMenuItem;
		BackgroundColor1: TMenuItem;
		Centered1: TMenuItem;
		N1: TMenuItem;
		N2: TMenuItem;
		ShowGrate1: TMenuItem;
		procedure FormHide(Sender: TObject);
		procedure Close1Click(Sender: TObject);
		procedure Color1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure Size1Click(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure Centered1Click(Sender: TObject);
		procedure BackgroundColor1Click(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ShowGrate1Click(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormPaint(Sender: TObject);
	private
		{ Private declarations }
		procedure RWOptions(const Save: Boolean);
	public
		{ Public declarations }
		GrateColor, BackgroundColor: TColor;
		GrateSize: SG;
	end;

var
	fGrate: TfGrate;

implementation

{$R *.DFM}
uses
	uScreen, uGraph, uDBitmap, uGColor, uGetInt, uDIniFile, uMenus, uColor,
	Math;

const
	DefaultGrateSize = 8;

procedure TfGrate.RWOptions(const Save: Boolean);
const Section = 'Grate';
begin
	GrateColor := MainIni.RWSGF(Section, 'Grate Color', GrateColor, clWhite, Save);
	BackgroundColor := MainIni.RWSGF(Section, 'Background Color', BackgroundColor, clBlack, Save);
	GrateSize := MainIni.RWSGF(Section, 'Size', GrateSize, DefaultGrateSize, Save);
	Centered1.Checked := MainIni.RWBGF(Section, 'Centered', Centered1.Checked, Centered1.Checked, Save);
end;

procedure TfGrate.FormHide(Sender: TObject);
begin
	ShowTaskBar(True);
end;

procedure TfGrate.Close1Click(Sender: TObject);
begin
	Close;
end;

procedure OnApply(Color: TColor);
begin
	fGrate.GrateColor := Color;
	fGrate.Invalidate;
end;

procedure TfGrate.Color1Click(Sender: TObject);
begin
	GetColor('Grate Color', GrateColor, clWhite, OnApply);
end;

procedure OnApplyB(Color: TColor);
begin
	fGrate.BackgroundColor := Color;
	fGrate.Invalidate;
end;

procedure TfGrate.BackgroundColor1Click(Sender: TObject);
begin
	GetColor('Background Color', BackgroundColor, clBlack, OnApplyB);
end;

procedure OnApplyGrateSize(Value: S8);
begin
	fGrate.GrateSize := Value;
	fGrate.Invalidate;
end;

procedure TfGrate.Size1Click(Sender: TObject);
begin
	if GetNumber('Grate Size', GrateSize, 0, DefaultGrateSize, Max(NowScreenMode.Width div 2, NowScreenMode.Height div 2), OnApplyGrateSize) then
end;

procedure TfGrate.FormCreate(Sender: TObject);
begin
	Background := baUser;
	FullScreen := True;
	MenuSet(PopupMenu1);

	RWOptions(False);
end;

procedure TfGrate.FormDestroy(Sender: TObject);
begin
	RWOptions(True);
end;

procedure TfGrate.Centered1Click(Sender: TObject);
begin
	Centered1.Checked := not Centered1.Checked;
	Invalidate;
end;

procedure TfGrate.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (Key = VK_F10) and (ssShift in Shift) then PopupMenu1.Popup(0, 0);
	if Key = VK_ESCAPE then Close;
end;

procedure TfGrate.ShowGrate1Click(Sender: TObject);
begin
	ShowGrate1.Checked := not ShowGrate1.Checked;
	Invalidate;
end;

procedure TfGrate.FormShow(Sender: TObject);
begin
	Invalidate;
end;

procedure TfGrate.FormPaint(Sender: TObject);
var
	x, y, m: Integer;
	Bmp: TDBitmap;
begin
	Bmp := BackBitmap;
	Bmp.Bar(BackgroundColor, ef16);

	if ShowGrate1.Checked = False then
	begin
		x := 0;
		m := Bmp.Width;
		while x < m do
		begin
			Bmp.Line(x, 0, x, Bmp.Height div 2 - 1, GrateColor, ef16);
			Inc(x, 2);
		end;

		y := Bmp.Height div 2;
		m := Bmp.Height;
		while y < m do
		begin
			Bmp.Line(0, y, Bmp.Width div 2 - 1, y, GrateColor, ef16);
			Inc(y, 2);
		end;

		y := Bmp.Height div 2;
		while y < Bmp.Height do
		begin
			m := Bmp.Width;
			x := Bmp.Width div 2;
			while x < m do
			begin
				if (x and 1) = (y and 1) then
					Pix(Bmp.Data, Bmp.ByteX, x, y, PRGBA(@GrateColor), ef16);
				Inc(x);
			end;
			Inc(y);
		end;

		Bmp.Bar(Bmp.Width div 2, 0, Bmp.Width - 1, Bmp.Height div 2 - 1, MixColors(GrateColor, BackgroundColor), ef16);
	end
	else if GrateSize > 0 then
	begin
		if Centered1.Checked then
			m := Bmp.Height div 2
		else
			m := Bmp.Height;
		y := 0;
		while y < m do
		begin
			Bmp.Line(0, y, Bmp.Width - 1, y, GrateColor, ef16);
			Inc(y, GrateSize);
		end;
		if Centered1.Checked then
		begin
			y := Bmp.Height - 1;
			while y >= m do
			begin
				Bmp.Line(0, y, Bmp.Width - 1, y, GrateColor, ef16);
				Dec(y, GrateSize);
			end;
		end;
		if Centered1.Checked then
			m := Bmp.Width div 2
		else
			m := Bmp.Width;
		x := 0;
		while x < m do
		begin
			Bmp.Line(x, 0, x, Bmp.Height - 1, GrateColor, ef16);
			Inc(x, GrateSize);
		end;
		if Centered1.Checked then
		begin
			x := Bmp.Width - 1;
			while x >= m do
			begin
				Bmp.Line(x, 0, x, Bmp.Height - 1, GrateColor, ef16);
				Dec(x, GrateSize);
			end;
		end;
	end;
end;

end.
