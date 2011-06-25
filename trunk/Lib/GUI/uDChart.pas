//* File:     Lib\GUI\uDChart.pas
//* Created:  1999-05-01
//* Modified: 2007-05-05
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDChart;

interface

uses Chart, SysUtils;

procedure HTMLStyle(const Chart: TChart);
procedure ChartToFile(const Chart: TChart; const FileName: TFileName);

implementation

uses
	Graphics,
	Series, TeCanvas,
	uDBitmap;
const
	BackgroundColor = clSilver; // Best font border.

procedure HTMLStyle(const Chart: TChart);
begin
	// Walls
{	Chart.LeftWall.Color := $ff8080;
	Chart.BottomWall.Color := $ff8080;}

	// Background
	Chart.BackColor := BackgroundColor;
(*	Chart.Gradient.Visible := False;
*	Chart.Gradient.Direction := gdFromCenter;
	Chart.Gradient.StartColor := $cef6ee{jaro}; // $F5ECCC {zima}; //$f4ebcb; //$ffc03e;
	Chart.Gradient.EndColor := clWhite; *)

	// Fonts
	Chart.Title.Font.Name := 'Verdana';
	Chart.Title.Font.Height := -13;
end;

procedure ChartToFile(const Chart: TChart; const FileName: TFileName);
var
	Bmp: TDBitmap;
begin
	Bmp := TDBitmap.Create;
	Bmp.SetSize(Chart.Width, Chart.Height);
	Chart.Draw(Bmp.Canvas, Bmp.GetRect);
	Bmp.TransparentColor := BackgroundColor;
	Bmp.Transparent := True;
	Bmp.SaveToFile(FileName);
	Bmp.Free;
end;

end.

