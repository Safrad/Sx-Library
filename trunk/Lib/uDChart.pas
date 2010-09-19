//* File:     Lib\uDChart.pas
//* Created:  1999-05-01
//* Modified: 2005-09-05
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDChart;

interface

uses Chart, SysUtils;

procedure HTMLStyle(Chart: TChart);
procedure ChartToFile(Chart: TChart; FileName: TFileName);

implementation

uses
	Graphics,
	Series, TeCanvas,
	uDBitmap;

procedure HTMLStyle(Chart: TChart);
begin
	// Walls
	Chart.LeftWall.Color := $ff8080;
	Chart.BottomWall.Color := $ff8080;

	// Background
	Chart.Gradient.Visible := True;
	Chart.Gradient.Direction := gdFromCenter;
	Chart.Gradient.StartColor := $F5ECCC; //$f4ebcb; //$ffc03e;
	Chart.Gradient.EndColor := clWhite;

	// Fonts
	Chart.Title.Font.Name := 'Verdana';
	Chart.Title.Font.Height := -13;

end;

procedure ChartToFile(Chart: TChart; FileName: TFileName);
var
	Bmp: TDBitmap;
begin
	Bmp := TDBitmap.Create;
	Bmp.SetSize(Chart.Width, Chart.Height);
	Chart.Draw(Bmp.Canvas, Bmp.GetRect);
	Bmp.SaveToFile(FileName);
	Bmp.Free;
end;

end.

