//* File:     Lib\uDChart.pas
//* Created:  1999-05-01
//* Modified: 2005-06-26
//* Version:  X.X.34.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@centrum.cz
//* Web:      http://safrad.webzdarma.cz

unit uDChart;

interface

uses Chart;

procedure HTMLStyle(Chart: TChart);

implementation

uses
	Graphics,
	Series, TeCanvas;

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

end.

