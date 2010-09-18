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
	Chart.Gradient.StartColor := clWhite;
	Chart.Gradient.EndColor := $ffc03e;

	// Fonts
	Chart.Title.Font.Name := 'Verdana';
	Chart.Title.Font.Height := -13;

end;

end.

