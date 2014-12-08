unit uDChart;

interface

uses Chart, SysUtils;

procedure AutoChartLimit(const Chart: TChart);
procedure HTMLStyle(const Chart: TChart);
procedure ChartToFile(const Chart: TChart; const FileName: TFileName);

implementation

uses
	Graphics,
	Series, TeCanvas, Math,
	uDBitmap, uTypes, TeEngine;
const
	BackgroundColor = clSilver; // Best font border.

procedure AutoChartLimit(const Chart: TChart);
var
	i: SG;
	MinValue, MaxValue: Double;
begin
	MinValue := MaxInt;
	MaxValue := MinInt;
	for i := 0 to Chart.SeriesCount - 1 do
	begin
		if Chart.Series[i].Active then
		begin
			MinValue := Min(MinValue, Chart.Series[i].MinYValue);
			MaxValue := Max(MaxValue, Chart.Series[i].MaxYValue);
		end;
	end;

	Chart.LeftAxis.Maximum := MaxInt;
	Chart.LeftAxis.Minimum := 100 * (Floor((MinValue - 0) / 100));
	Chart.LeftAxis.Maximum := 100 * (Ceil((MaxValue + (MaxValue - MinValue) / 20) / 100));
	Chart.LeftAxis.AutomaticMaximum := False;
	Chart.LeftAxis.AutomaticMinimum := False;
end;

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
	Bmp.SetSize(Chart.Width, Chart.Height, BackgroundColor);
	Chart.Draw(Bmp.Canvas, Bmp.GetFullRect);
	Bmp.TransparentColor := BackgroundColor;
	Bmp.Transparent := True;
	Bmp.SaveToFile(FileName);
	Bmp.Free;
end;

end.

