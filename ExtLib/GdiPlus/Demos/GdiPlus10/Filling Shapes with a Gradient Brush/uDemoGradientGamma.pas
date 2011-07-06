unit uDemoGradientGamma;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoGradientGamma = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoGradientGamma }

{$REGION}
/// You can enable gamma correction for a gradient brush by setting the
/// <A>GammaCorrection</A> property of at brush to True. You can disable gamma
/// correction by setting the <A>GammaCorrection</A> property to False. Gamma
/// correction is disabled by default.
///
/// The following example creates a linear gradient brush and uses that brush to
/// fill two rectangles. The first rectangle is filled without gamma correction
/// and the second rectangle is filled with gamma correction.

procedure TDemoGradientGamma.Example1;
var
  Brush: IGPLinearGradientBrush;
begin
  Brush := TGPLinearGradientBrush.Create(TGPPoint.Create(0, 10), TGPPoint.Create(200, 10),
    TGPColor.Red, TGPColor.Blue);
  Graphics.FillRectangle(Brush, 0, 0, 200, 50);
  Brush.GammaCorrection := True;
  Graphics.FillRectangle(Brush, 0, 60, 200, 50);
end;

/// The illustration above shows the two filled rectangles. The top rectangle,
/// which does not have gamma correction, appears dark in the middle. The bottom
/// rectangle, which has gamma correction, appears to have more uniform intensity.
///
/// The following example creates a path gradient brush based on a star-shaped
/// path. The code uses the path gradient brush with gamma correction disabled
/// (the default) to fill the path. Then the code sets the
/// <A>GammaCorrection</A> property to True to enable gamma correction for the
/// path gradient brush. The call to <A>IGPGraphics.TranslateTransform</A> sets
/// the world transformation of a <A>IGPGraphics</A> object so that the subsequent
/// call to <A>FillPath</A> fills a star that sits to the right of the first
/// star.

procedure TDemoGradientGamma.Example2;
const
  Points: array [0..9] of TGPPoint = (
    (X:  75; Y: 120), (X: 100; Y: 170),
    (X: 150; Y: 170), (X: 112; Y: 195),
    (X: 150; Y: 270), (X:  75; Y: 220),
    (X:   0; Y: 270), (X:  37; Y: 195),
    (X:   0; Y: 170), (X:  50; Y: 170));
var
  Path: IGPGraphicsPath;
  Brush: IGPPathGradientBrush;
  Colors: array [0..9] of TGPColor;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddLines(Points);

  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterColor := TGPColor.Red;

  Colors[0].Initialize(255,   0,   0,   0);
  Colors[1].Initialize(255,   0, 255,   0);
  Colors[2].Initialize(255,   0,   0, 255);
  Colors[3].Initialize(255, 255, 255, 255);
  Colors[4].Initialize(255,   0,   0,   0);
  Colors[5].Initialize(255,   0, 255,   0);
  Colors[6].Initialize(255,   0,   0, 255);
  Colors[7].Initialize(255, 255, 255, 255);
  Colors[8].Initialize(255,   0,   0,   0);
  Colors[9].Initialize(255,   0, 255,   0);
  Brush.SetSurroundColors(Colors);

  Graphics.FillPath(Brush, Path);
  Brush.GammaCorrection := True;
  Graphics.TranslateTransform(200, 0);
  Graphics.FillPath(Brush, Path);
end;

/// The illustration above shows the output of the preceding code. The star on
/// the right has gamma correction. Note that the star on the left, which does
/// not have gamma correction, has areas that appear dark.
{$ENDREGION}

procedure TDemoGradientGamma.Run;
begin
  Example1;
  Example2;
end;

initialization
  RegisterDemo('Filling Shapes with a Gradient Brush\Applying Gamma Correction to a Gradient', TDemoGradientGamma);

end.
