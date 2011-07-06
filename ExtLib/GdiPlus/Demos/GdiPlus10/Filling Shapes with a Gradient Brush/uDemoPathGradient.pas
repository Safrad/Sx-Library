unit uDemoPathGradient;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoPathGradient = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
    procedure Example3;
    procedure Example4;
    procedure Example5;
    procedure Example6;
    procedure Example7;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoPathGradient }

{$REGION}
/// The <A>IGPPathGradientBrush</A> interface allows you to customize the way you
/// fill a shape with gradually changing colors. A <A>IGPPathGradientBrush</A>
/// object has a boundary path and a center point. You can specify one color for
/// the center point and another color for the boundary. You can also specify
/// separate colors for each of several points along the boundary.
///
/// Note  In GDI+, a path is a sequence of lines and curves maintained by a
/// <A>IGPGraphicsPath</A> object. For more information about GDI+ paths, see
/// the examples under <A>Constructing and Drawing Paths</A>.
///
/// The following example fills an ellipse with a path gradient brush. The
/// center color is set to blue and the boundary color is set to aqua.

procedure TDemoPathGradient.Example1;
var
  Path: IGPGraphicsPath;
  Brush: IGPPathGradientBrush;
  SurroundColors: array [0..0] of TGPColor;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddEllipse(0, 0, 140, 70);
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterColor := TGPColor.Create(255, 0, 0, 255);
  SurroundColors[0].Initialize(255, 0, 255, 255);
  Brush.SetSurroundColors(SurroundColors);
  Graphics.FillEllipse(Brush, 0, 0, 140, 70);
end;

/// The top-left illustration above shows the filled ellipse.
///
/// By default, a path gradient brush does not extend outside the boundary of
/// the path. If you use the path gradient brush to fill a shape that extends
/// beyond the boundary of the path, the area of the screen outside the path
/// will not be filled. The next illustration aboveshows what happens if you
/// change the <A>FillEllipse</A> call in the preceding code to the following:

procedure TDemoPathGradient.Example2;
var
  Path: IGPGraphicsPath;
  Brush: IGPPathGradientBrush;
  SurroundColors: array [0..0] of TGPColor;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddEllipse(150, 0, 140, 70);
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterColor := TGPColor.Create(255, 0, 0, 255);
  SurroundColors[0].Initialize(255, 0, 255, 255);
  Brush.SetSurroundColors(SurroundColors);
  Graphics.FillRectangle(Brush, 150, 10, 200, 40);
end;

/// <H>Specifying Points on the Boundary</H>
/// The following example constructs a path gradient brush from a star-shaped
/// path. The code sets the <A>CenterColor</A> property to the color at the
/// centroid of the star to red. Then the code calls the <A>SetSurroundColors</A>
/// method to specify various colors (stored in the colors array) at the
/// individual points in the points array. The final code statement fills the
/// star-shaped path with the path gradient brush.

procedure TDemoPathGradient.Example3;
const
  Points: array [0..9] of TGPPoint = (
    (X: 375; Y:   0), (X: 400; Y:  50),
    (X: 450; Y:  50), (X: 412; Y:  75),
    (X: 450; Y: 150), (X: 375; Y: 100),
    (X: 300; Y: 150), (X: 337; Y:  75),
    (X: 300; Y:  50), (X: 350; Y:  50));
var
  Path: IGPGraphicsPath;
  Brush: IGPPathGradientBrush;
  SurroundColors: array [0..9] of TGPColor;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddLines(Points);
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterColor := TGPColor.Create(255, 255, 0, 0);

  SurroundColors[0].Initialize(255,   0,   0,   0);
  SurroundColors[1].Initialize(255,   0, 255,   0);
  SurroundColors[2].Initialize(255,   0,   0, 255);
  SurroundColors[3].Initialize(255, 255, 255, 255);
  SurroundColors[4].Initialize(255,   0,   0,   0);
  SurroundColors[5].Initialize(255,   0, 255,   0);
  SurroundColors[6].Initialize(255,   0,   0, 255);
  SurroundColors[7].Initialize(255, 255, 255, 255);
  SurroundColors[8].Initialize(255,   0,   0,   0);
  SurroundColors[9].Initialize(255,   0, 255,   0);
  Brush.SetSurroundColors(SurroundColors);
  Graphics.FillPath(Brush, Path);
end;

/// The 3rd illustration above shows the filled star.
///
/// The following example constructs a path gradient brush based on an array of
/// points. A color is assigned to each of the five points in the array. If you
/// were to connect the five points by straight lines, you would get a
/// five-sided polygon. A color is also assigned to the center (centroid) of
/// that polygon — in this example, the center is set to white. The final code
/// statement in the example fills a rectangle with the path gradient brush.
///
/// The color used to fill the rectangle is white at the center and changes
/// gradually as you move away from the center toward the points in the array.
/// For example, as you move from the center to the top-left corner, the color
/// changes gradually from white to red, and as you move from the center to the
/// top-right corner, the color changes gradually from white to green.

procedure TDemoPathGradient.Example4;
const
  Points: array [0..4] of TGPPointF = (
    (X: 460; Y:   0),
    (X: 620; Y:   0),
    (X: 620; Y: 200),
    (X: 540; Y: 150),
    (X: 460; Y: 200));
var
  Brush: IGPPathGradientBrush;
  Pen: IGPPen;
  SurroundColors: array [0..4] of TGPColor;
begin
  Brush := TGPPathGradientBrush.Create(Points);
  Brush.CenterColor := TGPColor.White;

  SurroundColors[0].Initialize(255, 255,   0,   0);
  SurroundColors[1].Initialize(255,   0, 255,   0);
  SurroundColors[2].Initialize(255,   0, 255,   0);
  SurroundColors[3].Initialize(255,   0,   0, 255);
  SurroundColors[4].Initialize(255, 255,   0,   0);
  Brush.SetSurroundColors(SurroundColors);
  Graphics.FillRectangle(Brush, 460, 0, 180, 220);

  Pen := TGPPen.Create(TGPColor.Blue);
  Graphics.DrawRectangle(Pen, 460, 0, 180, 220);
end;

/// Note that there is no <A>IGPGraphicsPath</A> object in the preceding code. The
/// particular <A>TGPPathGradientBrush</A> constructor in the example receives an
/// array of points but does not require a <A>IGPGraphicsPath</A> object. Also,
/// note that the path gradient brush is used to fill a rectangle, not a path.
/// The rectangle is larger than the path used to define the brush, so some of
/// the rectangle is not painted by the brush. The 4th illustration above shows
/// the rectangle (blue line) and the portion of the rectangle painted by the
/// path gradient brush.
///
/// <H>Customizing a Path Gradient</H>
/// One way to customize a path gradient brush is to set its focus scales. The
/// focus scales specify an inner path that lies inside the main path. The
/// center color is displayed everywhere inside that inner path rather than only
/// at the center point. To set the focus scales of a path gradient brush, call
/// the <A>SetFocusScales</A> method.
///
/// The following example creates a path gradient brush based on an elliptical
/// path. The code sets the boundary color to blue, sets the center color to
/// aqua, and then uses the path gradient brush to fill the elliptical path.
///
/// Next the code sets the focus scales of the path gradient brush. The x focus
/// scale is set to 0.3, and the y focus scale is set to 0.8. The code calls the
/// <A>TranslateTransform</A> method of a <A>IGPGraphics</A> object so that the
/// subsequent call to <A>FillPath</A> fills an ellipse that sits to the right
/// of the first ellipse.
///
/// To see the effect of the focus scales, imagine a small ellipse that shares
/// its center with the main ellipse. The small (inner) ellipse is the main
/// ellipse scaled (about its center) horizontally by a factor of 0.3 and
/// vertically by a factor of 0.8. As you move from the boundary of the outer
/// ellipse to the boundary of the inner ellipse, the color changes gradually
/// from blue to aqua. As you move from the boundary of the inner ellipse to the
/// shared center, the color remains aqua.

procedure TDemoPathGradient.Example5;
var
  Path: IGPGraphicsPath;
  Brush: IGPPathGradientBrush;
  SurroundColors: array [0..0] of TGPColor;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddEllipse(0, 230, 200, 100);
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.GammaCorrection := True;

  SurroundColors[0].Initialize(255, 0, 0, 255);
  Brush.SetSurroundColors(SurroundColors);
  Brush.CenterColor := TGPColor.Create(255, 0, 255, 255);
  Graphics.FillPath(Brush, Path);

  Brush.SetFocusScales(0.3, 0.8);
  Graphics.TranslateTransform(220, 0);
  Graphics.FillPath(Brush, Path);
  Graphics.ResetTransform;
end;

/// The fifth illustration above (the first on the 2nd row) shows the output of
/// the preceding code. The ellipse on the left is aqua only at the center
/// point. The ellipse on the right is aqua everywhere inside the inner path.
///
/// Another way to customize a path gradient brush is to specify an array of
/// preset colors and an array of interpolation positions.
///
/// The following example creates a path gradient brush based on a triangle. The
/// code sets the <A>IGPPathGradientBrush.InterpolationColors</A> property of the
/// path gradient brush to specify an array of interpolation colors (dark green,
/// aqua, blue) and an array of interpolation positions (0, 0.25, 1). As you
/// move from the boundary of the triangle to the center point, the color
/// changes gradually from dark green to aqua and then from aqua to blue. The
/// change from dark green to aqua happens in 25 percent of the distance from
/// dark green to blue.

procedure TDemoPathGradient.Example6;
const
  Points: array [0..2] of TGPPoint = (
    (X: 540; Y: 230), (X: 640; Y: 430), (X: 440; Y: 430));
  InterPositions: array [0..2] of Single = (
    0.00,  // Dark green is at the boundary of the triangle.
    0.25,  // Aqua is 25 percent of the way from the boundary to the center point
    1.00); // Blue is at the center point.
var
  Brush: IGPPathGradientBrush;
  PresetColors: array [0..2] of TGPColor;
  Blend: IGPColorBlend;
begin
  Brush := TGPPathGradientBrush.Create(Points);
  PresetColors[0] := TGPColor.Green;
  PresetColors[1] := TGPColor.Aqua;
  PresetColors[2] := TGPColor.Blue;
  Blend := TGPColorBlend.Create(PresetColors, InterPositions);
  Brush.InterpolationColors := Blend;
  Graphics.FillRectangle(Brush, 440, 230, 200, 200);
end;

/// The triangle in the illustration above shows the output of the preceding
/// code.
///
/// <H>Setting the Center Point</H>
/// By default, the center point of a path gradient brush is at the centroid of
/// the path used to construct the brush. You can change the location of the
/// center point by setting the <A>IGPPathGradientBrush.CenterPoint</A> property
/// of the <A>IGPPathGradientBrush</A> interface.
///
/// The following example creates a path gradient brush based on an ellipse. The
/// center of the ellipse is at (70, 385), but the center point of the path
/// gradient brush is set to (120, 390).

procedure TDemoPathGradient.Example7;
var
  Path: IGPGraphicsPath;
  Brush: IGPPathGradientBrush;
  SurroundColors: array [0..0] of TGPColor;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddEllipse(0, 350, 140, 70);
  Brush := TGPPathGradientBrush.Create(Path);
  Brush.CenterPoint := TGPPointF.Create(120, 390);
  SurroundColors[0] := TGPColor.Aqua;
  Brush.SetSurroundColors(SurroundColors);
  Brush.CenterColor := TGPColor.Blue;
  Graphics.FillEllipse(Brush, 0, 350, 140, 70);

  Brush.CenterPoint := TGPPointF.Create(145, 385);
  SurroundColors[0] := TGPColor.Yellow;
  Brush.SetSurroundColors(SurroundColors);
  Brush.CenterColor := TGPColor.Red;
  Graphics.TranslateTransform(150, 0);
  Graphics.FillEllipse(Brush, 0, 350, 140, 70);
end;

/// The next illustration above shows the filled ellipse and the center point of
/// the path gradient brush.
///
/// You can set the center point of a path gradient brush to a location outside
/// the path that was used to construct the brush, as you can see in the final
/// illustration above.
{$ENDREGION}

procedure TDemoPathGradient.Run;
begin
  Example1;
  Example2;
  Example3;
  Example4;
  Example5;
  Example6;
  Example7;
end;

initialization
  RegisterDemo('Filling Shapes with a Gradient Brush\Creating a Path Gradient', TDemoPathGradient);

end.
