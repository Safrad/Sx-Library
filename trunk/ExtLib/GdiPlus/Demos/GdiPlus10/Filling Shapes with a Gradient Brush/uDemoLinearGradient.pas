unit uDemoLinearGradient;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoLinearGradient = class(TDemo)
  strict private
    procedure HorizontalGradient;
    procedure CustomGradient;
    procedure DiagonalGradient;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoLinearGradient }

{$REGION}
/// You can use a gradient brush to fill a shape with a gradually changing
/// color. For example, you can use a horizontal gradient to fill a shape with
/// color that changes gradually as you move from the left edge of the shape to
/// the right edge. Imagine a rectangle with a left edge that is black
/// (represented by red, green, and blue components 0, 0, 0) and a right edge
/// that is red (represented by 255, 0, 0). If the rectangle is 256 pixels wide,
/// the red component of a given pixel will be one greater than the red
/// component of the pixel to its left. The leftmost pixel in a row has color
/// components (0, 0, 0), the second pixel has (1, 0, 0), the third pixel has
/// (2, 0, 0), and so on, until you get to the rightmost pixel, which has color
/// components (255, 0, 0). These interpolated color values make up the color
/// gradient.
///
/// A linear gradient changes color as you move horizontally, vertically, or
/// parallel to a specified slanted line. A path gradient changes color as you
/// move about the interior and boundary of a path. You can customize path
/// gradients to achieve a wide variety of effects.
///
/// GDI+ provides the <A>IGPLinearGradientBrush</A> and <A>IGPPathGradientBrush</A>
/// interfaces, both of which inherit from the <A>IGPBrush</A> interface.
///
/// GDI+ provides horizontal, vertical, and diagonal linear gradients. By
/// default, the color in a linear gradient changes uniformly. However, you can
/// customize a linear gradient so that the color changes in a non-uniform
/// fashion.
///
/// <H>Horizontal Linear Gradients</H>
/// The following example uses a horizontal linear gradient brush to fill a
/// line, an ellipse, and a rectangle:

procedure TDemoLinearGradient.HorizontalGradient;
var
  Brush: IGPLinearGradientBrush;
  Pen: IGPPen;
begin
  Brush := TGPLinearGradientBrush.Create(
    TGPPoint.Create(0, 10), TGPPoint.Create(200, 10),
    TGPColor.Red, TGPColor.Blue);
  Pen := TGPPen.Create(Brush);

  Graphics.DrawLine(Pen, 0, 10, 200, 10);
  Graphics.FillEllipse(Brush, 0, 30, 200, 100);
  Graphics.FillRectangle(Brush, 0, 155, 500, 30);
end;

/// The <A>TGPLinearGradientBrush</A> constructor receives four arguments: two
/// points and two colors. The first point (0, 10) is associated with the first
/// color (red), and the second point (200, 10) is associated with the second
/// color (blue). As you would expect, the line drawn from (0, 10) to (200, 10)
/// changes gradually from red to blue.
///
/// The 10s in the points (50, 10) and (200, 10) are not important. What's
/// important is that the two points have the same second coordinate — the line
/// connecting them is horizontal. The ellipse and the rectangle also change
/// gradually from red to blue as the horizontal coordinate goes from 0 to 200.
///
/// The top-left illustration above shows the line, the ellipse, and the
/// rectangle. Note that the color gradient repeats itself as the horizontal
/// coordinate increases beyond 200.
///
/// <H>Customizing Linear Gradients</H>
/// In the preceding example, the color components change linearly as you move
/// from a horizontal coordinate of 0 to a horizontal coordinate of 200. For
/// example, a point whose first coordinate is halfway between 0 and 200 will
/// have a blue component that is halfway between 0 and 255.
///
/// GDI+ allows you to adjust the way a color varies from one edge of a gradient
/// to the other. Suppose you want to create a gradient brush that changes from
/// black to red according to the following table.
///
/// Horizontal coordinate: 0, RGB components (0, 0, 0)
/// Horizontal coordinate: 40, RGB components (128, 0, 0)
/// Horizontal coordinate: 200, RGB components (255, 0, 0)
///
/// Note that the red component is at half intensity when the horizontal
/// coordinate is only 20 percent of the way from 0 to 200.
///
/// The following example sets the <A>Blend</A> property of a
/// <A>IGPLinearGradientBrush</A> object to associate three relative intensities
/// with three relative positions. As in the preceding table, a relative
/// intensity of 0.5 is associated with a relative position of 0.2. The code
/// fills an ellipse and a rectangle with the gradient brush.

procedure TDemoLinearGradient.CustomGradient;
const
  RelativeIntensities: array [0..2] of Single = (0.0, 0.5, 1.0);
  RelativePositions  : array [0..2] of Single = (0.0, 0.2, 1.0);
var
  Blend: IGPBlend;
  Brush: IGPLinearGradientBrush;
begin
  Brush := TGPLinearGradientBrush.Create(
    TGPPoint.Create(0, 10), TGPPoint.Create(200, 10),
    TGPColor.Red, TGPColor.Blue);

  Blend := TGPBlend.Create(RelativeIntensities, RelativePositions);
  Brush.Blend := Blend;

  Graphics.FillEllipse(Brush, 0, 230, 200, 100);
  Graphics.FillRectangle(Brush, 0, 355, 500, 30);
end;

/// The second illustration above shows the resulting ellipse and rectangle.
///
/// <H>Diagonal Linear Gradients</H>
/// The gradients in the preceding examples have been horizontal; that is, the
/// color changes gradually as you move along any horizontal line. You can also
/// define vertical gradients and diagonal gradients. The following code passes
/// the points (0, 400) and (200, 500) to a <A>TGPLinearGradientBrush</A>
/// constructor. The color blue is associated with (0, 400), and the color green
/// is associated with (200, 500). A line (with pen width 10) and an ellipse are
/// filled with the linear gradient brush.

procedure TDemoLinearGradient.DiagonalGradient;
var
  Brush: IGPLinearGradientBrush;
  Pen: IGPPen;
begin
  Brush := TGPLinearGradientBrush.Create(
    TGPPoint.Create(0, 400), TGPPoint.Create(200, 500),
    TGPColor.Blue, TGPColor.Lime);
  Pen := TGPPen.Create(Brush, 10);

  Graphics.DrawLine(Pen, 0, 400, 400, 600);
  Graphics.FillEllipse(Brush, 10, 500, 200, 100);
end;

/// The last illustration above shows the line and the ellipse. Note that the
/// color in the ellipse changes gradually as you move along any line that is
/// parallel to the line passing through (0, 400) and (200, 500).
{$ENDREGION}

procedure TDemoLinearGradient.Run;
begin
  HorizontalGradient;
  CustomGradient;
  DiagonalGradient;
end;

initialization
  RegisterDemo('Filling Shapes with a Gradient Brush\Creating a Linear Gradient', TDemoLinearGradient);

end.
