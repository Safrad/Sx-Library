unit uDemoCardinalSplines;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoCardinalSplines = class(TDemo)
  strict private
    procedure DrawBellShape;
    procedure DrawClosedCurve;
    procedure DrawWithTension;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoCardinalSplines }

{$REGION}
/// GDI+ supports several types of curves: ellipses, arcs, cardinal splines, and
/// Bézier splines. An ellipse is defined by its bounding rectangle; an arc is a
/// portion of an ellipse defined by a starting angle and a sweep angle. A
/// cardinal spline is defined by an array of points and a tension parameter —
/// the curve passes smoothly through each point in the array, and the tension
/// parameter influences the way the curve bends. A Bézier spline is defined by
/// two end points and two control points — the curve does not pass through the
/// control points, but the control points influence the direction and bend as
/// the curve goes from one end point to the other.
///
/// A cardinal spline is a curve that passes smoothly through a given set of
/// points. To draw a cardinal spline, create a <A>IGPGraphics</A> object and pass
/// an array of points to the <A>DrawCurve</A> method. The following example
/// draws a bell-shaped cardinal spline that passes through five designated
/// points:

procedure TDemoCardinalSplines.DrawBellShape;
const
  Points: array [0..4] of TGPPoint = (
    (X:  10; Y: 100),
    (X:  60; Y:  80),
    (X: 110; Y:  20),
    (X: 160; Y:  80),
    (X: 220; Y: 100));
var
  Pen: IGPPen;
  Brush: IGPSolidBrush;
  P: TGPPoint;
begin
  Pen := TGPPen.Create(TGPColor.Blue);
  Graphics.DrawCurve(Pen, Points);

  Brush := TGPSolidBrush.Create(TGPColor.Red);
  for P in Points do
    Graphics.FillRectangle(Brush, P.X - 2, P.Y - 2, 5, 5);
end;

/// You can use the <A>DrawClosedCurve</A> method of the <A>IGPGraphics</A>
/// interface to draw a closed cardinal spline. In a closed cardinal spline, the
/// curve continues through the last point in the array and connects with the
/// first point in the array.
///
/// The following example draws a closed cardinal spline that passes through six
/// designated points.

procedure TDemoCardinalSplines.DrawClosedCurve;
const
  Points: array [0..5] of TGPPoint = (
    (X: 310; Y:  60),
    (X: 400; Y:  80),
    (X: 450; Y:  40),
    (X: 430; Y: 120),
    (X: 370; Y: 100),
    (X: 330; Y: 160));
var
  Pen: IGPPen;
  Brush: IGPSolidBrush;
  P: TGPPoint;
begin
  Pen := TGPPen.Create(TGPColor.Blue);
  Graphics.DrawClosedCurve(Pen, Points);

  Brush := TGPSolidBrush.Create(TGPColor.Red);
  for P in Points do
    Graphics.FillRectangle(Brush, P.X - 2, P.Y - 2, 5, 5);
end;

/// You can change the way a cardinal spline bends by passing a tension argument
/// to the <A>DrawCurve</A> method. The following example draws three cardinal
/// splines that pass through the same set of points, with different tension
/// values. Note that when the tension is 0, the points are connected by
/// straight lines.

procedure TDemoCardinalSplines.DrawWithTension;
const
  Points: array [0..4] of TGPPoint = (
    (X:  20; Y: 170),
    (X: 100; Y: 130),
    (X: 200; Y: 220),
    (X: 300; Y: 170),
    (X: 400; Y: 200));
var
  Pen: IGPPen;
  Brush: IGPSolidBrush;
  P: TGPPoint;
begin
  Pen := TGPPen.Create(TGPColor.Blue);
  Graphics.DrawCurve(Pen, Points, 0.0);
  Pen.Color := TGPColor.Green;
  Graphics.DrawCurve(Pen, Points, 0.6);
  Pen.Color := TGPColor.Gray;
  Graphics.DrawCurve(Pen, Points, 1.0);

  Brush := TGPSolidBrush.Create(TGPColor.Red);
  for P in Points do
    Graphics.FillRectangle(Brush, P.X - 2, P.Y - 2, 5, 5);
end;
{$ENDREGION}

procedure TDemoCardinalSplines.Run;
begin
  DrawBellShape;
  DrawClosedCurve;
  DrawWithTension;
end;

initialization
  RegisterDemo('Constructing and Drawing Curves\Drawing Cardinal Splines', TDemoCardinalSplines);

end.
