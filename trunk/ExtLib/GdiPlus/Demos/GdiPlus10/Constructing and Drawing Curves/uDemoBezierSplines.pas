unit uDemoBezierSplines;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoBezierSplines = class(TDemo)
  strict private
    procedure DrawBezier1;
    procedure DrawBezier2;
  strict protected
    procedure Run; override;
  end;

implementation

uses
  SysUtils;

{ TDemoBezierSplines }

{$REGION}
/// A Bézier spline is defined by four points: a start point, two control
/// points, and an end point. The following example draws a Bézier spline with
/// start point (10, 100) and end point (200, 100). The control points are (100,
/// 10) and (150, 150):

procedure TDemoBezierSplines.DrawBezier1;
var
  P1, C1, C2, P2: TGPPointF;
  Pen: IGPPen;
  Font: IGPFont;
  Brush: IGPBrush;
begin
  P1.Initialize(10, 100); // start point
  C1.Initialize(100, 10); // first control point
  C2.Initialize(150, 150); // second control point
  P2.Initialize(200, 100); // end point
  Pen := TGPPen.Create(TGPColor.Blue);
  Graphics.DrawBezier(Pen, P1, C1, C2, P2);

  Pen.Color := TGPColor.Red;
  Graphics.DrawPolygon(Pen, [P1, C1, P2, C2]);

  Font := TGPFont.Create('Tahoma', 8);
  Brush := TGPSolidBrush.Create(TGPColor.Black);
  Graphics.DrawString('P1', Font, P1, Brush);
  Graphics.DrawString('C1', Font, C1, Brush);
  Graphics.DrawString('C2', Font, C2, Brush);
  Graphics.DrawString('P2', Font, P2, Brush);
end;

/// The top-left above illustration shows the resulting Bézier spline along with
/// its start point, control points, and end point. The illustration also shows
/// the spline's convex hull, which is a polygon formed by connecting the four
/// points with straight lines.
///
/// You can use the <A>DrawBeziers</A> method of the <A>IGPGraphics</A> interface
/// to draw a sequence of connected Bézier splines. The following example draws
/// a curve that consists of two connected Bézier splines. The end point of the
/// first Bézier spline is the start point of the second Bézier spline.

procedure TDemoBezierSplines.DrawBezier2;
const
  Points: array [0..6] of TGPPointF = (
    (X: 235; Y: 100),  // start point of first spline
    (X: 300; Y:  10),  // first control point of first spline
    (X: 305; Y:  50),  // second control point of first spline
    (X: 325; Y: 150),  // end point of first spline and start point of second spline
    (X: 340; Y:  80),  // first control point of second spline
    (X: 400; Y: 200),  // second control point of second spline
    (X: 425; Y:  80)); // end point of second spline
var
  Pen: IGPPen;
  Font: IGPFont;
  RedBrush, BlackBrush: IGPBrush;
  I: Integer;
  P: TGPPointF;
begin
  Pen := TGPPen.Create(TGPColor.Blue);
  Graphics.DrawBeziers(Pen, Points);

  Font := TGPFont.Create('Tahoma', 8);
  RedBrush := TGPSolidBrush.Create(TGPColor.Red);
  BlackBrush := TGPSolidBrush.Create(TGPColor.Black);
  for I := 0 to Length(Points) - 1 do
  begin
    P := Points[I];
    Graphics.FillRectangle(RedBrush, P.X - 2, P.Y - 2, 5, 5);
    Graphics.DrawString(Format('P[%d]', [I]), Font, P, BlackBrush);
  end;
end;
{$ENDREGION}

procedure TDemoBezierSplines.Run;
begin
  DrawBezier1;
  DrawBezier2;
end;

initialization
  RegisterDemo('Constructing and Drawing Curves\Drawing Bezier Splines', TDemoBezierSplines);

end.
