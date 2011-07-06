unit uDemoCreateFigures;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoCreateFigures = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
    procedure Example3;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoCreateFigures }

{$REGION}
/// A path is a sequence of graphics primitives (lines, rectangles, curves,
/// text, and the like) that can be manipulated and drawn as a single unit. A
/// path can be divided into figures that are either open or closed. A figure
/// can contain several primitives.
///
/// You can draw a path by calling the <A>DrawPath</A> method of the
/// <A>IGPGraphics</A> interface, and you can fill a path by calling the
/// <A>FillPath</A> method of the <A>IGPGraphics</A> interface.
///
/// To create a path, construct a <A>IGPGraphicsPath</A> object, and then call
/// methods, such as <A>AddLine</A> and <A>AddCurve</A>, to add primitives to
/// the path.
///
/// The following example creates a path that has a single arc. The arc has a
/// sweep angle of –180 degrees, which is counterclockwise in the default
/// coordinate system (see the illustration above with the red pen).

procedure TDemoCreateFigures.Example1;
var
  Pen: IGPPen;
  Path: IGPGraphicsPath;
begin
  Pen := TGPPen.Create(TGPColor.Red);
  Path := TGPGraphicsPath.Create;
  Path.AddArc(10, 10, 50, 50, 0, -180);
  Graphics.DrawPath(Pen, Path);
end;

/// The following example creates a path that has two figures. The first figure
/// is an arc followed by a line. The second figure is a line followed by a
/// curve, followed by a line. The first figure is left open, and the second
/// figure is closed (see the illustration above with the blue pen).

procedure TDemoCreateFigures.Example2;
const
  Points: array [0..2] of TGPPoint = (
    (X: 110; Y: 80), (X: 120; Y: 90), (X: 100; Y: 110));
var
  Pen: IGPPen;
  Path: IGPGraphicsPath;
begin
  Pen := TGPPen.Create(TGPColor.Blue, 2);
  Path := TGPGraphicsPath.Create;

  // The first figure is started automatically, so there is
  // no need to call StartFigure here.
  Path.AddArc(175, 50, 50, 50, 0, -180);
  Path.AddLine(100, 0, 250,20);

  Path.StartFigure;
  Path.AddLine(120, 40, 75, 110);
  Path.AddCurve(Points);
  Path.AddLine(120, 120, 220, 200);
  Path.CloseFigure;

  Graphics.DrawPath(Pen, Path);
end;

/// In addition to adding lines and curves to paths, you can add closed shapes:
/// rectangles, ellipses, pies, and polygons. The following example creates a
/// path that has two lines, a rectangle, and an ellipse. The code uses a pen to
/// draw the path and a brush to fill the path (see the illustration above with
/// the green pen).

procedure TDemoCreateFigures.Example3;
var
  Pen: IGPPen;
  Brush: IGPBrush;
  Path: IGPGraphicsPath;
begin
  Pen := TGPPen.Create(TGPColor.Green, 4);
  Brush := TGPSolidBrush.Create(TGPColor.Create(200, 200, 200));
  Path := TGPGraphicsPath.Create;

  Path.AddLine(260, 10, 350, 40);
  Path.AddLine(350, 60, 280, 60);
  Path.AddRectangle(TGPRect.Create(300, 35, 20, 40));
  Path.AddEllipse(260, 75, 40, 30);

  Graphics.DrawPath(Pen, Path);
  Graphics.FillPath(Brush, Path);
end;

/// The path in the preceding example has three figures. The first figure
/// consists of the two lines, the second figure consists of the rectangle, and
/// the third figure consists of the ellipse. Even when there are no calls to
/// <A>CloseFigure</A> or <A>StartFigure</A>, intrinsically closed shapes, such
/// as rectangles and ellipses, are considered separate figures.
{$ENDREGION}

procedure TDemoCreateFigures.Run;
begin
  Example1;
  Example2;
  Example3;
end;

initialization
  RegisterDemo('Constructing and Drawing Paths\Creating Figures from Lines, Curves and Shapes', TDemoCreateFigures);

end.
