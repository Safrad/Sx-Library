unit uDemoDrawLinesAndRectangles;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoDrawLineAndRectangles = class(TDemo)
  strict private
    procedure DrawLine;
    procedure DrawRectangle;
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// The <A>IGPGraphics</A> interface provides a variety of drawing methods
/// including those shown in the following list:
///
///  -<A>IGPGraphics.DrawLine</A>
///  -<A>IGPGraphics.DrawRectangle</A>
///  -<A>IGPGraphics.DrawEllipse</A>
///  -<A>IGPGraphics.DrawArc</A>
///  -<A>IGPGraphics.DrawPath</A>
///  -<A>IGPGraphics.DrawCurve</A>
///  -<A>IGPGraphics.DrawBezier</A>
///
/// One of the arguments that you pass to such a drawing method is a
/// <A>IGPPen</A> object.
///
/// To draw lines and rectangles, you need a <A>IGPGraphics</A> object and a
/// <A>IGPPen</A> object. The <A>IGPGraphics</A> object provides the <A>DrawLine</A>
/// method, and the <A>IGPPen</A> object stores features of the line, such as
/// color and width.
///
/// The following example draws a line from (20, 10) to (300, 100).

procedure TDemoDrawLineAndRectangles.DrawLine;
var
  Pen: IGPPen;
begin
  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));
  Graphics.DrawLine(Pen, 20, 10, 300, 100);
end;

/// The first statement of code uses the <A>TGPPen</A> class constructor to create
/// a black pen. The one argument passed to the <A>TGPPen</A> constructor is a
/// <A>TGPColor</A> record. The values used to construct the <A>TGPColor</A> object
/// — (255, 0, 0, 0) — correspond to the alpha, red, green, and blue components
/// of the color. These values define an opaque black pen.
///
/// The following example draws a rectangle with its upper-left corner at
/// (10, 10). The rectangle has a width of 100 and a height of 50. The second
/// argument passed to the <A>TGPPen</A> constructor indicates that the pen width
/// is 5 pixels.

procedure TDemoDrawLineAndRectangles.DrawRectangle;
var
  BlackPen: IGPPen;
begin
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 5);
  Graphics.DrawRectangle(BlackPen, 10, 10, 100, 50);
end;

/// When the rectangle is drawn, the pen is centered on the rectangle's
/// boundary. Because the pen width is 5, the sides of the rectangle are drawn
/// 5 pixels wide, such that 1 pixel is drawn on the boundary itself, 2 pixels
/// are drawn on the inside, and 2 pixels are drawn on the outside. For more
/// details on pen alignment, see the next demo <I>Setting Pen Width and
/// Alignment</I>.
{$ENDREGION}

procedure TDemoDrawLineAndRectangles.Run;
begin
  DrawLine;
  DrawRectangle;
end;

initialization
  RegisterDemo('Using a Pen to Draw Lines and Shapes\Using a Pen to Draw Lines and Rectangles', TDemoDrawLineAndRectangles);

end.
