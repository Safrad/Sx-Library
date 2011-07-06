unit uDemoPenWidthAndAlignment;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoPenWidthAndAlignment = class(TDemo)
  strict private
    procedure DrawLine;
    procedure DrawRectangle;
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// When you create a <A>IGPPen</A> object, you can supply the pen width as one of
/// the arguments to the constructor. You can also change the pen width by using
/// the <A>Width</A> property.
///
/// A theoretical line has a width of zero. When you draw a line, the pixels are
/// centered on the theoretical line. The following example draws a specified
/// line twice: once with a black pen of width 1 and once with a green pen of
/// width 10.

procedure TDemoPenWidthAndAlignment.DrawLine;
var
  BlackPen, GreenPen: IGPPen;
begin
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 1);
  GreenPen := TGPPen.Create(TGPColor.Create(255, 0, 255, 0), 10);

  GreenPen.Alignment := PenAlignmentCenter;
  Graphics.DrawLine(GreenPen, 10, 100, 100, 50);
  Graphics.DrawLine(BlackPen, 10, 100, 100, 50);
end;

/// The green pixels and the black pixels are centered on the theoretical line.
///
/// The following example draws a specified rectangle twice: once with a black
/// pen of width 1 and once with a green pen of width 10. The code passes the
/// value <B>PenAlignmentCenter</B> (an element of the <A>TPenAlignment</A>
/// enumeration) to the <A>Alignment</A> property to specify that the pixels
/// drawn with the green pen are centered on the boundary of the rectangle.
/// You can change the green pen's alignment by setting the <A>Alignment</A>
/// property to <B>PenAlignmentInset</B>. Then the pixels in the wide green line
/// appear on the inside of the rectangle

procedure TDemoPenWidthAndAlignment.DrawRectangle;
var
  BlackPen, GreenPen: IGPPen;
begin
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 1);
  GreenPen := TGPPen.Create(TGPColor.Create(255, 0, 255, 0), 10);

  Graphics.DrawRectangle(GreenPen, 10, 120, 50, 50);
  Graphics.DrawRectangle(BlackPen, 10, 120, 50, 50);

  GreenPen.Alignment := PenAlignmentInset;
  Graphics.DrawRectangle(GreenPen, 80, 120, 50, 50);
  Graphics.DrawRectangle(BlackPen, 80, 120, 50, 50);
end;
{$ENDREGION}

procedure TDemoPenWidthAndAlignment.Run;
begin
  DrawLine;
  DrawRectangle;
end;

initialization
  RegisterDemo('Using a Pen to Draw Lines and Shapes\Setting Pen Width and Alignment', TDemoPenWidthAndAlignment);

end.
