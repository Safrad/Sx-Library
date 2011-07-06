unit uDemoFillSolidColor;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoFillSolidColor = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// A Microsoft Windows GDI+Brush object is used to fill the interior of a
/// closed shape. GDI+ defines several fill styles: solid color, hatch pattern,
/// image texture, and color gradient.
///
/// To fill a shape with a solid color, create a <A>IGPSolidBrush</A> object, and
/// then that <A>IGPSolidBrush</A> object as an argument to one of the fill
/// methods of the <A>IGPGraphics</A> interface. The following example shows how
/// to fill an ellipse with the color red:

procedure TDemoFillSolidColor.Run;
var
  SolidBrush: IGPSolidBrush;
begin
  SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 255, 0, 0));
  Graphics.FillEllipse(SolidBrush, 0, 0, 100, 60);
end;

/// In the preceding example, the <A>TGPSolidBrush</A> constructor takes a
/// <A>TGPColor</A> record reference as its only argument. The values used by the
/// <A>TGPColor</A> constructor represent the alpha, red, green, and blue
/// components of the color. Each of these values must be in the range 0 through
/// 255. The first 255 indicates that the color is fully opaque, and the second
/// 255 indicates that the red component is at full intensity. The two zeros
/// indicate that the green and blue components both have an intensity of 0.
///
/// The four numbers (0, 0, 100, 60) passed to the <A>FillEllipse</A> method
/// specify the location and size of the bounding rectangle for the ellipse. The
/// rectangle has an upper-left corner of (0, 0), a width of 100, and a height
/// of 60.
{$ENDREGION}

initialization
  RegisterDemo('Using a Brush to Fill Shapes\Filling a Shape with a Solid Color', TDemoFillSolidColor);

end.
