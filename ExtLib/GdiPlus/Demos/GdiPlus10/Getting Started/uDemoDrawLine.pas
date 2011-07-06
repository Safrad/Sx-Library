unit uDemoDrawLine;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoDrawLine = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// To draw a line in Microsoft Windows GDI+ you need a <A>IGPGraphics</A> object,
/// a <A>IGPPen</A> object, and a <A>TGPColor</A> record. The <A>IGPGraphics</A>
/// object provides the <A>IGPGraphics.DrawLine</A> method, and the <A>IGPPen</A>
/// object holds attributes of the line, such as color and width. The <A>IGPPen</A>
/// object is passed as an argument to the <A>IGPGraphics.DrawLine</A> method.
///
/// The following code draws a line from (0, 0) to (200, 100). The argument
/// passed to the <A>TGPPen</A> constructor is a reference to a <A>TGPColor</A>
/// record. The four numbers passed to the color constructor represent the
/// alpha, red, green, and blue components of the color. The alpha component
/// determines the transparency of the color; 0 is fully transparent and 255 is
/// fully opaque. The four numbers passed to the <A>IGPGraphics.DrawLine</A>
/// method represent the starting point (0, 0) and the ending point (200, 100)
/// of the line.

procedure TDemoDrawLine.Run;
var
  Pen: IGPPen;
begin
  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 255));
  Graphics.DrawLine(Pen, 0, 0, 200, 100);
end;
{$ENDREGION}

initialization
  RegisterDemo('Getting Started\Drawing a Line', TDemoDrawLine);

end.
