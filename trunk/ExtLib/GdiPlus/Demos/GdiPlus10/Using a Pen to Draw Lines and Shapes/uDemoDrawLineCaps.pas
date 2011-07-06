unit uDemoDrawLineCaps;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoDrawLineCaps = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// You can draw the start or end of a line in one of several shapes called line
/// caps. Microsoft Windows GDI+ supports several line caps, such as round,
/// square, diamond, and arrowhead.
///
/// You can specify line caps for the start of a line (start cap), the end of a
/// line (end cap), or the dashes of a dashed line (dash cap).
///
/// The following example draws a line with an arrowhead at one end and a round
/// cap at the other end:

procedure TDemoDrawLineCaps.Run;
var
  Pen: IGPPen;
begin
  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0,255), 8);
  Pen.StartCap := LineCapArrowAnchor;
  Pen.EndCap := LineCapRoundAnchor;
  Graphics.DrawLine(Pen, 20, 175, 300, 175);
end;

/// <B>LineCapArrowAnchor</B> and <B>LineCapRoundAnchor</B> are elements of the
/// <A>TLineCap</A> enumeration.
{$ENDREGION}

initialization
  RegisterDemo('Using a Pen to Draw Lines and Shapes\Drawing a Line with Line Caps', TDemoDrawLineCaps);

end.
