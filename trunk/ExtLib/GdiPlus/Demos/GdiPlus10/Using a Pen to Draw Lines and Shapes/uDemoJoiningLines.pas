unit uDemoJoiningLines;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoJoiningLines = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// A line join is the common area that is formed by two lines whose ends meet
/// or overlap. Microsoft Windows GDI+ provides four line join styles: miter,
/// bevel, round, and miter clipped. Line join style is a property of the
/// <A>IGPPen</A> interface. When you specify a line join style for a pen and then
/// use that pen to draw a path, the specified join style is applied to all the
/// connected lines in the path.
///
/// You can specify the line join style by using the <A>LineJoin</A> property of
/// the <A>IGPPen</A> interface. The following example demonstrates a beveled line
/// join between a horizontal line and a vertical line:

procedure TDemoJoiningLines.Run;
var
  Path: IGPGraphicsPath;
  PenJoin: IGPPen;
begin
  Path := TGPGraphicsPath.Create;
  PenJoin := TGPPen.Create(TGPColor.Create(255, 0, 0, 255), 8);

  Path.StartFigure;
  Path.AddLine(TGPPoint.Create(50, 200), TGPPoint.Create(100, 200));
  Path.AddLine(TGPPoint.Create(100, 200), TGPPoint.Create(100, 250));

  PenJoin.LineJoin := LineJoinBevel;
  Graphics.DrawPath(PenJoin, Path);
end;

/// In the preceding example, the value (<B>LineJoinBevel</B>) passed to the
/// <A>LineJoin</A> property is an element of the <A>TLineJoin</A> enumeration.
{$ENDREGION}

initialization
  RegisterDemo('Using a Pen to Draw Lines and Shapes\Joining Lines', TDemoJoiningLines);

end.
