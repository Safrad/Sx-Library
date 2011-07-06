unit uDemoCustomDashedLine;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoCustomDashedLine = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// Microsoft Windows GDI+ provides several dash styles that are listed in the
/// <A>TDashStyle</A> enumeration. If those standard dash styles don't suit your
/// needs, you can create a custom dash pattern.
///
/// To draw a custom dashed line, put the lengths of the dashes and spaces in an
/// array and pass the array as an argument to the <A>SetDashPattern</A> method
/// of a <A>IGPPen</A> object. The following example draws a custom dashed line
/// based on the array [5, 2, 15, 4]. If you multiply the elements of the array
/// by the pen width of 5, you get [25, 10, 75, 20].
/// The displayed dashes alternate in length between 25 and 75, and the spaces
/// alternate in length between 10 and 20.

procedure TDemoCustomDashedLine.Run;
const
  DashValues: array [0..3] of Single = (5, 2, 15, 4);
var
  BlackPen: IGPPen;
begin
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 5);
  BlackPen.SetDashPattern(DashValues);
  Graphics.DrawLine(BlackPen, TGPPoint.Create(5, 5), TGPPoint.Create(405, 5));
end;

/// Note that the final dash has to be shorter than 25 units so that the line
/// can end at (405, 5).
{$ENDREGION}

initialization
  RegisterDemo('Using a Pen to Draw Lines and Shapes\Drawing a Custom Dashed Line', TDemoCustomDashedLine);

end.
