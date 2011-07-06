unit uDemoFillOpenFigures;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoFillOpenFigures = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoFillOpenFigures }

{$REGION}
/// You can fill a path by passing a <A>IGPGraphicsPath</A> object to the
/// <A>IGPGraphics.FillPath</A> method. The <A>FillPath</A> method fills the path
/// according to the fill mode (alternate or winding) currently set for the
/// path. If the path has any open figures, the path is filled as if those
/// figures were closed. GDI+ closes a figure by drawing a straight line from
/// its ending point to its starting point.
///
/// The following example creates a path that has one open figure (an arc) and
/// one closed figure (an ellipse). The <A>IGPGraphics.FillPath</A> method fills
/// the path according to the default fill mode, which is
/// <A>FillModeAlternate</A>.

procedure TDemoFillOpenFigures.Run;
var
  Path: IGPGraphicsPath;
  Pen: IGPPen;
  Brush: IGPBrush;
begin
  Path := TGPGraphicsPath.Create;

  // Add an open figure.
  Path.AddArc(0, 0, 150, 120, 30, 120);

  // Add an intrinsically closed figure.
  Path.AddEllipse(50, 50, 50, 100);

  Pen := TGPPen.Create(TGPColor.Create(128, 0, 0, 255), 5);
  Brush := TGPSolidBrush.Create(TGPColor.Red);

  Graphics.FillPath(Brush, Path);
  Graphics.DrawPath(Pen, Path);
end;

/// The illustration above the output of the preceding code. Note that path is
/// filled (according to <A>FillModeAlternate</A>) as if the open figure were
/// closed by a straight line from its ending point to its starting point.
{$ENDREGION}

initialization
  RegisterDemo('Constructing and Drawing Paths\Filling Open Figures', TDemoFillOpenFigures);

end.
