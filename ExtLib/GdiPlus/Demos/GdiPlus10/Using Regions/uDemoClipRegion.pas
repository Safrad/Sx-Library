unit uDemoClipRegion;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoClipRegion = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoClipRegion }

{$REGION}
/// One of the properties of the <A>IGPGraphics</A> interface is the clipping
/// region. All drawing done by a given <A>IGPGraphics</A> object is restricted to
/// the clipping region of that <A>IGPGraphics</A> object. You can set the
/// clipping region by calling the <A>SetClip</A> method or setting the
/// <A>Clip</A> property.
///
/// The following example constructs a path that consists of a single polygon.
/// Then the code constructs a region based on that path. The region is
/// assigned to the <A>Clip</A> property of the <A>IGPGraphics</A> object, and
/// then two strings are drawn.

procedure TDemoClipRegion.Run;
const
  PolyPoints: array [0..3] of TGPPoint = (
    (X: 10; Y: 10), (X: 150; Y: 10), (X: 100; Y: 75), (X: 100; Y: 150));
var
  Path: IGPGraphicsPath;
  Region: IGPRegion;
  Pen: IGPPen;
  Font: IGPFont;
  Brush: IGPBrush;
begin
  Path := TGPGraphicsPath.Create;
  Path.AddPolygon(PolyPoints);
  Region := TGPRegion.Create(Path);
  Pen := TGPPen.Create(TGPColor.Black);
  Graphics.DrawPath(Pen, Path);

  Graphics.Clip := Region;
  Font := TGPFont.Create('Arial', 36, [FontStyleBold], UnitPixel);
  Brush := TGPSolidBrush.Create(TGPColor.Red);
  Graphics.DrawString('A Clipping Region', Font, TGPPointF.Create(15, 25), Brush);
  Graphics.DrawString('A Clipping Region', Font, TGPPointF.Create(15, 68), Brush);
end;

/// The illustration above shows the clipped strings.
{$ENDREGION}

initialization
  RegisterDemo('Using Regions\Clipping with a Region', TDemoClipRegion);

end.
