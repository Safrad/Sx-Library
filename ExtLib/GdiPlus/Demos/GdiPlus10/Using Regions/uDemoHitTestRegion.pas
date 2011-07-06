unit uDemoHitTestRegion;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoHitTestRegion = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoHitTestRegion }

{$REGION}
/// The Microsoft Windows GDI+ <A>IGPRegion</A> interface allows you to define a
/// custom shape. The shape can be made up of lines, polygons, and curves.
///
/// Two common uses for regions are hit testing and clipping. Hit testing is
/// determining whether the mouse was clicked in a certain region of the screen.
/// Clipping is restricting drawing to a certain region.
///
/// The purpose of hit testing is to determine whether the cursor is over a
/// given object, such as an icon or a button. The following example creates a
/// plus-shaped region by forming the union of two rectangular regions. Assume
/// that the parameter <I>MousePoint</I> holds the location of the most recent
/// click. The code checks to see whether <I>MousePoint</I> is in the
/// plus-shaped region. If <I>MousePoint</I> is in the region (a hit), the
/// region is filled with an opaque red brush. Otherwise, the region is filled
/// with a semitransparent red brush.

procedure TDemoHitTestRegion.Run;

  procedure HitTest(const RegionOffset, MousePoint: TGPPoint);
  var
    Brush: IGPSolidBrush;
    Region1, Region2: IGPRegion;
  begin
    Brush := TGPSolidBrush.Create(0);
    // Create a plus-shaped region by forming the union of Region1 and Region2.
    Region1 := TGPRegion.Create(TGPRect.Create(RegionOffset.X + 50, RegionOffset.Y, 50, 150));
    Region2 := TGPRegion.Create(TGPRect.Create(RegionOffset.X, RegionOffset.Y + 50, 150, 50));
    // The union replaces Region1.
    Region1.Union(Region2);

    if (Region1.IsVisible(MousePoint, Graphics)) then
      Brush.Color := TGPColor.Create(255, 255, 0, 0)
    else
      Brush.Color := TGPColor.Create(64, 255, 0, 0);
    Graphics.FillRegion(Brush, Region1);

    // Draw MousePoint for reference
    Brush.Color := TGPColor.Blue;
    Graphics.FillRectangle(Brush, MousePoint.X - 2, MousePoint.Y - 2, 5, 5);
  end;

begin
  HitTest(TGPPoint.Create(0, 0), TGPPoint.Create(60, 10));
  HitTest(TGPPoint.Create(200, 0), TGPPoint.Create(220, 20));
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Regions\Hit Testing with a Region', TDemoHitTestRegion);

end.
