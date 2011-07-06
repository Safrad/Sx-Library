unit uDemoGraphicsState;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoGraphicsState = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
    procedure Example3;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoGraphicsState }

{$REGION}
/// A <A>IGPGraphics</A> object provides methods such as <A>DrawLine</A>,
/// <A>DrawImage</A>, and <A>DrawString</A> for displaying vector images, raster
/// images, and text. A <A>IGPGraphics</A> object also has several properties that
/// influence the quality and orientation of the items that are drawn. For
/// example, the smoothing mode property determines whether antialiasing is
/// applied to lines and curves, and the world transformation property
/// influences the position and rotation of the items that are drawn.
///
/// A <A>IGPGraphics</A> object is often associated with a particular display
/// device. When you use a <A>IGPGraphics</A> object to draw in a window, the
/// <A>IGPGraphics</A> object is also associated with that particular window.
///
/// A <A>IGPGraphics</A> object can be thought of as a container because it holds
/// a set of properties that influence drawing, and it is linked to
/// device-specific information. You can create a secondary container within an
/// existing <A>IGPGraphics</A> object by calling the <A>BeginContainer</A> method
/// of that <A>IGPGraphics</A> object.
///
/// <H>Graphics State</H>
/// A <A>IGPGraphics</A> object does more than provide drawing methods, such as
/// <A>DrawLine</A> and <A>DrawRectangle</A>. A <A>IGPGraphics</A> object also
/// maintains graphics state, which can be divided into the following categories:
///
///  -A link to a device context
///  -Quality settings
///  -Transformations
///  -A clipping region
///
/// <H>Device Context</H>
/// As an application programmer, you don't have to think about the interaction
/// between a <A>IGPGraphics</A> object and its device context. This interaction
/// is handled by GDI+ behind the scenes.
///
/// <H>Quality Settings</H>
/// A <A>IGPGraphics</A> object has several properties that influence the quality
/// of the items that are drawn on the screen. You can view and manipulate these
/// properties by calling get and set methods. For example, you can set the
/// <A>TextRenderingHint</A> property to specify the type of antialiasing (if
/// any) applied to text. Other set properties that influence quality are
/// <A>SmoothingMode</A>, <A>CompositingMode</A>, <A>CompositingQuality</A>, and
/// <A>InterpolationMode</A>.
///
/// The following example draws two ellipses, one with the smoothing mode set to
/// <A>SmoothingModeAntiAlias</A> and one with the smoothing mode set to
/// <A>SmoothingModeHighSpeed</A>.

procedure TDemoGraphicsState.Example1;
var
  Pen: IGPPen;
begin
  Pen := TGPPen.Create(TGPColor.Lime, 3);
  Graphics.SmoothingMode := SmoothingModeAntiAlias;
  Graphics.DrawEllipse(Pen, 0, 0, 200, 100);
  Graphics.SmoothingMode := SmoothingModeHighSpeed;
  Graphics.DrawEllipse(Pen, 0, 150, 200, 100);
end;

/// The green ellipsis in the illustration above show the result.
///
/// <H>Transformations</H>
/// A <A>IGPGraphics</A> object maintains two transformations (world and page)
/// that are applied to all items drawn by that <A>IGPGraphics</A> object. Any
/// affine transformation can be stored in the world transformation. Affine
/// transformations include scaling, rotating, reflecting, skewing, and
/// translating. The page transformation can be used for scaling and for
/// changing units (for example, pixels to inches). For more information on
/// transformations, see Coordinate Systems and Transformations in the
/// Platform SDK.
///
/// The following example sets the world and page transformations of a
/// <A>IGPGraphics</A> object. The world transformation is set to a 30-degree
/// rotation. The page transformation is set so that the coordinates passed to
/// the second <A>DrawEllipse</A> will be treated as millimeters instead of
/// pixels. The code makes two identical calls to the <A>DrawEllipse</A> method.
/// The world transformation is applied to the first <A>DrawEllipse</A> call,
/// and both transformations (world and page) are applied to the second
/// <A>DrawEllipse</A> call.

procedure TDemoGraphicsState.Example2;
var
  Pen: IGPPen;
begin
  Pen := TGPPen.Create(TGPColor.Red);

  Graphics.ResetTransform;
  Graphics.RotateTransform(30);             // World transformation
  Graphics.DrawEllipse(Pen, 30, 0, 50, 25);
  Graphics.PageUnit := UnitMillimeter;      // Page transformation
  Graphics.DrawEllipse(Pen, 30, 0, 50, 25);
end;

/// The red ellipsis in the illustration above show the result. Note that the
/// 30-degree rotation is about the origin of the coordinate system (upper-left
/// corner of the client area), not about the centers of the ellipses. Also note
/// that the pen width of 1 means 1 pixel for the first ellipse and 1 millimeter
/// for the second ellipse.
///
/// <H>Clipping Region</H>
/// A <A>IGPGraphics</A> object maintains a clipping region that applies to all
/// items drawn by that <A>IGPGraphics</A> object. You can set the clipping region
/// by calling the <A>SetClip</A> method or setting the <A>Clip</A> property.
///
/// The following example creates a plus-shaped region by forming the union of
/// two rectangles. That region is designated as the clipping region of a
/// <A>IGPGraphics</A> object. Then the code draws two lines that are restricted
/// to the interior of the clipping region.

procedure TDemoGraphicsState.Example3;
var
  Pen: IGPPen;
  Brush: IGPBrush;
  Region: IGPRegion;
begin
  Pen := TGPPen.Create(TGPColor.Red, 5);
  Brush := TGPSolidBrush.Create(TGPColor.Create(255, 180, 255, 255));

  // Create a plus-shaped region by forming the union of two rectangles.
  Region := TGPRegion.Create(TGPRect.Create(300, 0, 50, 150));
  Region.Union(TGPRect.Create(250, 50, 150, 50));
  Graphics.FillRegion(Brush, Region);

  // Set the clipping region.
  Graphics.Clip := Region;

  // Draw two clipped lines.
  Graphics.DrawLine(Pen, 250, 30, 400, 160);
  Graphics.DrawLine(Pen, 290, 20, 440, 150);
end;
{$ENDREGION}

procedure TDemoGraphicsState.Run;
begin
  Example1;
  Example2;
  Graphics.PageUnit := UnitPixel;
  Graphics.ResetTransform;
  Example3;
end;

initialization
  RegisterDemo('Using Graphics Containers\The State of a Graphics Object', TDemoGraphicsState);

end.
