unit uDemoCompositingMode;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoCompositingMode = class(TDemo)
  strict private
    procedure DrawShapes(const CompositingMode: TGPCompositingMode;
      const YOffset: Integer);
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoCompositingMode }

{$REGION}
/// There might be times when you want to create an off-screen bitmap that has
/// the following characteristics:
///
///  -Colors have alpha values that are less than 255.
///  -Colors are not alpha blended with each other as you create the bitmap.
///  -When you display the finished bitmap, colors in the bitmap are alpha
/// blended with the background colors on the display device.
///
/// To create such a bitmap, construct a blank <A>IGPBitmap</A> object, and then
/// construct a <A>IGPGraphics</A> object based on that bitmap. Set the
/// compositing mode of the <A>IGPGraphics</A> object to CompositingModeSourceCopy.
///
/// The following example creates a <A>IGPGraphics</A> object based on a
/// <A>IGPBitmap</A> object. The code uses the <A>IGPGraphics</A> object along with
/// two semitransparent brushes (alpha = 160) to paint on the bitmap. The code
/// fills a red ellipse and a green ellipse using the semitransparent brushes.
/// The green ellipse overlaps the red ellipse, but the green is not blended
/// with the red because the compositing mode of the <A>IGPGraphics</A> object is
/// set to CompositingModeSourceCopy.
///
/// The code draws the bitmap on the screen twice: once on a white background
/// and once on a multicolored background. The pixels in the bitmap that are
/// part of the two ellipses have an alpha component of 160, so the ellipses are
/// blended with the background colors on the screen.

procedure TDemoCompositingMode.DrawShapes(
  const CompositingMode: TGPCompositingMode; const YOffset: Integer);
var
  Bitmap: IGPBitmap;
  BitmapGraphics: IGPGraphics;
  RedBrush, GreenBrush, Brush: IGPSolidBrush;
begin
  // Create a blank bitmap.
  Bitmap := TGPBitmap.Create(180, 100);
  // Create a IGPGraphics object that can be used to draw on the bitmap.
  BitmapGraphics := TGPGraphics.Create(Bitmap);
  // Create a red brush and a green brush, each with an alpha value of 160.
  RedBrush := TGPSolidBrush.Create(TGPColor.Create(160, 255, 0, 0));
  GreenBrush := TGPSolidBrush.Create(TGPColor.Create(160, 0, 255, 0));
  // Set the compositing mode using the parameter. When CompositingMode is set
  // to CompositingModeSourceCopy, then when overlapping ellipses are drawn,
  // the colors of the ellipses are not blended.
  BitmapGraphics.CompositingMode := CompositingMode;
  BitmapGraphics.FillEllipse(RedBrush, 0, 0, 150, 70);
  BitmapGraphics.FillEllipse(GreenBrush, 30, 30, 150, 70);

  // Draw a multicolored background on the screen.
  Graphics.CompositingQuality := CompositingQualityGammaCorrected;
  Brush := TGPSolidBrush.Create(TGPColor.Aqua);
  Graphics.FillRectangle(Brush, 200, YOffset, 60, 100);
  Brush.Color := TGPColor.Yellow;
  Graphics.FillRectangle(Brush, 260, YOffset, 60, 100);
  Brush.Color := TGPColor.Fuchsia;
  Graphics.FillRectangle(Brush, 320, YOffset, 60, 100);

  // Display the bitmap on a white background.
  Graphics.DrawImage(Bitmap, 0, YOffset);
  // Display the bitmap on a multicolored background.
  Graphics.DrawImage(Bitmap, 200, YOffset);
end;

/// The following code calls the routine above 2 times with different compositing
/// modes. Setting the compositing mode to CompositingModeSourceOver causes the
/// ellipses to be blended with each other as well as with the background.

procedure TDemoCompositingMode.Run;
begin
  DrawShapes(CompositingModeSourceCopy, 0);
  DrawShapes(CompositingModeSourceOver, 110);
end;
{$ENDREGION}

initialization
  RegisterDemo('Alpha Blending Lines and Fills\Using Compositing Mode to Control Alpha Blending', TDemoCompositingMode);

end.
