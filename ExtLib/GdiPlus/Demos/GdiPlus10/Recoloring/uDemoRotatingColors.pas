unit uDemoRotatingColors;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoRotatingColors = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoRotatingColors }

{$REGION}
/// Rotation in a four-dimensional color space is difficult to visualize. We can
/// make it easier to visualize rotation by agreeing to keep one of the color
/// components fixed. Suppose we agree to keep the alpha component fixed at 1
/// (fully opaque). Then we can visualize a three-dimensional color space with
/// red, green, and blue axes as shown in the following illustration.
///
///  <I>See the Platform SDK topic "Rotating Colors" for the illustration</I>
///
/// A color can be thought of as a point in 3-D space. For example, the point
/// (1, 0, 0) in space represents the color red, and the point (0, 1, 0) in
/// space represents the color green.
///
/// The following illustration shows what it means to rotate the color (1, 0, 0)
/// through an angle of 60 degrees in the Red-Green plane. Rotation in a plane
/// parallel to the Red-Green plane can be thought of as rotation about the blue
/// axis.
///
///  <I>See the Platform SDK topic "Rotating Colors" for the illustration</I>
///
/// The following illustration shows how to initialize a color matrix to perform
/// rotations about each of the three coordinate axes (red, green, blue).
///
///  <I>See the Platform SDK topic "Rotating Colors" for the illustration</I>
///
/// The following example takes an image that is all one color (1, 0, 0.6) and
/// applies a 60-degree rotation about the blue axis. The angle of the rotation
/// is swept out in a plane that is parallel to the Red-Green plane.

procedure TDemoRotatingColors.Run;
const
  Degrees = 60;
  Radians = Degrees * Pi / 180;
var
  Bitmap: IGPBitmap;
  BitmapGraphics: IGPGraphics;
  ImageAttributes: IGPImageAttributes;
  ColorMatrix: TGPColorMatrix;
begin
  Bitmap := TGPBitmap.Create(100, 100);
  BitmapGraphics := TGPGraphics.Create(Bitmap);
  BitmapGraphics.Clear(TGPColor.Create(255, 204, 0, 153));
  ImageAttributes := TGPImageAttributes.Create;

  ColorMatrix.SetToIdentity;
  ColorMatrix.M[0,0] := Cos(Radians);
  ColorMatrix.M[1,0] := -Sin(Radians);
  ColorMatrix.M[0,1] := Sin(Radians);
  ColorMatrix.M[1,1] := Cos(Radians);

  ImageAttributes.SetColorMatrix(ColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
  Graphics.DrawImage(Bitmap, 10, 10, 100, 100);
  Graphics.DrawImage(Bitmap,
    150, 10, 100, 100, // destination rectangle
    0, 0, 100, 100,    // source rectangle
    UnitPixel, ImageAttributes);
end;

/// The illustration above shows the original image on the left and the
/// transformed image on the right.
///
/// The color rotation performed in the preceding code example can be visualized
/// as follows.
///
///  <I>See the Platform SDK topic "Rotating Colors" for the illustration</I>
{$ENDREGION}

initialization
  RegisterDemo('Recoloring\Rotating Colors', TDemoRotatingColors);

end.
