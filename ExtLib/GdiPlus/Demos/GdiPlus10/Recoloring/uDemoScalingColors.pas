unit uDemoScalingColors;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoScalingColors = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoScalingColors }

{$REGION}
/// A scaling transformation multiplies one or more of the four color components
/// by a number. The color matrix entries that represent scaling are given in
/// the following table.
///
///  <B>Component to</B>
///  <B>be scaled</B><T><B>Matrix entry</B>
///  Red<T><T>[0,0]
///  Green<T><T>[1,1]
///  Blue<T><T>[2,2]
///  Alpha<T><T>[3,3]
///
/// The following example constructs an <A>IGPImage</A> object from the file
/// ColorBars2.bmp. Then the code scales the blue component of each pixel in the
/// image by a factor of 2. The original image is drawn alongside the
/// transformed image.

procedure TDemoScalingColors.Example1;
const
  ColorMatrix: TGPColorMatrix = (
    M: ((1.0, 0.0, 0.0, 0.0, 0.0),
        (0.0, 1.0, 0.0, 0.0, 0.0),
        (0.0, 0.0, 2.0, 0.0, 0.0),
        (0.0, 0.0, 0.0, 1.0, 0.0),
        (0.0, 0.0, 0.0, 0.0, 1.0)));
var
  Image: IGPImage;
  ImageAttributes: IGPImageAttributes;
  Width, Height: Integer;
begin
  Image := TGPImage.Create('ColorBars2.bmp');
  ImageAttributes := TGPImageAttributes.Create;
  Width := Image.Width;
  Height := Image.Height;

  ImageAttributes.SetColorMatrix(ColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
  Graphics.DrawImage(Image, 10, 10, Width, Height);
  Graphics.DrawImage(Image,
    150, 10, Width, Height, // destination rectangle
    0, 0, Width, Height,    // source rectangle
    UnitPixel, ImageAttributes);
end;

/// The top illustration above shows the original image on the left and the
/// transformed image on the right.
///
/// The following table shows the color vectors for the four bars before and
/// after the blue scaling. Note that the blue component in the fourth color bar
/// went from 0.8 to 0.6. That is because GDI+ retains only the fractional part
/// of the result. For example, (2)(0.8) = 1.6, and the fractional part of 1.6
/// is 0.6. Retaining only the fractional part ensures that the result is always
/// in the interval [0, 1].
///
///  <B>Original</B><T><T><B>Scaled</B>
///  (0.4, 0.4, 0.4, 1)<T><T>(0.4, 0.4, 0.8, 1)
///  (0.4, 0.2, 0.2, 1)<T><T>(0.4, 0.2, 0.4, 1)
///  (0.2, 0.4, 0.2, 1)<T><T>(0.2, 0.4, 0.4, 1)
///  (0.4, 0.4, 0.8, 1)<T><T>(0.4, 0.4, 0.6, 1)
///
/// The following example constructs an <A>IGPImage</A> object from the file
/// ColorBars3.bmp. Then the code scales the red, green, and blue components of
/// each pixel in the image. The red components are scaled down 25 percent, the
/// green components are scaled down 35 percent, and the blue components are
/// scaled down 50 percent.

procedure TDemoScalingColors.Example2;
const
  ColorMatrix: TGPColorMatrix = (
    M: ((0.75, 0.0 , 0.0, 0.0, 0.0),
        (0.0 , 0.65, 0.0, 0.0, 0.0),
        (0.0 , 0.0 , 0.5, 0.0, 0.0),
        (0.0 , 0.0 , 0.0, 1.0, 0.0),
        (0.0 , 0.0 , 0.0, 0.0, 1.0)));
var
  Image: IGPImage;
  ImageAttributes: IGPImageAttributes;
  Width, Height: Integer;
begin
  Image := TGPImage.Create('ColorBars3.bmp');
  ImageAttributes := TGPImageAttributes.Create;
  Width := Image.Width;
  Height := Image.Height;

  ImageAttributes.SetColorMatrix(ColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
  Graphics.DrawImage(Image, 10, 150, Width, Height);
  Graphics.DrawImage(Image,
    150, 150, Width, Height, // destination rectangle
    0, 0, Width, Height,    // source rectangle
    UnitPixel, ImageAttributes);
end;

/// The bottom illustration above shows the original image on the left and the
/// transformed image on the right.
///
/// The following table shows the color vectors for the four bars before and
/// after the red, green and blue scaling.
///
///  <B>Original</B><T><T><B>Scaled</B>
///  (0.6, 0.6, 0.6, 1)<T><T>(0.45, 0.39, 0.3, 1)
///  (0, 1, 1, 1)<T><T>(0, 0.65, 0.5, 1)
///  (1, 1, 0, 1)<T><T>(0.75, 0.65, 0, 1)
///  (1, 0, 1, 1)<T><T>(0.75, 0, 0.5, 1)
{$ENDREGION}

procedure TDemoScalingColors.Run;
begin
  Example1;
  Example2;
end;

initialization
  RegisterDemo('Recoloring\Scaling Colors', TDemoScalingColors);

end.
