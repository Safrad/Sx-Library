unit uDemoShearingColors;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoShearingColors = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoShearingColors }

{$REGION}
/// Shearing increases or decreases a color component by an amount proportional
/// to another color component. For example, consider the transformation where
/// the red component is increased by one half the value of the blue component.
/// Under such a transformation, the color (0.2, 0.5, 1) would become
/// (0.7, 0.5, 1). The new red component is 0.2 + (1/2)(1) = 0.7.
///
/// The following example constructs an <A>IGPImage</A> object from the file
/// ColorBars4.bmp. Then the code applies the shearing transformation described
/// in the preceding paragraph to each pixel in the image.

procedure TDemoShearingColors.Run;
const
  ColorMatrix: TGPColorMatrix = (
    M: ((1.0, 0.0, 0.0, 0.0, 0.0),
        (0.0, 1.0, 0.0, 0.0, 0.0),
        (0.5, 0.0, 1.0, 0.0, 0.0),
        (0.0, 0.0, 0.0, 1.0, 0.0),
        (0.0, 0.0, 0.0, 0.0, 1.0)));
var
  Image: IGPImage;
  ImageAttributes: IGPImageAttributes;
  Width, Height: Integer;
begin
  Image := TGPImage.Create('ColorBars4.bmp');
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

/// The illustration above shows the original image on the left and the
/// transformed image on the right.
///
/// The following table shows the color vectors for the four bars before and
/// after the shearing transformation.
///
///  <B>Original</B><T><T><B>Sheared</B>
///  (0, 0, 1, 1)<T><T>(0.5, 0, 1, 1)
///  (0.5, 1, 0.5, 1)<T><T>(0.75, 1, 0.5, 1)
///  (1, 1, 0, 1)<T><T>(1, 1, 0, 1)
///  (0.4, 0.4, 0.4, 1)<T><T>(0.6, 0.4, 0.4, 1)
{$ENDREGION}

initialization
  RegisterDemo('Recoloring\Shearing Colors', TDemoShearingColors);

end.
