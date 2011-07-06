unit uDemoTranslatingColors;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoTranslatingColors = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoTranslatingColors }

{$REGION}
/// A translation adds a value to one or more of the four color components. The
/// color matrix entries that represent translations are given in the following
/// table.
///
///  <B>Component to</B>
///  <B>be translated</B><T><B>Matrix entry</B>
///  Red<T><T>[4,0]
///  Green<T><T>[4,1]
///  Blue<T><T>[4,2]
///  Alpha<T><T>[4,2]
///
/// The following example constructs an <A>IGPImage</A> object from the file
/// ColorBars.bmp. Then the code adds 0.75 to the red component of each pixel in
/// the image. The original image is drawn alongside the transformed image.

procedure TDemoTranslatingColors.Run;
const
  ColorMatrix: TGPColorMatrix = (
    M: ((1.0 , 0.0, 0.0, 0.0, 0.0),
        (0.0 , 1.0, 0.0, 0.0, 0.0),
        (0.0 , 0.0, 1.0, 0.0, 0.0),
        (0.0 , 0.0, 0.0, 1.0, 0.0),
        (0.75, 0.0, 0.0, 0.0, 1.0)));
var
  Image: IGPImage;
  ImageAttributes: IGPImageAttributes;
  Width, Height: Integer;
begin
  Image := TGPImage.Create('ColorBars.bmp');
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
/// The following table lists the color vectors for the four bars before and
/// after the red translation. Note that because the maximum value for a color
/// component is 1, the red component in the second row does not change.
/// (Similarly, the minimum value for a color component is 0.)
///
///  <B>Original</B><T><T><B>Translated</B>
///  Black (0, 0, 0, 1)<T>(0.75, 0, 0, 1)
///  Red (1, 0, 0, 1)<T><T>(1, 0, 0, 1)
///  Green (0, 1, 0, 1)<T>(0.75, 1, 0, 1)
///  Blue (0, 0, 1, 1)<T><T>(0.75, 0, 1, 1)
{$ENDREGION}

initialization
  RegisterDemo('Recoloring\Translating Colors', TDemoTranslatingColors);

end.
