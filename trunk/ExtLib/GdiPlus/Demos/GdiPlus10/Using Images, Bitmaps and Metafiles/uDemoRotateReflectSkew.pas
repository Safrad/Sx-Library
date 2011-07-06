unit uDemoRotateReflectSkew;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoRotateReflectSkew = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// You can rotate, reflect, and skew an image by specifying destination points
/// for the upper-left, upper-right, and lower-left corners of the original
/// image. The three destination points determine an affine transformation that
/// maps the original rectangular image to a parallelogram. (The lower-right
/// corner of the original image is mapped to the fourth corner of the
/// parallelogram, which is calculated from the three specified destination
/// points.)
///
/// For example, suppose the original image is a rectangle with upper-left
/// corner at (0, 0), upper-right corner at (100, 0), and lower-left corner at
/// (0, 50). Now suppose we map those three points to destination points as
/// in the following code:

procedure TDemoRotateReflectSkew.Run;
const
  DestinationPoints: TGPPlgPoints = (
    (X: 200; Y:  20),  // destination for upper-left point of original
    (X: 110; Y: 100),  // destination for upper-right point of original
    (X: 250; Y:  30)); // destination for lower-left point of original
var
  Image: IGPImage;
begin
  Image := TGPImage.Create('Stripes.bmp');
  // Draw the image unaltered with its upper-left corner at (0, 0).
  Graphics.DrawImage(Image, 0, 0);
  // Draw the image mapped to the parallelogram.
  Graphics.DrawImage(Image, DestinationPoints);
end;

/// The illustration shows the original image and the image mapped to the
/// parallelogram. The original image has been skewed, reflected, rotated, and
/// translated. The x-axis along the top edge of the original image is mapped
/// to the line that runs through (200, 20) and (110, 100). The y-axis along the
/// left edge of the original image is mapped to the line that runs through
/// (200, 20) and (250, 30).
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Rotating, Reflecting and Skewing Images', TDemoRotateReflectSkew);

end.
