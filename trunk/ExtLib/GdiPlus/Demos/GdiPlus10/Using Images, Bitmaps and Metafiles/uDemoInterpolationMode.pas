unit uDemoInterpolationMode;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoInterpolationMode = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// The interpolation mode of a <A>IGPGraphics</A> object influences the way
/// Microsoft Windows GDI+ scales (stretches and shrinks) images. The
/// <A>TInterpolationMode</A> enumeration defines several interpolation modes,
/// some of which are shown in the following list:
///
///  - InterpolationModeNearestNeighbor
///  - InterpolationModeBilinear
///  - InterpolationModeHighQualityBilinear
///  - InterpolationModeBicubic
///  - InterpolationModeHighQualityBicubic
///
/// To stretch an image, each pixel in the original image must be mapped to a
/// group of pixels in the larger image. To shrink an image, groups of pixels in
/// the original image must be mapped to single pixels in the smaller image. The
/// effectiveness of the algorithms that perform these mappings determines the
/// quality of a scaled image. Algorithms that produce higher-quality scaled
/// images tend to require more processing time. In the preceding list,
/// <B>InterpolationModeNearestNeighbor</B> is the lowest-quality mode and
/// <B>InterpolationModeHighQualityBicubic</B> is the highest-quality mode.
///
/// To set the interpolation mode, pass one of the members of the
/// <A>TInterpolationMode</A> enumeration to the <A>InterpolationMode</A>
/// property of a <A>IGPGraphics</A> object.
///
/// The following example draws an image and then shrinks the image with three
/// different interpolation modes:

procedure TDemoInterpolationMode.Run;
var
  Image: IGPImage;
  Width, Height: Integer;
begin
  Image := TGPImage.Create('GrapeBunch.bmp');
  Width := Image.Width;
  Height := Image.Height;

  // Draw the image with no shrinking or stretching.
  Graphics.DrawImage(Image,
    TGPRect.Create(10, 10, Width, Height), // destination rectangle
    0, 0,      // upper-left corner of source rectangle
    Width,     // width of source rectangle
    Height,    // height of source rectangle
    UnitPixel);

  // Shrink the image using low-quality interpolation.
  Graphics.InterpolationMode := InterpolationModeNearestNeighbor;
  Graphics.DrawImage(Image,
    TGPRectF.Create(10, 250, 0.6 * Width, 0.6 * Height), // destination rectangle
    0, 0,      // upper-left corner of source rectangle
    Width,     // width of source rectangle
    Height,    // height of source rectangle
    UnitPixel);

  // Shrink the image using medium-quality interpolation.
  Graphics.InterpolationMode := InterpolationModeHighQualityBilinear;
  Graphics.DrawImage(Image,
    TGPRectF.Create(150, 250, 0.6 * Width, 0.6 * Height), // destination rectangle
    0, 0,      // upper-left corner of source rectangle
    Width,     // width of source rectangle
    Height,    // height of source rectangle
    UnitPixel);

  // Shrink the image using high-quality interpolation.
  Graphics.InterpolationMode := InterpolationModeHighQualityBicubic;
  Graphics.DrawImage(Image,
    TGPRectF.Create(290, 250, 0.6 * Width, 0.6 * Height), // destination rectangle
    0, 0,      // upper-left corner of source rectangle
    Width,     // width of source rectangle
    Height,    // height of source rectangle
    UnitPixel);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Using Interpolation Mode to Control Image Quality During Scaling', TDemoInterpolationMode);

end.
