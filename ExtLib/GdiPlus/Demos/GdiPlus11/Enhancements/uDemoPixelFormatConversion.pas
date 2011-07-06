unit uDemoPixelFormatConversion;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoPixelFormatConversion = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoPixelFormatConversion }

{$REGION}
/// GDI+ 1.1 provides the <A>IGPBitmap.ConvertFormat</A> method to convert a
/// bitmap to a different pixel format. For example, you can convert a bitmap
/// from a True Color image (24 bits per pixel) to an indexed image (4 or 8
/// bits per pixel) with a color palette.
///
/// When you create an indexed version of bitmap, you usually want to create an
/// optimal color palette that represents the colors in the bitmap as accurate
/// as possible. You can create such a palette by calling the clas static
/// <A>TGPBitmap.InitializePalette</A> method. You specify how many colors you
/// want in the palette, how the palette should be generated (use
/// <A>PaletteTypeOptimal</A> for an optimal palette) and the bitmap the palette
/// should be created for. The method returns an <A>IGPColorPalette</A> object.
///
/// You pass the <A>IGPColorPalette</A> to the <A>IGPBitmap.ConvertFormat</A> method
/// along with some other settings such as the requested pixel format and the
/// type of dithering that should be used.
///
/// The example below loads a bitmap from a file and creates 3 reduced-color
/// versions of it. The first two reduced-color versions result in a 16 color
/// bitmap. The first version uses solid dithering (<A>DitherTypeSolid</A>) and
/// the second version uses diffusion dithering (<A>DitherTypeErrorDiffusion</A>)
/// As you can see in the results above, diffusion dithering usually leads to
/// the best results.
/// The final version is a 256 color conversion (8 bits per pixel) that uses
/// diffusion dithering too. You can see that this bitmap more closely matches
/// the original one.

procedure TDemoPixelFormatConversion.Run;
var
  Bitmap: IGPBitmap;
  Palette: IGPColorPalette;
begin
  Bitmap := TGPBitmap.Create('ImageFileSmall.jpg');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Convert to a 16-color bitmap (4 bits per pixel) using solid dithering
  Palette := TGPBitmap.InitializePalette(16, PaletteTypeOptimal, 16, False, Bitmap);
  Bitmap.ConvertFormat(PixelFormat4bppIndexed, DitherTypeSolid, PaletteTypeOptimal, Palette, 0);
  Graphics.DrawImage(Bitmap, 330, 0, Bitmap.Width, Bitmap.Height);

  // Convert to a 16-color bitmap (4 bits per pixel) using diffusion dithering
  Bitmap := TGPBitmap.Create('ImageFileSmall.jpg');
  Palette := TGPBitmap.InitializePalette(16, PaletteTypeOptimal, 16, False, Bitmap);
  Bitmap.ConvertFormat(PixelFormat4bppIndexed, DitherTypeErrorDiffusion, PaletteTypeOptimal, Palette, 0);
  Graphics.DrawImage(Bitmap, 0, 210, Bitmap.Width, Bitmap.Height);

  // Convert to a 256-color bitmap (8 bits per pixel) using diffusion dithering
  Bitmap := TGPBitmap.Create('ImageFileSmall.jpg');
  Palette := TGPBitmap.InitializePalette(256, PaletteTypeOptimal, 256, False, Bitmap);
  Bitmap.ConvertFormat(PixelFormat8bppIndexed, DitherTypeErrorDiffusion, PaletteTypeOptimal, Palette, 0);
  Graphics.DrawImage(Bitmap, 330, 210, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Enhancements\Convert Pixel Format for Bitmaps', TDemoPixelFormatConversion);

end.
