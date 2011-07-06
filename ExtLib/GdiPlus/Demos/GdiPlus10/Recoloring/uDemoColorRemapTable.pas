unit uDemoColorRemapTable;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoColorRemapTable = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoColorRemapTable }

{$REGION}
/// Remapping is the process of converting the colors in an image according to a
/// color remap table. The color remap table is an array of <A>TGPColorMap</A>
/// records. Each <A>TGPColorMap</A> record in the array has an <B>OldColor</B>
/// member and a <B>NewColor</B> member.
///
/// When GDI+ draws an image, each pixel of the image is compared to the array
/// of old colors. If a pixel's color matches an old color, its color is changed
/// to the corresponding new color. The colors are changed only for rendering —
/// the color values of the image itself (stored in an Image or Bitmap object)
/// are not changed.
///
/// To draw a remapped image, initialize an array of <A>TGPColorMap</A> records.
/// Pass that array to the <A>SetRemapTable</A> method of an
/// <A>IGPImageAttributes</A> object, and then pass the <A>IGPImageAttributes</A>
/// object to the <A>IGPGraphics.DrawImage</A> method of a <A>IGPGraphics</A> object.
///
/// The following example creates an <A>IGPImage</A> object from the file
/// RemapInput.bmp. The code creates a color remap table that consists of a
/// single <A>TGPColorMap</A> record. The <B>OldColor</B> member of the
/// <A>TGPColorMap</A> record is red, and the <B>NewColor</B> member is blue. The
/// image is drawn once without remapping and once with remapping. The remapping
/// process changes all the red pixels to blue.

procedure TDemoColorRemapTable.Run;
var
  Image: IGPImage;
  ImageAttributes: IGPImageAttributes;
  Width, Height: Integer;
  ColorMap: array [0..0] of TGPColorMap;
begin
  Image := TGPImage.Create('RemapInput.bmp');
  ImageAttributes := TGPImageAttributes.Create;
  Width := Image.Width;
  Height := Image.Height;

  ColorMap[0].OldColor := TGPColor.Red;
  ColorMap[0].NewColor := TGPColor.Blue;
  ImageAttributes.SetRemapTable(ColorMap, ColorAdjustTypeBitmap);

  Graphics.DrawImage(Image, 10, 10, Width, Height);
  Graphics.DrawImage(Image,
    150, 10, Width, Height, // destination rectangle
    0, 0, Width, Height,    // source rectangle
    UnitPixel, ImageAttributes);
end;

/// The illustration above shows the original image on the left and the remapped
/// image on the right.
{$ENDREGION}

initialization
  RegisterDemo('Recoloring\Using a Color Remap Table', TDemoColorRemapTable);

end.
