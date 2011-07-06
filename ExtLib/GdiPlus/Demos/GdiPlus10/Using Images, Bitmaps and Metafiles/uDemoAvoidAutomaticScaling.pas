unit uDemoAvoidAutomaticScaling;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoAvoidAutomaticScaling = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// If you pass only the upper-left corner of an image to the <A>DrawImage</A>
/// method, Microsoft Windows GDI+ might scale the image, which would decrease
/// performance.
///
/// The following call to the <A>DrawImage</A> method specifies an upper-left
/// corner of (50, 30) but does not specify a destination rectangle:
///
/// <C>Graphics.DrawImage(Image, 50, 30);  // upper-left corner at (50, 30)</C>
///
/// Although this is the easiest version of the <A>DrawImage</A> method in terms
/// of the number of required arguments, it is not necessarily the most
/// efficient. If the number of dots per inch on the current display device is
/// different than the number of dots per inch on the device where the image was
/// created, GDI+ scales the image so that its physical size on the current
/// display device is as close as possible to its physical size on the device
/// where it was created.
///
/// If you want to prevent such scaling, pass the width and height of a
/// destination rectangle to the <A>DrawImage</A> method. The following example
/// draws the same image twice. In the first case, the width and height of the
/// destination rectangle are not specified, and the image is automatically
/// scaled. In the second case, the width and height (measured in pixels) of the
/// destination rectangle are specified to be the same as the width and height
/// of the original image.

procedure TDemoAvoidAutomaticScaling.Run;
var
  Image: IGPImage;
begin
  Image := TGPImage.Create('Texture.bmp');
  Graphics.DrawImage(Image, 10, 10);
  Graphics.DrawImage(Image, 120, 10, Image.Width, Image.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Improving Performance by Avoiding Automatic Scaling', TDemoAvoidAutomaticScaling);

end.
