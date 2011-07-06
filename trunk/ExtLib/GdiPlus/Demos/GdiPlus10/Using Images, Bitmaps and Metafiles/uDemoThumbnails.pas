unit uDemoThumbnails;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoThumbnails = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// A thumbnail image is a small version of an image. You can create a thumbnail
/// image by calling the <A>GetThumbnailImage</A> method of an <A>IGPImage</A>
/// object.
///
/// The following example constructs an <A>IGPImage</A> object from the file
/// ImageFile.jpg. The original image has a width of 640 pixels and a height of
/// 480 pixels. The code creates a thumbnail image that has a width of 100
/// pixels and a height of 100 pixels.

procedure TDemoThumbnails.Run;
var
  Image, Thumbnail: IGPImage;
begin
  Image := TGPImage.Create('ImageFile.jpg');
  Thumbnail := Image.GetThumbnailImage(100, 100, nil, nil);
  Graphics.DrawImage(Thumbnail, 10, 10, Thumbnail.Width, Thumbnail.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Creating Thumbnail Images', TDemoThumbnails);

end.
