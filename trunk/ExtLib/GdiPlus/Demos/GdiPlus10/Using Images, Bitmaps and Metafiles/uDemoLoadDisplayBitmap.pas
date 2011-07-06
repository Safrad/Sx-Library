unit uDemoLoadDisplayBitmap;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoLoadDisplayBitmap = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// Microsoft Windows GDI+ provides the <A>IGPImage</A> interface for working with
/// raster images (bitmaps) and vector images (metafiles). The <A>IGPBitmap</A>
/// interface and the <A>IGPMetafile</A> interface both inherit from the
/// <A>IGPImage</A> interface. The <A>IGPBitmap</A> interface expands on the
/// capabilities of the <A>IGPImage</A> interface by providing additional methods
/// for loading, saving, and manipulating raster images. The <A>IGPMetafile</A>
/// interface expands on the capabilities of the <A>IGPImage</A> interface by
/// providing additional methods for recording and examining vector images.
///
/// To display a raster image (bitmap) on the screen, you need an <A>IGPImage</A>
/// object and a <A>IGPGraphics</A> object. Pass the name of a file (or a pointer
/// to a stream) to an <A>TGPImage</A> constructor. After you have created an
/// <A>IGPImage</A> object, pass that <A>IGPImage</A> object to the <A>DrawImage</A>
/// method of a <A>IGPGraphics</A> object.
///
/// The following example creates an <A>IGPImage</A> object from a JPEG file and
/// then draws the image with its upper-left corner at (60, 10):

procedure TDemoLoadDisplayBitmap.Run;
var
  Image: IGPImage;
begin
  Image := TGPImage.Create('Grapes.jpg');
  Graphics.DrawImage(Image, 60, 10);
end;

/// The <A>IGPImage</A> interface provides basic methods for loading and
/// displaying raster images and vector images. The <A>IGPBitmap</A> interface,
/// which inherits from the <A>IGPImage</A> interface, provides more specialized
/// methods for loading, displaying, and manipulating raster images. For
/// example, you can construct a <A>IGPBitmap</A> object from an icon handle
/// (HICON).
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Loading and Displaying Bitmaps', TDemoLoadDisplayBitmap);

end.
