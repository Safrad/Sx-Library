unit uDemoLoadDisplayMetafile;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoLoadDisplayMetafile = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// The <A>IGPImage</A> interface provides basic methods for loading and
/// displaying raster images and vector images. The <A>IGPMetafile</A> interface,
/// which inherits from the <A>IGPImage</A> class, provides more specialized
/// methods for recording, displaying, and examining vector images.
///
/// To display a vector image (metafile) on the screen, you need an
/// <A>IGPImage</A> object and a <A>IGPGraphics</A> object. Pass the name of a file
/// (or a pointer to a stream) to an <A>TGPImage</A> constructor. After you have
/// created an <A>IGPImage</A> object, pass that <A>IGPImage</A> object to the
/// <A>DrawImage</A> method of a <A>IGPGraphics</A> object.
///
/// The following example creates an <A>IGPImage</A> object from an EMF (enhanced
/// metafile) file and then draws the image with its upper-left corner at
/// (60, 10):

procedure TDemoLoadDisplayMetafile.Run;
var
  Image: IGPImage;
begin
  Image := TGPImage.Create('SampleMetafile.emf');
  Graphics.DrawImage(Image, 60, 10);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Loading and Displaying Metafiles', TDemoLoadDisplayMetafile);

end.
