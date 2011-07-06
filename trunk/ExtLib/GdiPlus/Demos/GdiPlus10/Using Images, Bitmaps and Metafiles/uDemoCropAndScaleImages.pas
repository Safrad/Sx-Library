unit uDemoCropAndScaleImages;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoCropAndScaleImages = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// The <A>IGPGraphics</A> interface provides several <A>DrawImage</A> methods,
/// some of which have source and destination rectangle parameters that you can
/// use to crop and scale images.
///
/// The following example constructs an <A>IGPImage</A> object from the file
/// Apple.gif. The code draws the entire apple image in its original size. The
/// code then calls the <A>DrawImage</A> method of a <A>IGPGraphics</A> object to
/// draw a portion of the apple image in a destination rectangle that is larger
/// than the original apple image.
///
/// The <A>DrawImage</A> method determines which portion of the apple to draw by
/// looking at the source rectangle, which is specified by the third, fourth,
/// fifth, and sixth arguments. In this case, the apple is cropped to 75 percent
/// of its width and 75 percent of its height.
///
/// The <A>DrawImage</A> method determines where to draw the cropped apple and
/// how big to make the cropped apple by looking at the destination rectangle,
/// which is specified by the second argument. In this case, the destination
/// rectangle is 30 percent wider and 30 percent taller than the original image.

procedure TDemoCropAndScaleImages.Run;
var
  Image: IGPImage;
  Width, Height: Integer;
  DestinationRect: TGPRectF;
begin
  Image := TGPImage.Create('Apple.gif');
  Width := Image.Width;
  Height := Image.Height;

  // Make the destination rectangle 30 percent wider and
  // 30 percent taller than the original image.
  // Put the upper-left corner of the destination
  // rectangle at (150, 20).
  DestinationRect.Initialize(150, 20, 1.3 * Width, 1.3 * Height);

  // Draw the image unaltered with its upper-left corner at (0, 0).
  Graphics.DrawImage(Image, 0, 0);

  // Draw a portion of the image. Scale that portion of the image
  // so that it fills the destination rectangle.
  Graphics.DrawImage(Image, DestinationRect,
    0, 0,          // upper-left corner of source rectangle
    0.75 * Width,  // width of source rectangle
    0.75 * Height, // height of source rectangle
    UnitPixel);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Cropping and Scaling Images', TDemoCropAndScaleImages);

end.
