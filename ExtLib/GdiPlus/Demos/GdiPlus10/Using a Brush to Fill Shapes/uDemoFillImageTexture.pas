unit uDemoFillImageTexture;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoFillImageTexture = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// You can fill a closed shape with a texture by using the <A>IGPImage</A>
/// interface and the <A>IGPTextureBrush</A> interface.
///
/// The following example fills an ellipse with an image. The code constructs an
/// <A>IGPImage</A> object, and then passes that <A>IGPImage</A> object as an
/// argument to a <A>TGPTextureBrush</A> constructor. The third code statement
/// scales the image, and the fourth statement fills the ellipse with
/// repeated copies of the scaled image:

procedure TDemoFillImageTexture.Run;
var
  Image: IGPImage;
  Brush: IGPTextureBrush;
begin
  Image := TGPImage.Create('ImageFile.jpg');
  Brush := TGPTextureBrush.Create(Image);
  Brush.Transform := TGPMatrix.Create(75 / 640, 0, 0, 75/480, 0, 0);
  Graphics.FillEllipse(Brush, TGPRect.Create(0, 50, 150, 250));
end;

/// In the preceding code example, the <A>Transform</A> property sets the
/// transformation that is applied to the image before it is drawn. Assume that
/// the original image has a width of 640 pixels and a height of 480 pixels. The
/// transform shrinks the image to 75×75, by setting the horizontal and
/// vertical scaling values.
///
/// <B>Note</B> In the preceding example, the image size is 75×75, and the
/// ellipse size is 150×250. Because the image is smaller than the ellipse it
/// is filling, the ellipse is tiled with the image. Tiling means that the image
/// is repeated horizontally and vertically until the boundary of the shape is
/// reached. For more information on tiling, see the next demo
/// <I>Tiling a Shape with an Image</I>.
{$ENDREGION}

initialization
  RegisterDemo('Using a Brush to Fill Shapes\Filling a Shape with an Image Texture', TDemoFillImageTexture);

end.
