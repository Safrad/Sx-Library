unit uDemoLineTexture;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoLineTexture = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// Instead of drawing a line or curve with a solid color, you can draw with a
/// texture. To draw lines and curves with a texture, create a
/// <A>IGPTextureBrush</A> object, and pass that <A>IGPTextureBrush</A> object to a
/// <A>TGPPen</A> constructor. The image associated with the texture brush is used
/// to tile the plane (invisibly), and when the pen draws a line or curve, the
/// stroke of the pen uncovers certain pixels of the tiled texture.
///
/// The following example creates an <A>IGPImage</A> object from the file
/// Texture1.jpg. That image is used to construct a <A>IGPTextureBrush</A> object,
/// and the <A>IGPTextureBrush</A> object is used to construct a <A>IGPPen</A>
/// object. The call to <A>IGPGraphics.DrawImage</A> draws the image with its
/// upper-left corner at (0, 0). The call to <A>Graphics.DrawEllipse</A> uses
/// the <A>IGPPen</A> object to draw a textured ellipse.

procedure TDemoLineTexture.Run;
var
  Image: IGPImage;
  Brush: IGPTextureBrush;
  TexturedPen: IGPPen;
begin
  Image := TGPImage.Create('Texture1.jpg');
  Brush := TGPTextureBrush.Create(Image);
  TexturedPen := TGPPen.Create(Brush, 30);

  Graphics.DrawImage(Image, 0, 0, Image.Width, Image.Height);
  Graphics.DrawEllipse(TexturedPen, 100, 20, 200, 100);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using a Pen to Draw Lines and Shapes\Drawing a Line with a Texture', TDemoLineTexture);

end.
