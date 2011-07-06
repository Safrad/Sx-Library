unit uDemoSemitransparentBrushes;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoSemitransparentBrushes = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoSemitransparentBrushes }

{$REGION}
/// When you fill a shape, you must pass a <A>IGPBrush</A> object to one of the
/// fill methods of the <A>IGPGraphics</A> interface. The one parameter of the
/// <A>TGPSolidBrush</A> constructor is a <A>TGPColor</A> record.
/// To fill an opaque shape, set the alpha component of the color to 255. To
/// fill a semitransparent shape, set the alpha component to any value from
/// 1 through 254.
///
/// When you fill a semitransparent shape, the color of the shape is blended
/// with the colors of the background. The alpha component specifies how the
/// shape and background colors are mixed; alpha values near 0 place more weight
/// on the background colors, and alpha values near 255 place more weigh on the
/// shape color.
///
/// The following example draws an image and then fills three ellipses that
/// overlap the image. The first ellipse uses an alpha component of 255, so it
/// is opaque. The second and third ellipses use an alpha component of 128, so
/// they are semitransparent; you can see the background image through the
/// ellipses. Setting the <A>IGPGraphics.CompositingQuality</A> property causes
/// the blending for the third ellipse to be done in conjunction with gamma
/// correction.

procedure TDemoSemitransparentBrushes.Run;
var
  Image: IGPImage;
  OpaqueBrush, SemiTransBrush: IGPSolidBrush;
begin
  Image := TGPImage.Create('Texture1.jpg');
  Graphics.DrawImage(Image, 50, 50, Image.Width, Image.Height);
  OpaqueBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));
  SemiTransBrush := TGPSolidBrush.Create(TGPColor.Create(128, 0, 0, 255));
  Graphics.FillEllipse(OpaqueBrush, 35, 45, 45, 30);
  Graphics.FillEllipse(SemiTransBrush, 86, 45, 45, 30);
  Graphics.CompositingQuality := CompositingQualityGammaCorrected;
  Graphics.FillEllipse(SemiTransBrush, 40, 90, 86, 30);
end;
{$ENDREGION}

initialization
  RegisterDemo('Alpha Blending Lines and Fills\Drawing with Opaque and Semitransparent Brushes', TDemoSemitransparentBrushes);

end.
