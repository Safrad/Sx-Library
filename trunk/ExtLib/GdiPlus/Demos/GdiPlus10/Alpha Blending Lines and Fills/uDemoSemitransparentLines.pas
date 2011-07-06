unit uDemoSemitransparentLines;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoSemitransparentLines = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoSemitransparentLines }

{$REGION}
/// In Microsoft Windows GDI+, a color is a 32-bit value with 8 bits each for
/// alpha, red, green, and blue. The alpha value indicates the transparency of
/// the color — the extent to which the color is blended with the background
/// color. Alpha values range from 0 through 255, where 0 represents a fully
/// transparent color, and 255 represents a fully opaque color.
///
/// Alpha blending is a pixel-by-pixel blending of source and background color
/// data. Each of the three components (red, green, blue) of a given source
/// color is blended with the corresponding component of the background color
/// according to the following formula:
///
/// DisplayColor = SourceColor × Alpha / 255 + BackgroundColor × (255 – Alpha) / 255
///
/// For example, suppose the red component of the source color is 150 and the
/// red component of the background color is 100. If the alpha value is 200, the
/// red component of the resultant color is calculated as follows:
///
/// 150 × 200 / 255 + 100 × (255 – 200) / 255 = 139
///
/// When you draw a line, you must pass a <A>IGPPen</A> object to the
/// <A>DrawLine</A> method of the <A>IGPGraphics</A> class. One of the
/// parameters of the <A>TGPPen</A> constructor is a <A>TGPColor</A> record. To
/// draw an opaque line, set the alpha component of the color to 255. To draw a
/// semitransparent line, set the alpha component to any value from 1 through
/// 254.
///
/// When you draw a semitransparent line over a background, the color of the
/// line is blended with the colors of the background. The alpha component
/// specifies how the line and background colors are mixed; alpha values near 0
/// place more weight on the background colors, and alpha values near 255 place
/// more weigh on the line color.
///
/// The following example draws an image and then draws three lines that use the
/// image as a background. The first line uses an alpha component of 255, so it
/// is opaque. The second and third lines use an alpha component of 128, so they
/// are semitransparent; you can see the background image through the lines.
/// Setting the <A>CompositingQuality</A> property causes the blending for the
/// third line to be done in conjunction with gamma correction.

procedure TDemoSemitransparentLines.Run;
var
  Image: IGPImage;
  OpaquePen, SemiTransPen: IGPPen;
begin
  Image := TGPImage.Create('Texture1.jpg');
  Graphics.DrawImage(Image, 10, 5, Image.Width, Image.Height);
  OpaquePen := TGPPen.Create(TGPColor.Create(255, 0, 0, 255), 15);
  SemiTransPen := TGPPen.Create(TGPColor.Create(128, 0, 0, 255), 15);
  Graphics.DrawLine(OpaquePen, 0, 20, 100, 20);
  Graphics.DrawLine(SemiTransPen, 0, 40, 100, 40);
  Graphics.CompositingQuality := CompositingQualityGammaCorrected;
  Graphics.DrawLine(SemiTransPen, 0, 60, 100, 60);
end;
{$ENDREGION}

initialization
  RegisterDemo('Alpha Blending Lines and Fills\Drawing Opaque and Semitransparent Lines', TDemoSemitransparentLines);

end.
