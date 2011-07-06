unit uDemoAntialiasingText;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoAntialiasingText = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoAntialiasingText }

{$REGION}
/// Microsoft Windows GDI+ provides various quality levels for drawing text.
/// Typically, higher quality rendering takes more processing time than lower
/// quality rendering.
///
/// The quality level is a property of the <A>IGPGraphics</A> class. To set the
/// quality level, set the <A>TextRenderingHint</A> method of a <A>IGPGraphics</A>
/// object. The <A>TextRenderingHint</A> property receives one of the elements
/// of the <A>TTextRenderingHint</A> enumeration.
///
/// GDI+ provides traditional antialiasing and a new kind of antialiasing based
/// on Microsoft ClearType display technology only available on Windows XP and
/// Windows Server 2003 and later. ClearType smoothing improves readability on
/// color LCD monitors that have a digital interface, such as the monitors in
/// laptops and high-quality flat desktop displays. Readability on CRT screens
/// is also somewhat improved.
///
/// ClearType is dependent on the orientation and ordering of the LCD stripes.
/// Currently, ClearType is implemented only for vertical stripes that are
/// ordered RGB. This might be a concern if you are using a tablet PC, where the
/// display can be oriented in any direction, or if you are using a screen that
/// can be turned from landscape to portrait.
///
/// The following example draws text with two different quality settings.

procedure TDemoAntialiasingText.Run;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Brush: IGPSolidBrush;
  Rect: TGPRect;
begin
  FontFamily := TGPFontFamily.Create('Times New Roman');
  Font := TGPFont.Create(FontFamily, 32, FontStyleRegular, UnitPixel);

  Brush := TGPSolidBrush.Create(TGPColor.White);
  Rect.Initialize(10, 10, 250, 100);
  Graphics.FillRectangle(Brush, Rect);
  Brush.Color := TGPColor.Blue;

  Graphics.TextRenderingHint := TextRenderingHintSingleBitPerPixel;
  Graphics.DrawString('SingleBitPerPixel', Font, TGPPointF.Create(10, 10), Brush);

  Graphics.TextRenderingHint := TextRenderingHintAntiAlias;
  Graphics.DrawString('AntiAlias', Font, TGPPointF.Create(10, 60), Brush);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Text and Fonts\Antialiasing with Text', TDemoAntialiasingText);

end.
