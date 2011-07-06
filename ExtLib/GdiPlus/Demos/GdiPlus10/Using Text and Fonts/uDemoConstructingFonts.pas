unit uDemoConstructingFonts;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoConstructingFonts = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoConstructingFonts }

{$REGION}
/// Microsoft Windows GDI+ provides several classes that form the foundation for
/// drawing text. The <A>IGPGraphics</A> interface has several <A>DrawString</A>
/// methods that allow you to specify various features of text, such as
/// location, bounding rectangle, font, and format. Other interfaces that
/// contribute to text rendering include <A>IGPFontFamily</A>, <A>IGPFont</A>,
/// <A>IGPStringFormat</A>, <A>IGPInstalledFontCollection</A>, and
/// <A>IGPPrivateFontCollection</A>.
///
/// Microsoft Windows GDI+ groups fonts with the same typeface but different
/// styles into font families. For example, the Arial font family contains the
/// following fonts:
///
///  -Arial Regular
///  -Arial Bold
///  -Arial Italic
///  -Arial Bold Italic
///
/// GDI+ uses four styles to form families: regular, bold, italic, and bold
/// italic. Adjectives such as narrow and rounded are not considered styles;
/// rather they are part of the family name. For example, Arial Narrow is a font
/// family whose members are the following:
///
///  -Arial Narrow Regular
///  -Arial Narrow Bold
///  -Arial Narrow Italic
///  -Arial Narrow Bold Italic
///
/// Before you can draw text with GDI+, you need to construct a <A>IGPFontFamily</A>
/// object and a <A>IGPFont</A> object. The <A>IGPFontFamily</A> objects specifies
/// the typeface (for example, Arial), and the <A>IGPFont</A> object specifies the
/// size, style, and units.
///
/// The following example constructs a regular style Arial font with a size of
/// 16 pixels:

procedure TDemoConstructingFonts.Run;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Brush: IGPBrush;
begin
  FontFamily := TGPFontFamily.Create('Arial');
  Font := TGPFont.Create(FontFamily, 16, FontStyleRegular, UnitPixel);
  Brush := TGPSolidBrush.Create(TGPColor.Black);
  Graphics.DrawString('The Quick Brown Fox Jumps Over The Lazy Dog', Font,
    TGPPointF.Create(0, 0), Brush);
end;

/// In the preceding code, the first argument passed to the <A>TGPFont</A>
/// constructor is the <A>IGPFontFamily</A> object. The second argument specifies
/// the size of the font measured in units identified by the fourth argument.
/// The third argument identifies the style.
///
/// UnitPixel is a member of the <A>TUnit</A> enumeration, and FontStyleRegular
/// an empty set of type <A>TFontStyle</A> enumeration.
{$ENDREGION}

initialization
  RegisterDemo('Using Text and Fonts\Constructing Font Families and Fonts', TDemoConstructingFonts);

end.
