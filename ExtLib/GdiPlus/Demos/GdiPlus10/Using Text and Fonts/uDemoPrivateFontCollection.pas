unit uDemoPrivateFontCollection;

interface

uses
  Windows,
  SysUtils,
  GdiPlus,
  uDemo;

type
  TDemoPrivateFontCollection = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoPrivateFontCollection }

{$REGION}
/// The <A>IGPPrivateFontCollection</A> interface inherits from the
/// <A>IFontCollection</A> abstract base interface. You can use a
/// <A>IGPPrivateFontCollection</A> object to maintain a set of fonts specifically
/// for your application. A private font collection can include installed system
/// fonts as well as fonts that have not been installed on the computer. To add
/// a font file to a private font collection, call the <A>AddFontFile</A> method
/// of a <A>IGPPrivateFontCollection</A> object.
///
/// The <A>Families</A> property of a <A>IGPPrivateFontCollection</A> object
/// returns an array of <A>IGPFontFamily</A> objects.
///
/// The number of font families in a private font collection is not necessarily
/// the same as the number of font files that have been added to the collection.
/// For example, suppose you add the files ArialBd.tff, Times.tff, and
/// TimesBd.tff to a collection. There will be three files but only two families
/// in the collection because Times.tff and TimesBd.tff belong to the same
/// family.
///
/// The following example adds the following three font files to a PrivateFontCollection object:
///
///  -<WinDir>\Fonts\Arial.tff (Arial, regular)
///  -<WinDir>\Fonts\CourBI.tff (Courier New, bold italic)
///  -<WinDir>\Fonts\TimesBd.tff (Times New Roman, bold)
///
/// For each <A>IGPFontFamily</A> object in the collection, the code calls the
/// <A>IsStyleAvailable</A> method to determine whether various styles (regular,
/// bold, italic, bold italic, underline, and strikeout) are available. The
/// argument passed to the <A>IsStyleAvailable</A> method are members of the
/// <A>TFontStyle</A> enumeration.
///
/// If a given family/style combination is available, a <A>IGPFont</A> object is
/// constructed using that family and style. The first argument passed to the
/// <A>TGPFont</A> constructor is the font family name (not a <A>IGPFontFamily</A>
/// object as is the case for other variations of the <A>TGPFont</A> constructor),
/// and the final argument is the <A>IGPPrivateFontCollection</A> object. After
/// the <A>IGPFont</A> object is constructed, it is passed to the <A>DrawString</A>
/// method of the <A>IGPGraphics</A> class to display the family name along with
/// the name of the style.

procedure TDemoPrivateFontCollection.Run;
var
  WinDir: array [0..MAX_PATH] of Char;
  FontDir, FamilyName: String;
  Collection: IGPPrivateFontCollection;
  Family: IGPFontFamily;
  Font: IGPFont;
  Point: TGPPointF;
  Brush: IGPBrush;
begin
  GetWindowsDirectory(WinDir, MAX_PATH);
  FontDir := IncludeTrailingPathDelimiter(WinDir) + 'Fonts' + PathDelim;
  Collection := TGPPrivateFontCollection.Create;
  Collection.AddFontFile(FontDir + 'Arial.ttf');
  Collection.AddFontFile(FontDir + 'CourBI.ttf');
  Collection.AddFontFile(FontDir + 'TimesBd.ttf');
  Point.Initialize(10, 0);
  Brush := TGPSolidBrush.Create(TGPColor.Black);

  for Family in Collection.Families do
  begin
    FamilyName := Family.FamilyName;

    // Is the regular style available?
    if (Family.IsStyleAvailable(FontStyleRegular)) then
    begin
      Font := TGPFont.Create(FamilyName, 16, FontStyleRegular, UnitPixel, Collection);
      Graphics.DrawString(FamilyName + ' Regular', Font, Point, Brush);
      Point.Y := Point.Y + Font.GetHeight(0);
    end;

    // Is the bold style available?
    if (Family.IsStyleAvailable([FontStyleBold])) then
    begin
      Font := TGPFont.Create(FamilyName, 16, [FontStyleBold], UnitPixel, Collection);
      Graphics.DrawString(FamilyName + ' Bold', Font, Point, Brush);
      Point.Y := Point.Y + Font.GetHeight(0);
    end;

    // Is the italic style available?
    if (Family.IsStyleAvailable([FontStyleItalic])) then
    begin
      Font := TGPFont.Create(FamilyName, 16, [FontStyleItalic], UnitPixel, Collection);
      Graphics.DrawString(FamilyName + ' Italic', Font, Point, Brush);
      Point.Y := Point.Y + Font.GetHeight(0);
    end;

    // Is the bold italic style available?
    if (Family.IsStyleAvailable([FontStyleBold, FontStyleItalic])) then
    begin
      Font := TGPFont.Create(FamilyName, 16, [FontStyleBold, FontStyleItalic], UnitPixel, Collection);
      Graphics.DrawString(FamilyName + ' Bold Italic', Font, Point, Brush);
      Point.Y := Point.Y + Font.GetHeight(0);
    end;

    // Is the underline style available?
    if (Family.IsStyleAvailable([FontStyleUnderline])) then
    begin
      Font := TGPFont.Create(FamilyName, 16, [FontStyleUnderline], UnitPixel, Collection);
      Graphics.DrawString(FamilyName + ' Underline', Font, Point, Brush);
      Point.Y := Point.Y + Font.GetHeight(0);
    end;

    // Is the strokeout style available?
    if (Family.IsStyleAvailable([FontStyleStrikeout])) then
    begin
      Font := TGPFont.Create(FamilyName, 16, [FontStyleStrikeout], UnitPixel, Collection);
      Graphics.DrawString(FamilyName + ' Strkeout', Font, Point, Brush);
      Point.Y := Point.Y + Font.GetHeight(0);
    end;

    // Separate the families with white space.
    Point.Y := Point.Y + 10;
  end;
end;

/// Arial.tff (which was added to the private font collection in the preceding
/// code example) is the font file for the Arial regular style. Note, however,
/// that the program output shows several available styles other than regular
/// for the Arial font family. That is because Microsoft Windows GDI+ can
/// simulate the bold, italic, and bold italic styles from the regular style.
/// GDI+ can also produce underlines and strikeouts from the regular style.
///
/// Similarly, GDI+ can simulate the bold italic style from either the bold
/// style or the italic style. The program output shows that the bold italic
/// style is available for the Times family even though TimesBd.tff (Times New
/// Roman, bold) is the only Times file in the collection.
{$ENDREGION}

initialization
  RegisterDemo('Using Text and Fonts\Creating a Private Font Collection', TDemoPrivateFontCollection);

end.
