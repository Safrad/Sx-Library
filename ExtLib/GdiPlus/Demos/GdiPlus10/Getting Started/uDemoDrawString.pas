unit uDemoDrawString;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoDrawString = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
procedure TDemoDrawString.Run;
var
  Brush: IGPSolidBrush;
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Point: TGPPointF;
begin
  Brush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));
  FontFamily := TGPFontFamily.Create('Times New Roman');
  Font := TGPFont.Create(FontFamily, 24, FontStyleRegular, UnitPixel);
  Point.Initialize(10, 20);
  Graphics.DrawString('Hello World', Font, Point, Brush);
end;

/// The previous code creates several GDI+ objects. The <A>IGPGraphics</A> object
/// provides the <A>IGPGraphics.DrawString</A> method, which does the actual
/// drawing. The <A>IGPSolidBrush</A> object specifies the color of the string.
///
/// The <A>IGPFontFamily</A> constructor receives a single, string argument that
/// identifies the font family. The <A>IGPFontFamily</A> object is the first
/// argument passed to the <A>TGPFont</A> constructor. The second argument passed
/// to the <A>TGPFont</A> constructor specifies the font size, and the third
/// argument specifies the style. The value <B>FontStyleRegular</B> is a member
/// of the <A>TFontStyle</A> enumeration. The last argument to the <A>TGPFont</A>
/// constructor indicates that the size of the font (24 in this case) is
/// measured in pixels. The value <B>UnitPixel</B> is a member of the
/// <A>TUnit</A> enumeration.
///
/// The first argument passed to the <A>IGPGraphics.DrawString</A> method is a
/// wide-character string. The second argument is the <A>IGPFont</A> object. The
/// third argument is a reference to a <A>TGPPointF</A> record that specifies the
/// location where the string will be drawn. The last argument is the
/// <A>IGPBrush</A> object, which specifies the color of the string.

{$ENDREGION}

initialization
  RegisterDemo('Getting Started\Drawing a String', TDemoDrawString);

end.
