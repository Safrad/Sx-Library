unit uDemoDrawingText;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoDrawingText = class(TDemo)
  strict private
    procedure DrawAtLocation;
    procedure DrawInRectangle;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoDrawingText }

{$REGION}
/// You can use the <A>DrawString</A> method of the <A>IGPGraphics</A> class to
/// draw text at a specified location or within a specified rectangle.
///
/// <H>Drawing Text at a Specified Location</H>
/// To draw text at a specified location, you need <A>IGPGraphics</A>,
/// <A>IGPFontFamily</A>, <A>IGPFont</A>, <A>TGPPointF</A>, and <A>IGPBrush</A> objects.
///
/// The following example draws the string "Hello" at location (30, 10). The
/// font family is Times New Roman. The font, which is an individual member of
/// the font family, is Times New Roman, size 24 pixels, regular style.

procedure TDemoDrawingText.DrawAtLocation;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Point: TGPPointF;
  SolidBrush: IGPBrush;
begin
  FontFamily := TGPFontFamily.Create('Times New Roman');
  Font := TGPFont.Create(FontFamily, 24, FontStyleRegular, UnitPixel);
  Point.Initialize(30, 10);
  SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));
  Graphics.DrawString('Hello', Font, Point, SolidBrush);
end;

/// In the preceding example, the <A>TGPFontFamily</A> constructor receives a
/// string that identifies the font family. The <A>IGPFontFamily</A> object is
/// passed as the first argument to the <A>TGPFont</A> constructor. The second
/// argument passed to the <A>TGPFont</A> constructor specifies the size of the
/// font measured in units given by the fourth argument. The third argument
/// specifies the style (regular, bold, italic, and so on) of the font.
///
/// The <A>DrawString</A> method receives four arguments. The first argument is
/// the string to be drawn. The second argument is the <A>IGPFont</A> object that
/// was constructed previously. The third argument is a <A>TGPPointF</A> record
/// that contains the coordinates of the upper-left corner of the string. The
/// fourth argument is a <A>IGPBrush</A> object that will be used to fill the
/// characters of the string.
///
/// <H>Drawing Text in a Rectangle</H>
/// One of the <A>DrawString</A> methods of the <A>IGPGraphics</A> interface has a
/// <A>TGPRectF</A> parameter. By calling that <A>DrawString</A> method, you can
/// draw text that wraps in a specified rectangle. To draw text in a rectangle,
/// you need <A>IGPGraphics</A>, <A>IGPFontFamily</A>, <A>IGPFont</A>, <A>TGPRectF</A>
/// and <A>IGPBrush</A> objects.
///
/// The following example creates a rectangle with upper-left corner (30, 50),
/// width 100, and height 122. Then the code draws a string inside that
/// rectangle. The string is restricted to the rectangle and wraps in such a way
/// that individual words are not broken.

procedure TDemoDrawingText.DrawInRectangle;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Rect: TGPRectF;
  SolidBrush: IGPBrush;
  Pen: IGPPen;
begin
  FontFamily := TGPFontFamily.Create('Arial');
  Font := TGPFont.Create(FontFamily, 12, [FontStyleBold], UnitPoint);
  Rect.Initialize(30, 50, 100, 122);
  SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));
  Graphics.DrawString('Draw text in a rectangle by passing a TGPRectF to the DrawString method',
    Font, Rect, nil, SolidBrush);

  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));
  Graphics.DrawRectangle(Pen, Rect);
end;

/// In the preceding example, the third argument passed to the <A>DrawString</A>
/// method is a <A>TGPRectF</A> record that specifies the bounding rectangle for
/// the text. The fourth parameter is of type <A>IGPStringFormat</A> — the
/// argument is nil because no special string formatting is required.
{$ENDREGION}

procedure TDemoDrawingText.Run;
begin
  DrawAtLocation;
  DrawInRectangle;
end;

initialization
  RegisterDemo('Using Text and Fonts\Drawing Text', TDemoDrawingText);

end.
