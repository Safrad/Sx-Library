unit uDemoFormattingText;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoFormattingText = class(TDemo)
  strict private
    procedure AligningText;
    procedure SettingTabStops;
    procedure DrawingVerticalText;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoFormattingText }

{$REGION}
/// To apply special formatting to text, initialize a <A>IGPStringFormat</A>
/// object and pass that object to the <A>DrawString</A> method of the
/// <A>IGPGraphics</A> interface.
///
/// To draw formatted text in a rectangle, you need <A>IGPGraphics</A>,
/// <A>IGPFontFamily</A>, <A>IGPFont</A>, <A>TGPRectF</A>, <A>IGPStringFormat</A> and
/// <A>IGPBrush</A> objects.
///
/// <H>Aligning Text</H>
/// The following example draws text in a rectangle. Each line of text is
/// centered (side to side), and the entire block of text is centered (top to
/// bottom) in the rectangle.

procedure TDemoFormattingText.AligningText;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Rect: TGPRectF;
  StringFormat: IGPStringFormat;
  SolidBrush: IGPBrush;
  Pen: IGPPen;
begin
  FontFamily := TGPFontFamily.Create('Arial');
  Font := TGPFont.Create(FontFamily, 12, [FontStyleBold], UnitPoint);
  Rect.Initialize(30, 10, 120, 140);
  StringFormat := TGPStringFormat.Create;
  SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));

  // Center-justify each line of text.
  StringFormat.Alignment := StringAlignmentCenter;

  // Center the block of text (top to bottom) in the rectangle.
  StringFormat.LineAlignment := StringAlignmentCenter;

  Graphics.DrawString('Use IGPStringFormat and TGPRectF objects to center text in a rectangle.',
    Font, Rect, StringFormat, SolidBrush);

  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));
  Graphics.DrawRectangle(Pen, Rect);
end;

/// The preceding code sets two properties of the <A>IGPStringFormat</A> object:
/// <A>Alignment</A> and <A>LineAlignment</A>. The assignment to <A>Alignment</A>
/// specifies that each line of text is centered in the rectangle given by the
/// third argument passed to the <A>DrawString</A> method. The assignment to
/// <A>LineAlignment</A> specifies that the block of text is centered (top to
/// bottom) in the rectangle.
///
/// The value StringAlignmentCenter is an element of the <A>TStringAlignment</A>
/// enumeration.
///
/// <H>Setting Tab Stops</H>
/// You can set tab stops for text by calling the <A>SetTabStops</A> method of a
/// <A>IGPStringFormat</A> object and then passing that <A>IGPStringFormat</A>
/// object to the <A>DrawString</A> method of the <A>IGPGraphics</A> interface.
///
/// The following example sets tab stops at 150, 250, and 350. Then the code
/// displays a tabbed list of names and test scores.

procedure TDemoFormattingText.SettingTabStops;
const
  Tabs: array [0..2] of Single = (150, 100, 100);
  Str = 'Name'#9'Test 1'#9'Test 2'#9'Test 3'#13#10+
        'Joe'#9'95'#9'88'#9'91'#13#10+
        'Mary'#9'98'#9'84'#9'90'#13#10+
        'Sam'#9'42'#9'76'#9'98'#13#10+
        'Jane'#9'65'#9'73'#9'92';
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Rect: TGPRectF;
  StringFormat: IGPStringFormat;
  SolidBrush: IGPBrush;
  Pen: IGPPen;
begin
  FontFamily := TGPFontFamily.Create('Courier New');
  Font := TGPFont.Create(FontFamily, 12, FontStyleRegular, UnitPoint);
  Rect.Initialize(170, 10, 450, 100);
  StringFormat := TGPStringFormat.Create;
  SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));

  StringFormat.SetTabStops(0, Tabs);

  Graphics.DrawString(Str, Font, Rect, StringFormat, SolidBrush);

  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));
  Graphics.DrawRectangle(Pen, Rect);
end;

/// The preceding code passes three arguments to the <A>SetTabStops</A> method.
/// The second argument is an array containing the tab offsets. The first
/// argument passed to <A>SetTabStops</A> is 0, which indicates that the first
/// offset in the array is measured from position 0, the left edge of the
/// bounding rectangle.
///
/// <H>Drawing Vertical Text</H>
/// You can use a <A>IGPStringFormat</A> object to specify that text be drawn
/// vertically rather than horizontally.
///
/// The following example passes the value [StringFormatFlagsDirectionVertical]
/// to the <A>FormatFlags</A> property of a <A>IGPStringFormat</A> object. That
/// <A>IGPStringFormat</A> object is passed to the <A>IDrawString<A> method of the
/// <A>IGPGraphics</A> interface. The value StringFormatFlagsDirectionVertical is
/// an element of the <A>TStringFormatFlags</A> enumeration.

procedure TDemoFormattingText.DrawingVerticalText;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Point: TGPPointF;
  StringFormat: IGPStringFormat;
  SolidBrush: IGPBrush;
begin
  FontFamily := TGPFontFamily.Create('Lucida Console');
  Font := TGPFont.Create(FontFamily, 14, FontStyleRegular, UnitPoint);
  Point.Initialize(170, 120);
  StringFormat := TGPStringFormat.Create;
  SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));

  StringFormat.FormatFlags := [StringFormatFlagsDirectionVertical];

  Graphics.DrawString('Vertical text', Font, Point, StringFormat, SolidBrush);
end;
{$ENDREGION}

procedure TDemoFormattingText.Run;
begin
  AligningText;
  SettingTabStops;
  DrawingVerticalText;
end;

initialization
  RegisterDemo('Using Text and Fonts\Formatting Text', TDemoFormattingText);

end.
