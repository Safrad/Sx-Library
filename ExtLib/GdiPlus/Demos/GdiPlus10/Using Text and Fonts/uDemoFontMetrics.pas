unit uDemoFontMetrics;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoFontMetrics = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoFontMetrics }

class function TDemoFontMetrics.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

{$REGION}
/// The <A>IGPFontFamily</A> interface provides the following methods that
/// retrieve various metrics for a particular family/style combination:
///
///  -<A>GetEmHeight</A>(FontStyle)
///  -<A>GetCellAscent</A>(FontStyle)
///  -<A>GetCellDescent</A>(FontStyle)
///  -<A>GetLineSpacing</A>(FontStyle)
///
/// The numbers returned by these methods are in font design units, so they are
/// independent of the size and units of a particular <A>IGPFont</A> object.
///
/// The following example displays the metrics for the regular style of the
/// Arial font family.

procedure TDemoFontMetrics.Run;
var
  FontFamily: IGPFontFamily;
  Font: IGPFont;
  Ascent, Descent, LineSpacing: Integer;
  AscentPixel, DescentPixel, LineSpacingPixel: Single;
begin
  FontFamily := TGPFontFamily.Create('Arial');
  Font := TGPFont.Create(FontFamily, 16, FontStyleRegular, UnitPixel);

  TextOutput.Add(Format('Font.Size returns %g.', [Font.Size]));
  TextOutput.Add(Format('FontFamily.GetEmHeight returns %d.',
    [FontFamily.GetEmHeight(FontStyleRegular)]));

  Ascent := FontFamily.GetCellAscent(FontStyleRegular);
  AscentPixel := Font.Size * Ascent / FontFamily.GetEmHeight(FontStyleRegular);
  TextOutput.Add(Format('The ascent is %d design units, %g pixels.',
    [Ascent, AscentPixel]));

  Descent := FontFamily.GetCellDescent(FontStyleRegular);
  DescentPixel := Font.Size * Descent / FontFamily.GetEmHeight(FontStyleRegular);
  TextOutput.Add(Format('The decent is %d design units, %g pixels.',
    [Descent, DescentPixel]));

  LineSpacing := FontFamily.GetLineSpacing(FontStyleRegular);
  LineSpacingPixel := Font.Size * LineSpacing / FontFamily.GetEmHeight(FontStyleRegular);
  TextOutput.Add(Format('The line spacing is %d design units, %g pixels.',
    [LineSpacing, LineSpacingPixel]));
end;

/// Note the first two lines of output above. The <A>IGPFont</A> object returns a
/// size of 16, and the <A>IGPFontFamily</A> object returns an em height of 2,048.
/// These two numbers (16 and 2,048) are the key to converting between font
/// design units and the units (in this case pixels) of the <A>IGPFont</A> object.
{$ENDREGION}

initialization
  RegisterDemo('Using Text and Fonts\Obtaining Font Metrics', TDemoFontMetrics);

end.
