unit uDemoFillHatchPattern;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoFillHatchPattern = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// A hatch pattern is made from two colors: one for the background and one for
/// the lines that form the pattern over the background. To fill a closed shape
/// with a hatch pattern, use a <A>IGPHatchBrush</A> object. The following example
/// demonstrates how to fill an ellipse with a hatch pattern:

procedure TDemoFillHatchPattern.Run;
var
  Brush: IGPHatchBrush;
begin
  Brush := TGPHatchBrush.Create(HatchStyleHorizontal, TGPColor.Create(255, 255, 0, 0),
    TGPColor.Create(255, 128, 255, 255));
  Graphics.FillEllipse(Brush, 0, 0, 100, 60);
end;

/// The <A>TGPHatchBrush</A> constructor takes three arguments: the hatch style,
/// the color of the hatch line, and the color of the background. The hatch
/// style argument can be any element of the <A>THatchStyle</A> enumeration.
/// There are more than fifty elements in the <A>THatchStyle</A> enumeration; a
/// few of those elements are shown in the following list:
///
///  - <B>HatchStyleHorizontal</B>
///  - <B>HatchStyleVertical</B>
///  - <B>HatchStyleForwardDiagonal</B>
///  - <B>HatchStyleBackwardDiagonal</B>
///  - <B>HatchStyleCross</B>
///  - <B>HatchStyleDiagonalCross</B>
{$ENDREGION}

initialization
  RegisterDemo('Using a Brush to Fill Shapes\Filling a Shape with a Hatch Pattern', TDemoFillHatchPattern);

end.
