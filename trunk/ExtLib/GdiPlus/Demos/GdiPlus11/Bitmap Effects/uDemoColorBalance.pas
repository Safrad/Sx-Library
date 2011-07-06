unit uDemoColorBalance;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoColorBalance = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoColorBalance }

{$REGION}
/// The ColorBalance class enables you to change the color balance (relative
/// amounts of red, green, and blue) of a bitmap. The effect has 3 parameters:
///  1. CyanRed (-100..100): If the value is 0, there is no change. As the value
/// moves from 0 to 100, the amount of red in the image increases and the amount
/// of cyan decreases. As the value moves from 0 to -100, the amount of red in
/// the image decreases and the amount of cyan increases.
///  2. MagentaGreen (-100..100): If the value is 0, there is no change. As the
/// value moves from 0 to 100, the amount of green in the image increases and the
/// amount of magenta decreases. As the value moves from 0 to -100, the amount of
/// green in the image decreases and the amount of magenta increases.
///  3. YellowBlue (-100..100): If the value is 0, there is no change. As the
/// value moves from 0 to 100, the amount of blue in the image increases and the
/// amount of yellow decreases. As the value moves from 0 to -100, the amount of
/// blue in the image decreases and the amount of yellow increases.

procedure TDemoColorBalance.Run;
var
  Bitmap: IGPBitmap;
  ColorBalance: IGPColorBalance;
  Params: TGPColorBalanceParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Color Balance effect
  ColorBalance := TGPColorBalance.Create;
  Params.CyanRed := 20;
  Params.MagentaGreen := 0;
  Params.YellowBlue := -20;
  ColorBalance.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(ColorBalance);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Color Balance Effect', TDemoColorBalance);

end.
