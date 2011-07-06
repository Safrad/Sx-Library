unit uDemoHueSaturationLightness;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoHueSaturationLightness = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoHueSaturationLightness }

{$REGION}
/// The <A>IGPHueSaturationLightness</A> interface enables you to change the hue,
/// saturation, and lightness of a bitmap. The effect has 3 parameters:
///  1. HueLevel (-180..180): A value of 0 specifies no change. Positive values
/// specify counterclockwise rotation on the color wheel. Negative values
/// specify clockwise rotation on the color wheel.
///  2. SaturationLevel (-100..100): A value of 0 specifies no change. Positive
/// values specify increased saturation and negative values specify decreased
/// saturation.
///  3. LightnessLevel (-100.100): A value of 0 specifies no change. Positive
/// values specify increased lightness and negative values specify decreased
/// lightness.

procedure TDemoHueSaturationLightness.Run;
var
  Bitmap: IGPBitmap;
  HueSaturationLightness: IGPHueSaturationLightness;
  Params: TGPHueSaturationLightnessParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Hue, Saturation & Lightness effect
  HueSaturationLightness := TGPHueSaturationLightness.Create;
  Params.HueLevel := -130;
  Params.SaturationLevel := -20;
  Params.LightnessLevel := 0;
  HueSaturationLightness.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(HueSaturationLightness);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Hue, Saturation & Lightness', TDemoHueSaturationLightness);

end.
