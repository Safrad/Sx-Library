unit uDemoBrightnessContrast;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoBrightnessContrast = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoBrightnessContrast }

{$REGION}
/// The <A>IGPBrightnessContrast</A> interface enables you to change the
/// brightness and contrast of a bitmap. The effect has 2 parameters:
///  1. BrightnessLevel (-255..255): If the value is 0, the brightness remains
/// the same. As the value moves from 0 to 255, the brightness of the image
/// increases. As the value moves from 0 to -255, the brightness of the image
/// decreases.
///  2. ContrastLevel (-100..100): If the value is 0, the contrast remains the
/// same. As the value moves from 0 to 100, the contrast of the image increases.
/// As the value moves from 0 to -100, the contrast of the image decreases.

procedure TDemoBrightnessContrast.Run;
var
  Bitmap: IGPBitmap;
  BrightnessContrast: IGPBrightnessContrast;
  Params: TGPBrightnessContrastParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Brightness & Contrast effect
  BrightnessContrast := TGPBrightnessContrast.Create;
  Params.BrightnessLevel := 50;
  Params.ContrastLevel := 50;
  BrightnessContrast.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(BrightnessContrast);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Brightness & Contrast Effect', TDemoBrightnessContrast);

end.
