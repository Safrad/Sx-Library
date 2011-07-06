unit uDemoColorCurve;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoColorCurve = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoColorCurve }

{$REGION}
/// The <A>IGPColorCurve</A> interface encompasses eight separate adjustments:
/// exposure, density, contrast, highlight, shadow, midtone, white saturation,
/// and black saturation. The effect has 3 parameters:
///  1. Adjustment: Element of the <A>TCurveAdjustments</A> enumeration that
/// specifies the adjustment to be applied (one of the eight mentioned earlier).
///  2. Channel: Element of the CurveChannel enumeration that specifies the
/// color channel to which the adjustment applies (All, Red, Green or Blue).
///  3. AdjustValue: Integer that specifies the intensity of the adjustment. The
/// range of acceptable values depends on which adjustment is being applied. To
/// see the range of acceptable values for a particular adjustment, see the
/// <A>TCurveAdjustments</A> enumeration.

procedure TDemoColorCurve.Run;
var
  Bitmap: IGPBitmap;
  ColorCurve: IGPColorCurve;
  Params: TGPColorCurveParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Color Curve effect
  ColorCurve := TGPColorCurve.Create;
  Params.Adjustment := AdjustExposure;
  Params.Channel := CurveChannelAll;
  Params.AdjustValue := -50;
  ColorCurve.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(ColorCurve);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Color Curve Effect', TDemoColorCurve);

end.
