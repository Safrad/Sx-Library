unit uDemoTint;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoTint = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoTint }

{$REGION}
/// The <A>IGPTint</A> interface enables you to apply a tint to a bitmap. The
/// effect has 2 parameters:
///  1. Hue (-180..180): A value of 0 specifies blue. A positive value specifies
/// a clockwise angle on the color wheel. For example, positive 60 specifies
/// cyan and positive 120 specifies green. A negative value specifies a
/// counter-clockwise angle on the color wheel. For example, negative 60
/// specifies magenta and negative 120 specifies red.
///  2. Amount (-100..100): specifies how much the hue (given by the hue
/// parameter) is strengthened or weakened. A value of 0 specifies no change.
/// Positive values specify that the hue is strengthened and negative values
/// specify that the hue is weakened.

procedure TDemoTint.Run;
var
  Bitmap: IGPBitmap;
  Tint: IGPTint;
  Params: TGPTintParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Tint effect
  Tint := TGPTint.Create;
  Params.Hue := 120;
  Params.Amount := 50;
  Tint.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(Tint);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Tint Effect', TDemoTint);

end.
