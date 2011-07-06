unit uDemoColorMatrix;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoColorMatrix = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoColorMatrix }

{$REGION}
/// The <A>IGPColorMatrixEffect</A> interface enables you to apply an affine
/// transformation to a bitmap. To specify the transformation, set the elements
/// of a <A>TGPColorMatrix</A> records, and pass that record to the
/// <A>SetParameters</A> method of a <A>IGPColorMatrixEffect</A> object.

procedure TDemoColorMatrix.Run;
const
  ColorMatrix: TGPColorMatrix = (
    M: ((1.5, 0.3, 0.0, 0.0, 0.0),
        (0.0, 0.8, 0.0, 0.0, 0.0),
        (0.0, 0.0, 1.0, 0.0, 0.0),
        (0.0, 0.0, 0.0, 1.0, 0.0),
        (0.2, 0.3, 0.4, 0.0, 1.0)));
var
  Bitmap: IGPBitmap;
  ColorMatrixEffect: IGPColorMatrixEffect;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Color Matrix effect
  ColorMatrixEffect := TGPColorMatrixEffect.Create;
  ColorMatrixEffect.Parameters := ColorMatrix;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(ColorMatrixEffect);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Color Matrix Effect', TDemoColorMatrix);

end.
