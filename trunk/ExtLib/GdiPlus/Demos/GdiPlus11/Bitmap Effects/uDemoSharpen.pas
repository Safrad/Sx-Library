unit uDemoSharpen;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoSharpen = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoSharpen }

{$REGION}
/// This example show the <A>IGPSharpen</A> effect. This effect has two
/// parameters:
///  1. The sharpening radius (between 0 and 255) determines how many
/// surrounding pixels are involved in calculating the value of each pixel.
///  2. The amount of sharpening, ranging from 0 (no sharpening) to 100
/// (full sharpening).
///
/// This example also shows how to apply the effect in a non-destructive way
/// by passing it as a parameter to a new <A>IGPGraphics.DrawImage</A> version.
/// This version of the <A>DrawImage</A> method requires the following
/// parameters:
///  1. The bitmap to draw.
///  2. A source rectangle that specifies the portion of the bitmap to draw.
///  3. A <A>IGPMatrix</A> object that is used to transform the source rectangle
/// to a destination rectangle. In this example, the matrix is translated
/// to the right so the destination rectangle will be right next to the original
/// image.
///  4. The effect to apply (any <A>IEffect</A> descendant).
///  5. An optional <A>IGPImageAttributes</A> object.
///  6. The measurement unit to use

procedure TDemoSharpen.Run;
var
  Bitmap: IGPBitmap;
  Sharpen: IGPSharpen;
  Params: TGPSharpenParams;
  Transform: IGPMatrix;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Sharpen effect
  Sharpen := TGPSharpen.Create;
  Params.Radius := 5;
  Params.Amount := 100;
  Sharpen.Parameters := Params;

  // Draw the bitmap using the effect
  Transform := TGPMatrix.Create;
  Transform.Translate(Bitmap.Width, 0);
  Graphics.DrawImage(Bitmap,
    TGPRectF.Create(0, 0, Bitmap.Width, Bitmap.Height),
    Transform, Sharpen, nil, UnitPixel);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Sharpen Effect', TDemoSharpen);

end.
