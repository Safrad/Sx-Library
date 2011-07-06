unit uDemoRedEyeCorrection;

interface

uses
  Windows,
  Types,
  GdiPlus,
  uDemo;

type
  TDemoRedEyeCorrection = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoRedEyeCorrection }

{$REGION}
/// This example show the <A>IGPRedEyeCorrection</A> effect. This effect has two
/// parameters:
///  1. The number of rectangles in the bitmap to which the effect is applied.
/// You need to supply a rectangle for each eye.
///  2. A pointer to an array of rectanges. These must be of type Windows.TGPRect
/// (and <I>not</I> of the TGPRect type in the GdiPlus unit). In this example we
/// supply two rectangles that cover the two eyes in the bitmap.
///
/// This example also shows that you can apply the same effect multiple times.
/// The more times you apply the effect, the stronger it will be.
procedure TDemoRedEyeCorrection.Run;
var
  Bitmap: IGPBitmap;
  RedEye: IGPRedEyeCorrection;
  Params: TGPRedEyeCorrectionParams;
  Eyes: array [0..1] of Windows.TRect;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Red Eye Correction effect
  RedEye := TGPRedEyeCorrection.Create;
  Eyes[0] := Rect(132, 115, 152, 135);
  Eyes[1] := Rect(261, 142, 281, 162);
  Params.NumberOfAreas := 2;
  Params.Areas := @Eyes[0];
  RedEye.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(RedEye);
  Bitmap.ApplyEffect(RedEye);
  Bitmap.ApplyEffect(RedEye);
  Bitmap.ApplyEffect(RedEye);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Red Eye Correction Effect', TDemoRedEyeCorrection);

end.
