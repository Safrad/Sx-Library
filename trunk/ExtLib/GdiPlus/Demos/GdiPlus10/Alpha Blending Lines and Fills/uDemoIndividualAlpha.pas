unit uDemoIndividualAlpha;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoIndividualAlpha = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoIndividualAlpha }

{$REGION}
/// The previous example <A>Using a Color Matrix to Set Alpha Values in Images</A>
/// shows a nondestructive method for changing the alpha values of an image. The
/// example in that topic renders an image semitransparently, but the pixel data
/// in the bitmap is not changed. The alpha values are altered only during
/// rendering.
///
/// The following example shows how to change the alpha values of individual
/// pixels. The code in the example actually changes the alpha information in a
/// <A>IGPBitmap</A> object. The approach is much slower than using a color matrix
/// and an <A>IGPImageAttributes</A> object but gives you control over the
/// individual pixels in the bitmap.

procedure TDemoIndividualAlpha.Run;
var
  Bitmap: IGPBitmap;
  Width, Height, Row, Column: Integer;
  Color: TGPColor;
  Pen: IGPPen;
begin
  Bitmap := TGPBitmap.Create('Texture1.jpg');
  Width := Bitmap.Width;
  Height := Bitmap.Height;
  for Row := 0 to Height - 1 do
  begin
    for Column := 0 to Width - 1 do
    begin
      Color := Bitmap.Pixels[Column, Row];
      Color.Alpha := (255 * Column) div Width;
      Bitmap.Pixels[Column, Row] := Color;
    end;
  end;
  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 25);
  Graphics.DrawLine(Pen, TGPPoint.Create(10, 35), TGPPoint.Create(200, 35));
  Graphics.DrawImage(Bitmap, 30, 0, Bitmap.Width, Bitmap.Height);
end;

/// The preceding code example uses nested loops to change the alpha value of
/// each pixel in the bitmap. For each pixel, the <A>IGPBitmap.Pixels</A> property
/// returns the existing color, <A>TGPColor.SetAlpha</A> changes the alpha value
/// of the color, and the modified color is stored in the bitmap again. The
/// alpha value is set based on the column of the bitmap. In the first column,
/// alpha is set to 0. In the last column, alpha is set to 255. So the resulting
/// image goes from fully transparent (on the left edge) to fully opaque (on the
/// right edge).
///
/// The <A>IGPBitmap.Pixels</A> property gives you control of the individual pixel
/// values. However, using <A>IGPBitmap.Pixels</A> is not nearly as fast as using
/// the <A>IGPImageAttributes</A> interface and the <A>TGPColorMatrix</A> record.
{$ENDREGION}

initialization
  RegisterDemo('Alpha Blending Lines and Fills\Setting the Alpha Values of Individual Pixels', TDemoIndividualAlpha);

end.
