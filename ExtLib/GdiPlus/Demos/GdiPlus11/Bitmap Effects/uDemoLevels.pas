unit uDemoLevels;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoLevels = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoLevels }

{$REGION}
/// The <A>IGPLevels</A> interface encompasses three bitmap adjustments:
/// highlight, midtone, and shadow. The effect has 3 parameters:
///  1: Highlight (0..100): You can use this adjustment to lighten pixels that
/// are already lighter than a certain threshold. Setting highlight to 100
/// specifies no change. Setting highlight to t specifies that a color channel
/// value is increased if it is already greater than t percent of full
/// intensity. For example, setting highlight to 90 specifies that all color
/// channel values greater than 90 percent of full intensity are increased.
///  2. Midtone (-100..100): Color channel values in the middle of the intensity
/// range are altered more than color channel values near the minimum or maximum
/// intensity. You can use this adjustment to lighten (or darken) an image
/// without loosing the contrast between the darkest and lightest portions of
/// the image. A value of 0 specifies no change. Positive values specify that
/// the midtones are made lighter, and negative values specify that the midtones
/// are made darker.
///  3. Shadow (0..100): Integer in the range 0 through 100 that specifies which
/// pixels should be darkened. You can use this adjustment to darken pixels that
/// are already darker than a certain threshold. Setting shadow to 0 specifies
/// no change. Setting shadow to t specifies that a color channel value is
/// decreased if it is already less than t percent of full intensity. For
/// example, setting shadow to 10 specifies that all color channel values less
/// than 10 percent of full intensity are decreased.

procedure TDemoLevels.Run;
var
  Bitmap: IGPBitmap;
  Levels: IGPLevels;
  Params: TGPLevelsParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Levels effect
  Levels := TGPLevels.Create;
  Params.Highlight := 90;
  Params.Midtone := -20;
  Params.Shadow := 10;
  Levels.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(Levels);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Levels Effect', TDemoLevels);

end.
