unit uDemoColorLUT;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoColorLUT = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoColorLUT }

{$REGION}
/// A <A>TGPColorLUTParams</A> record has four members, each being a lookup table
/// for a particular color channel: alpha, red, green, or blue. The lookup
/// tables can be used to make custom color adjustments to bitmaps. Each lookup
/// table is an array of 256 bytes that you can set to values of your choice.

procedure TDemoColorLUT.Run;
var
  Bitmap: IGPBitmap;
  ColorLUT: IGPColorLUT;
  Params: TGPColorLUTParams;
  I: Integer;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Color LUT effect
  ColorLUT := TGPColorLUT.Create;

  for I := 0 to 255 do
  begin
    // Keep the red channel unchanged
    Params.LutR[I] := I;
    // Inverse the green channel
    Params.LutG[I] := 255 - I;
    // Half the intensity of the blue channel
    Params.LutB[I] := I div 2;
    // Set alpha (opacity) to 80%
    Params.LutA[I] := (I * 80) div 100;
  end;
  ColorLUT.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(ColorLUT);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Color LUT Effect', TDemoColorLUT);

end.
