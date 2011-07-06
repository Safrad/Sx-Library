unit uDemoBlur;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoBlur = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoBlur }

{$REGION}
/// The biggest addition to version 1.1 of GDI+ is the support for various
/// bitmap effects. For example you can apply a blur or change the hue of a
/// bitmap. There is even an effect that performs red eye correction on
/// photographs taken with a flash.
///
/// Each effect requires two declarations: an interface that represents the
/// specific effect (for example <A>IGPBlur</A>, <A>IGPSharpen</A> etc.) and a
/// record for the parameters of the effect (like <A>TGPBlurParams</A>,
/// <A>TGPSharpenParams</A> etc. All effect interfaces inherited form <A>IEffect</A>
/// and have a property called <A>Parameters</A> to set and retrieve the effect
/// parameters.
///
/// The example below creates an <A>IGPBlur</A> object and sets the blur radius
/// parameter to 5 pixels. The <A>IGPBlur.Parameters</A> property must be set to
/// a record of type <A>TGPBlurParams</A>.
///
/// There are two ways to apply an effect. The first way is to apply it directly
/// to the bitmap using the <A>IGPBitmap.ApplyEffect</A> method. This will change
/// the contents of the bitmap. You pass the <A>IEffect</A> object as the first
/// parameter to the method. You can supply an optional rect parameter called
/// ROI (for Region Of Interest) if you want to limit the effect to a certain
/// area of the bitmap.
///
/// The second way to apply an effect is during a <A>IGPGraphics.DrawImage</A>
/// command. This method will leave the original bitmap unchanged but has the
/// disadvantage that the effect has to be recalculated each time the bitmap
/// is drawn. This method will be used in another example.

procedure TDemoBlur.Run;
var
  Bitmap: IGPBitmap;
  Blur: IGPBlur;
  Params: TGPBlurParams;
begin
  // Load and display a bitmap
  Bitmap := TGPBitmap.Create('Collage.png');
  Graphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

  // Create a Blur effect
  Blur := TGPBlur.Create;
  Params.Radius := 5;
  Params.ExpandEdge := False;
  Blur.Parameters := Params;

  // Apply the effect to the bitmap and draw the result
  Bitmap.ApplyEffect(Blur);
  Graphics.DrawImage(Bitmap, Bitmap.Width, 0, Bitmap.Width, Bitmap.Height);
end;

/// The illustration above shows the result. The original image is shown to the
/// left and the blurred one to the right.
{$ENDREGION}

initialization
  RegisterDemo('Bitmap Effects\Blur Effect', TDemoBlur);

end.
