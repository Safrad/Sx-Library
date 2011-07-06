unit uDemoListInstalledEncoders;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoListInstalledEncoders = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

{ TDemoListInstalledEncoders }

class function TDemoListInstalledEncoders.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

{$REGION}
/// Microsoft Windows GDI+ provides the <A>Image</A> class and the <A>Bitmap</A>
/// class for storing images in memory and manipulating images in memory. GDI+
/// writes images to disk files with the help of image encoders and loads images
/// from disk files with the help of image decoders. An encoder translates the
/// data in an Image or Bitmap object into a designated disk file format. A
/// decoder translates the data in a disk file to the format required by the
/// <A>Image</A> and <A>Bitmap</A> objects. GDI+ has built-in encoders and decoders that support the following file types:
///
///  -BMP
///  -GIF
///  -JPEG
///  -PNG
///  -TIFF
///
/// GDI+ also has built-in decoders that support the following file types:
///
///  -WMF
///  -EMF
///  -ICON
///
/// GDI+ provides the class function <A>TGPImageCodecInfo.GetImageEncoders</A>
/// so that you can determine which image encoders are available on your
/// computer. <A>TGPImageCodecInfo.GetImageEncoders</A> returns an array of
/// <A>IGPImageCodecInfo</A> objects.

procedure TDemoListInstalledEncoders.Run;
var
  Encoders: IGPImageCodecInfoArray;
  Encoder: IGPImageCodecInfo;
begin
  Encoders := TGPImageCodecInfo.GetImageEncoders;
  for Encoder in Encoders do
    TextOutput.Add(Encoder.MimeType);
end;

/// The output above should show at least the following entries:
///
///  image/bmp
///  image/jpeg
///  image/gif
///  image/tiff
///  image/png
{$ENDREGION}

initialization
  RegisterDemo('Using Image Encoders and Decoders\Listing Installed Encoders', TDemoListInstalledEncoders);

end.
