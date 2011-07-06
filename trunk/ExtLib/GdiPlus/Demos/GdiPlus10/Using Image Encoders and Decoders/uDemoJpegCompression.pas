unit uDemoJpegCompression;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoJpegCompression = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoJpegCompression }

class function TDemoJpegCompression.Outputs: TDemoOutputs;
begin
  Result := [doText, doGraphic];
end;

{$REGION}
/// To specify the compression level when you save a JPEG image, initialize an
/// <A>IGPEncoderParameters</A> object and pass that object to the <A>Save</A>
/// method of the <A>IGPImage</A> class. The <A>EncoderParameters</A>
/// interface has several overloaded <A>Add</A> methods that you can use to add
/// parameters. To set a JPEG compression level, use the <A>Add</A> method with
/// a parameter type of EncoderQuality and a value between 0 and 100.
///
/// The following example saves three JPEG images, each with a different quality
/// level. A quality level of 0 corresponds to the greatest compression, and a
/// quality level of 100 corresponds to the least compression.

procedure TDemoJpegCompression.Run;
var
  Image, JpegImage: IGPImage;
  Params: IGPEncoderParameters;
  Quality: Int32;
begin
  Image := TGPImage.Create('GrapeBunch.bmp');
  Graphics.DrawImage(Image, 10, 10, Image.Width, Image.Height);

  Params := TGPEncoderParameters.Create;

  // Save the image as a JPEG with quality level 1.
  Quality := 1;
  Params.Add(EncoderQuality, Quality);
  try
    Image.Save('GrapeBunch001.jpg', TGPImageFormat.Jpeg, Params);
    TextOutput.Add('GrapeBunch001.jpg was saved successfully.');

    JpegImage := TGPImage.Create('GrapeBunch001.jpg');
    Graphics.DrawImage(JpegImage, 220, 10, JpegImage.Width, JpegImage.Height);
  except
    on E: Exception do
      TextOutput.Add('Attempt to save GrapeBunch001.jpg failed: ' + E.Message);
  end;

  // Save the image as a JPEG with quality level 50.
  Quality := 50;
  Params.Clear;
  Params.Add(EncoderQuality, Quality);
  try
    Image.Save('GrapeBunch050.jpg', TGPImageFormat.Jpeg, Params);
    TextOutput.Add('GrapeBunch050.jpg was saved successfully.');

    JpegImage := TGPImage.Create('GrapeBunch050.jpg');
    Graphics.DrawImage(JpegImage, 430, 10, JpegImage.Width, JpegImage.Height);
  except
    on E: Exception do
      TextOutput.Add('Attempt to save GrapeBunch050.jpg failed: ' + E.Message);
  end;

  // Save the image as a JPEG with quality level 100.
  Quality := 100;
  Params.Clear;
  Params.Add(EncoderQuality, Quality);
  try
    Image.Save('GrapeBunch100.jpg', TGPImageFormat.Jpeg, Params);
    TextOutput.Add('GrapeBunch100.jpg was saved successfully.');

    JpegImage := TGPImage.Create('GrapeBunch100.jpg');
    Graphics.DrawImage(JpegImage, 640, 10, JpegImage.Width, JpegImage.Height);
  except
    on E: Exception do
      TextOutput.Add('Attempt to save GrapeBunch100.jpg failed: ' + E.Message);
  end;
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Image Encoders and Decoders\Setting JPEG Compression Level', TDemoJpegCompression);

end.
