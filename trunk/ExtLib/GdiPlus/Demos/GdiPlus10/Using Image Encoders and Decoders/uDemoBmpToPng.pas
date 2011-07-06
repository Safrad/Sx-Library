unit uDemoBmpToPng;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoBmpToPng = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoBmpToPng }

class function TDemoBmpToPng.Outputs: TDemoOutputs;
begin
  Result := [doText, doGraphic];
end;

{$REGION}
/// To save an image to a disk file, call the <A>Save</A> method of the
/// <A>IGPImage</A> interface. The following example loads a BMP image from a disk
/// file, converts the image to the PNG format, and saves the converted image to
/// a new disk file. The code uses the predefined class property
/// TGPImageFormat.Png to save the image in PNG format.

procedure TDemoBmpToPng.Run;
var
  Image: IGPImage;
begin
  Image := TGPImage.Create('Bird.bmp');
  Graphics.DrawImage(Image, 10, 10, Image.Width, Image.Height);
  try
    Image.Save('Bird.png', TGPImageFormat.Png);
    TextOutput.Add('Bird.png was saved successfully.');

    Image := TGPImage.Create('Bird.png');
    Graphics.DrawImage(Image, 130, 10, Image.Width, Image.Height);
  except
    on E: Exception do
      TextOutput.Add('Failure: ' + E.Message);
  end;
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Image Encoders and Decoders\Converting a BMP Image to a PNG Image', TDemoBmpToPng);

end.
