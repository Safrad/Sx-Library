unit uDemoTransformJpeg;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoTransformJpeg = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoTransformJpeg }

class function TDemoTransformJpeg.Outputs: TDemoOutputs;
begin
  Result := [doGraphic, doText];
end;

{$REGION}
/// When you compress a JPEG image, some of the information in the image is
/// lost. If you open a JPEG file, alter the image, and save it to another JPEG
/// file, the quality will decrease. If you repeat that process many times, you
/// will see a substantial degradation in the image quality.
///
/// Because JPEG is one of the most popular image formats on the Web, and
/// because people often like to modify JPEG images, GDI+ provides the following
/// transformations that can be performed on JPEG images without loss of
/// information:
///
///  -Rotate 90 degrees
///  -Rotate 180 degrees
///  -Rotate 270 degrees
///  -Flip horizontally
///  -Flip vertically
///
/// You can apply one of the transformations shown in the preceding list when
/// you call the <A>Save</A> method of an <A>IGPImage</A> object. If the following
/// conditions are met, then the transformation will proceed without loss of
/// information:
///
///  -The file used to construct the <A>IGPImage</A> object is a JPEG file.
///  -The width and height of the image are both multiples of 16.
///
/// If the width and height of the image are not both multiples of 16, GDI+ will
/// do its best to preserve the image quality when you apply one of the rotation
/// or flipping transformations shown in the preceding list.
///
/// To transform a JPEG image, initialize an <A>IGPEncoderParameters</A> object
/// and pass that object to the <A>Save</A> method of the <A>IGPImage</A>
/// interface. Add a single parameter to the <A>IGPEncoderParameters</A> of type
/// EncoderTransformation and with a value that holds one of the following
/// elements of the <A>TEncoderValue</A> enumeration:
///
///  -EncoderValueTransformRotate90,
///  -EncoderValueTransformRotate180,
///  -EncoderValueTransformRotate270,
///  -EncoderValueTransformFlipHorizontal,
///  -EncoderValueTransformFlipVertical
///
/// The following example creates an <A>IGPImage</A> object from a JPEG file and
/// then saves the image to a new file. During the save process, the image is
/// rotated 90 degrees. If the width and height of the image are both multiples
/// of 16, the process of rotating and saving the image causes no loss of
/// information.

procedure TDemoTransformJpeg.Run;
var
  Image: IGPImage;
  Width, Height: Integer;
  Params: IGPEncoderParameters;
begin
  Image := TGPImage.Create('ImageFile.jpg');
  Graphics.DrawImage(Image, 0, 0, Image.Width div 2, Image.Height div 2);
  Width := Image.Width;
  Height := Image.Height;

  TextOutput.Add(Format('The width of the image is %d,', [Width]));
  if ((Width mod 16) = 0) then
    TextOutput.Add('which is a multiple of 16.')
  else
    TextOutput.Add('which is not a multiple of 16.');

  TextOutput.Add(Format('The height of the image is %d,', [Height]));
  if ((Height mod 16) = 0) then
    TextOutput.Add('which is a multiple of 16.')
  else
    TextOutput.Add('which is not a multiple of 16.');

  Params := TGPEncoderParameters.Create;
  Params.Add(EncoderTransformation, EncoderValueTransformRotate90);
  Image.Save('ImageFileR90.jpg', TGPImageFormat.Jpeg, Params);

  // Reload rotated image
  Image := TGPImage.Create('ImageFileR90.jpg');
  Graphics.DrawImage(Image, 330, 0, Image.Width div 2, Image.Height div 2);
end;
{$ENDREGION}

initialization
  RegisterDemo('Using Image Encoders and Decoders\Transforming a JPEG Image Without Loss of Information', TDemoTransformJpeg);

end.
