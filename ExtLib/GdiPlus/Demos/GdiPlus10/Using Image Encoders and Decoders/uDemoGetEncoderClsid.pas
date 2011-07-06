unit uDemoGetEncoderClsid;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoGetEncoderClsid = class(TDemo)
  strict private
    function GetEncoderClsid(const MimeType: String; out ClsId: TGUID): Integer;
    procedure GetPngClsId;
    procedure GetJpegClsId;
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoGetEncoderClsid }

{$REGION}
/// The function GetEncoderClsid in the following example receives the MIME type
/// of an encoder and returns the class identifier (CLSID) of that encoder. The
/// MIME types of the encoders built into Microsoft Windows GDI+ are as follows:
///
///  -image/bmp
///  -image/jpeg
///  -image/gif
///  -image/tiff
///  -image/png
///
/// The function calls <A>TGPImageCodecInfo.GetImageEncoders</A> to get an array
/// of <A>IGPImageCodecInfo</A> objects. If one of the <A>IGPImageCodecInfo</A>
/// objects in that array represents the requested encoder, the function returns
/// the index of the <A>IGPImageCodecInfo</A> object and copies the CLSID into the
/// ClsId parameter. If the function fails, it returns –1.

function TDemoGetEncoderClsid.GetEncoderClsid(const MimeType: String;
  out ClsId: TGUID): Integer;
var
  Encoders: IGPImageCodecInfoArray;
begin
  Encoders := TGPImageCodecInfo.GetImageEncoders;
  for Result := 0 to Encoders.Count - 1 do
    if SameText(Encoders[Result].MimeType, MimeType) then
    begin
      ClsId := Encoders[Result].ClsId;
      Exit;
    end;
  Result := -1;
end;

/// The following example calls the GetEncoderClsid function to determine the
/// CLSID of the PNG encoder:

procedure TDemoGetEncoderClsid.GetPngClsId;
var
  Index: Integer;
  ClsId: TGUID;
begin
  Index := GetEncoderClsid('image/png', ClsId);
  if (Index < 0) then
    TextOutput.Add('The PNG encoder is not installed.')
  else
  begin
    TextOutput.Add('An IGPImageCodecInfo object representing the PNG encoder');
    TextOutput.Add(Format('was found at position %d in the array.', [Index]));
    TextOutput.Add(Format('The CLSID of the PNG encoder is %s.', [GUIDToString(ClsId)]));
  end;
end;

/// When you need information about one of the built-in encoders or decoders,
/// there is an easier way that doesn't require a function like GetEncoderClsId.
/// The <A>TGPImageFormat</A> class has some class properties called <A>Bmp</A>,
/// <A>Jpeg</A>, <A>Gif</A>, <A>Tiff</A> and <A>Png</A> that return an
/// <A>IImageFormat</A> object which contains a CodecId property.

procedure TDemoGetEncoderClsid.GetJpegClsId;
begin
  TextOutput.Add(Format('The CLSID of the JPEG encoder is %s.',
    [GUIDToString(TGPImageFormat.Jpeg.CodecId)]));
end;
{$ENDREGION}

class function TDemoGetEncoderClsid.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

procedure TDemoGetEncoderClsid.Run;
var
  ClsId: TGUID;
begin
  GetEncoderClsid('image/bmp', ClsId);
  GetPngClsid;
  TextOutput.Add('');
  GetJpegClsId;
end;

initialization
  RegisterDemo('Using Image Encoders and Decoders\Retrieving the Class Identifier for an Encoder', TDemoGetEncoderClsid);

end.
