unit uDemoListInstalledDecoders;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoListInstalledDecoders = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

{ TDemoListInstalledDecoders }

class function TDemoListInstalledDecoders.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

{$REGION}
/// Microsoft Windows GDI+ provides the <A>TGPImageCodecInfo.GetImageDecoders</A>
/// class function so that you can determine which image decoders are available
/// on your computer. <A>TGPImageCodecInfo.GetImageDecoders</A> returns an array
/// of <A>IGPImageCodecInfo</A> objects.

procedure TDemoListInstalledDecoders.Run;
var
  Decoders: IGPImageCodecInfoArray;
  Decoder: IGPImageCodecInfo;
begin
  Decoders := TGPImageCodecInfo.GetImageDecoders;
  for Decoder in Decoders do
    TextOutput.Add(Decoder.MimeType);
end;

/// The output above should show at least the following entries:
///
///  image/bmp
///  image/jpeg
///  image/gif
///  image/x-emf
///  image/x-wmf
///  image/tiff
///  image/png
///  image/x-icon
{$ENDREGION}

initialization
  RegisterDemo('Using Image Encoders and Decoders\Listing Installed Decoders', TDemoListInstalledDecoders);

end.
