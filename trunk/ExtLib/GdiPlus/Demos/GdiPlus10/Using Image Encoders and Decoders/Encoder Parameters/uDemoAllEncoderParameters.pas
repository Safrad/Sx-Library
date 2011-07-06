unit uDemoAllEncoderParameters;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoAllEncoderParameters = class(TDemo)
  strict private
    procedure ShowEncoders;
    procedure ShowAllEncoderParameters(const Encoder: IGPImageCodecInfo);
    function EncoderParameterCategoryFromGuid(const Guid: TGuid): String;
    function ValueTypeToString(const ValueType: TGPEncoderParameterValueType): String;
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoAllEncoderParameters }

class function TDemoAllEncoderParameters.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

{$REGION}
/// The following example lists all the parameters supported by the various
/// encoders installed on the computer. The main method calls
/// <A>TGPImageCodecInfo.GetImageEncoders</A> to discover which encoders are
/// available. For each available encoder, the main method calls the helper
/// method ShowAllEncoderParameters.

procedure TDemoAllEncoderParameters.ShowEncoders;
var
  Encoders: IGPImageCodecInfoArray;
  Encoder: IGPImageCodecInfo;
begin
  Encoders := TGPImageCodecInfo.GetImageEncoders;
  for Encoder in Encoders do
    ShowAllEncoderParameters(Encoder);
end;

/// The ShowAllEncoderParameters method calls the
/// <A>IGPImage.GetEncoderParameterList</A> method to discover which parameters
/// are supported by a given encoder. For each supported parameter, the function
/// lists the category, data type, and number of values. The
/// ShowAllEncoderParameters function relies on two helper functions:
/// EncoderParameterCategoryFromGuid and ValueTypeToString.

procedure TDemoAllEncoderParameters.ShowAllEncoderParameters(
  const Encoder: IGPImageCodecInfo);
var
  Bitmap: IGPBitmap;
  Params: IGPEncoderParameters;
  Param: PGPNativeEncoderParameter;
  {$POINTERMATH ON} // Allow access to Values as an array
  Values: PLongWord;
  J, K: Integer;
begin
  TextOutput.Add(Encoder.MimeType);

  // Create IGPBitmap (inherited from IGPImage) interface so that we can call
  // GetEncoderParameterList.
  Bitmap := TGPBitmap.Create(1, 1);

  // Get the parameter list for the encoder.
  Params := Bitmap.GetEncoderParameterList(Encoder.ClsId);
  TextOutput.Add(Format('  There are %d TEncoderParameter records in the array.',
    [Params.Count]));

  for K := 0 to Params.Count - 1 do
  begin
    Param := Params[K];
    TextOutput.Add(Format('    Parameter[%d]', [K]));
    TextOutput.Add(Format('      The category is %s.',
      [EncoderParameterCategoryFromGuid(Param.Guid)]));
    TextOutput.Add(Format('      The data type is %s.',
      [ValueTypeToString(Param.ValueType)]));
    TextOutput.Add(Format('      The number of values is %d.',
      [Param.NumberOfValues]));

    if IsEqualGuid(Param.Guid, EncoderColorDepth) then
    begin
      TextOutput.Add('      The allowable values for ColorDepth are');
      Values := Param.Value;
      for J := 0 to Param.NumberOfValues - 1 do
        TextOutput.Add('        ' + IntToStr(Values[J]));
    end;
  end;

  TextOutput.Add('');
end;

function TDemoAllEncoderParameters.ValueTypeToString(
  const ValueType: TGPEncoderParameterValueType): String;
const
  ValueTypes: array [TGPEncoderParameterValueType] of String = (
    'Byte', 'ASCII', 'Short', 'Long', 'Rational', 'LongRange', 'Undefined',
    'RationalRange');
begin
  Result := ValueTypes[ValueType];
end;

function TDemoAllEncoderParameters.EncoderParameterCategoryFromGuid(
  const Guid: TGuid): String;
begin
  if IsEqualGUID(Guid, EncoderCompression) then
    Result := 'Compression'
  else if IsEqualGUID(Guid, EncoderColorDepth) then
    Result := 'ColorDepth'
  else if IsEqualGUID(Guid, EncoderScanMethod) then
    Result := 'ScanMethod'
  else if IsEqualGUID(Guid, EncoderVersion) then
    Result := 'Version'
  else if IsEqualGUID(Guid, EncoderRenderMethod) then
    Result := 'RenderMethod'
  else if IsEqualGUID(Guid, EncoderQuality) then
    Result := 'Quality'
  else if IsEqualGUID(Guid, EncoderTransformation) then
    Result := 'Transformation'
  else if IsEqualGUID(Guid, EncoderLuminanceTable) then
    Result := 'LuminanceTable'
  else if IsEqualGUID(Guid, EncoderChrominanceTable) then
    Result := 'ChrominanceTable'
  else if IsEqualGUID(Guid, EncoderSaveFlag) then
    Result := 'SaveFlag'
  else
    Result := GUIDToString(Guid);
end;

/// You can draw the following conclusions by examining the preceding program output:
///
///  -The JPEG encoder supports the Transformation, Quality, LuminanceTable, and
/// ChrominanceTable parameter categories.
///  -The TIFF encoder supports the Compression, ColorDepth, and SaveFlag
/// parameter categories.
///
/// You can also see the number of acceptable values for each parameter
/// category. For example, you can see that the ColorDepth parameter category
/// (TIFF codec) has five values of type LongWord. The code above lists those
/// five values (1, 4, 8, 24 and 32).
///
/// <B>Note</B> In some cases, the values in an <A>TEncoderParameter</A> record
/// are the numeric values of elements of the <A>TEncoderValue</A> enumeration.
/// However, the numbers in the preceding list do not relate to the
/// <A>TEncoderValue</A> enumeration. The numbers mean 1 bit per pixel, 2 bits
/// per pixel, and so on. If you write code similar to the preceding example to
/// investigate the allowable values for the other parameter categories, you
/// will obtain a result similar to the following.
///
///  -JPEG encoder parameter Transformation, Allowable values:
/// EncoderValueTransformRotate90, EncoderValueTransformRotate180,
/// EncoderValueTransformRotate270, EncoderValueTransformFlipHorizontal,
/// EncoderValueTransformFlipVertical
///  -JPEG encoder parameter Quality, Allowable values: 0 through 100
///  -TIFF encoder parameter Compression, Allowable values:
/// EncoderValueCompressionLZW, EncoderValueCompressionCCITT3,
/// EncoderValueCompressionCCITT4, EncoderValueCompressionRle,
/// EncoderValueCompressionNone
///  -TIFF encoder parameter ColorDepth, Allowable values: 1, 4, 8, 24, 32
///  -TIFF encoder parameter SaveFlag, Allowable values: EncoderValueMultiFrame
///
/// <B>Note</B> If the width and height of a JPEG image are multiples of 16, you
/// can apply any of the transformations allowed by the EncoderTransformation
/// parameter category (for example, 90-degree rotation) without loss of
/// information.
{$ENDREGION}

procedure TDemoAllEncoderParameters.Run;
begin
  ShowEncoders;
end;

initialization
  RegisterDemo('Using Image Encoders and Decoders\Encoder Parameters\Listing Parameters and Values for all Encoders', TDemoAllEncoderParameters);

end.
