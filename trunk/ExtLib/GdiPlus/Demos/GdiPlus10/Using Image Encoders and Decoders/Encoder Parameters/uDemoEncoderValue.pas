unit uDemoEncoderValue;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoEncoderValue = class(TDemo)
  strict private
    function GetJpegParameters: IGPEncoderParameters;
    procedure ShowTransformationParameter(const Params: IGPEncoderParameters);
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoEncoderValue }

{$REGION}
/// A given encoder supports certain parameter categories, and for each of those
/// categories, that encoder allows certain values. For example, the JPEG
/// encoder supports the EncoderValueQuality parameter category, and the
/// allowable parameter values are the integers 0 through 100. Some of the
/// allowable parameter values are the same across several encoders. These
/// standard values are defined in the <A>TEncoderValue</A> enumeration:
///
///  <C>type</C>
///  <C>  TEncoderValue = (</C>
///  <C>    EncoderValueColorTypeCMYK,            // 0</C>
///  <C>    EncoderValueColorTypeYCCK,            // 1</C>
///  <C>    EncoderValueCompressionLZW,           // 2</C>
///  <C>    EncoderValueCompressionCCITT3,        // 3</C>
///  <C>    EncoderValueCompressionCCITT4,        // 4</C>
///  <C>    EncoderValueCompressionRle,           // 5</C>
///  <C>    EncoderValueCompressionNone,          // 6</C>
///  <C>    EncoderValueScanMethodInterlaced,     // 7</C>
///  <C>    EncoderValueScanMethodNonInterlaced,  // 8</C>
///  <C>    EncoderValueVersionGif87,             // 9</C>
///  <C>    EncoderValueVersionGif89,             // 10</C>
///  <C>    EncoderValueRenderProgressive,        // 11</C>
///  <C>    EncoderValueRenderNonProgressive,     // 12</C>
///  <C>    EncoderValueTransformRotate90,        // 13</C>
///  <C>    EncoderValueTransformRotate180,       // 14</C>
///  <C>    EncoderValueTransformRotate270,       // 15</C>
///  <C>    EncoderValueTransformFlipHorizontal,  // 16</C>
///  <C>    EncoderValueTransformFlipVertical,    // 17</C>
///  <C>    EncoderValueMultiFrame,               // 18</C>
///  <C>    EncoderValueLastFrame,                // 19</C>
///  <C>    EncoderValueFlush,                    // 20</C>
///  <C>    EncoderValueFrameDimensionTime,       // 21</C>
///  <C>    EncoderValueFrameDimensionResolution, // 22</C>
///  <C>    EncoderValueFrameDimensionPage);      // 23</C>
///
/// One of the parameter categories supported by the JPEG encoder is the
/// EncoderTransformation category. By examining the <A>TEncoderValue</A>
/// enumeration, you might speculate (and you would be correct) that the
/// EncoderTransformation category allows the following five values:
///
///  <C>EncoderValueTransformRotate90,        // 13</C>
///  <C>EncoderValueTransformRotate180,       // 14</C>
///  <C>EncoderValueTransformRotate270,       // 15</C>
///  <C>EncoderValueTransformFlipHorizontal,  // 16</C>
///  <C>EncoderValueTransformFlipVertical,    // 17</C>
///
/// The following example verifies that the JPEG encoder supports the
/// EncoderTransformation parameter category and that there are five allowable
/// values for such a parameter.

function TDemoEncoderValue.GetJpegParameters: IGPEncoderParameters;
var
  Bitmap: IGPBitmap;
  Param: PGPNativeEncoderParameter;
begin
  // Create IGPBitmap (inherited from IGPImage) interface so that we can call
  // GetEncoderParameterList.
  Bitmap := TGPBitmap.Create(1, 1);

  // Get the parameter list for the JPEG encoder.
  Result := Bitmap.GetEncoderParameterList(TGPImageFormat.Jpeg.CodecId);

  TextOutput.Add(Format('There are %d TEncoderParameter records in the array.',
    [Result.Count]));
  Param := Result[0];
  TextOutput.Add('Parameter[0]');
  TextOutput.Add(Format('  The GUID is %s.', [GUIDToString(Param.Guid)]));
  TextOutput.Add(Format('  The value type is %d.', [Ord(Param.ValueType)]));
  TextOutput.Add(Format('  The number of values is %d.', [Param.NumberOfValues]));
end;

/// The preceding code should output the following:
///
///  There are 4 TEncoderParameter records in the array.
///  Parameter[0]
///     The GUID is {8D0EB2D1-A58E-4EA8-AA14-108074B7B6F9}.
///     The value type is 4.
///     The number of values is 5.
///
/// You can look up the GUID in GdiPlus.pas and find out that the category of
/// this <A>TEncoderParameter</A> record is EncoderTransformation. The
/// <A>TGPEncoderParameterValueType</A> enumeration indicates that data type 4 is
/// EncoderParameterValueTypeLong (32-bit unsigned integer). The number of
/// values is five, so we know that the Value member of the
/// <A>TEncoderParameter</A> record is a pointer to an array of five LongWord
/// values.
///
/// The following code is a continuation of the preceding example. The code
/// lists the allowable values for an <A>TEncoderTransformation</A> parameter:

procedure TDemoEncoderValue.ShowTransformationParameter(
  const Params: IGPEncoderParameters);
var
  Param: PGPNativeEncoderParameter;
  {$POINTERMATH ON} // Allow access to Values as an array
  Values: PLongWord;
  J, NumValues: Integer;
  S: String;
begin
  Assert(Params.Count > 0);
  Param := Params[0];
  Values := Param.Value;
  NumValues := Param.NumberOfValues;
  S := '  The allowable values are';
  for J := 0 to NumValues - 1 do
    S := S + '  ' + IntToStr(Values[J]);
  TextOutput.Add(S);
end;

/// The preceding code produces the following output:
///
///  The allowable values are  13  14  15  16  17
///
/// The allowable values (13, 14, 15, 16, and 17) correspond to the following
/// members of the <A>TEncoderValue</A> enumeration:
///
///  <C>EncoderValueTransformRotate90,        // 13</C>
///  <C>EncoderValueTransformRotate180,       // 14</C>
///  <C>EncoderValueTransformRotate270,       // 15</C>
///  <C>EncoderValueTransformFlipHorizontal,  // 16</C>
///  <C>EncoderValueTransformFlipVertical,    // 17</C>
{$ENDREGION}

class function TDemoEncoderValue.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

procedure TDemoEncoderValue.Run;
var
  Params: IGPEncoderParameters;
begin
  Params := GetJpegParameters;
  TextOutput.Add('');
  ShowTransformationParameter(Params);
end;

initialization
  RegisterDemo('Using Image Encoders and Decoders\Encoder Parameters\Using the TEncoderValue enumeration', TDemoEncoderValue);

end.
