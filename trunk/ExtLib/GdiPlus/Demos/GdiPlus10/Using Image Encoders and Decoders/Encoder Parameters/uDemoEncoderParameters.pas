unit uDemoEncoderParameters;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoEncoderParameters = class(TDemo)
  strict private
    function GetJpegParameters: IGPEncoderParameters;
    procedure ShowSecondJpegParameter(const Params: IGPEncoderParameters);
    procedure ShowJpegQualityParameter(const Params: IGPEncoderParameters);
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoEncoderParameters }

{$REGION}
/// The <A>IGPImage</A> interface provides the <A>GetEncoderParameterList</A>
/// method so that you can determine the parameters that are supported by a
/// given image encoder. The <A>GetEncoderParameterList</A> method returns an
/// array of <A>TEncoderParameter</A> records.
///
/// The following example obtains the parameter list for the JPEG encoder.
/// It uses the class property TGPImageFormat.Jpeg to obtain the ClsId of the
/// JPEG encoder.

function TDemoEncoderParameters.GetJpegParameters: IGPEncoderParameters;
var
  Bitmap: IGPBitmap;
begin
  // Create IGPBitmap (inherited from IGPImage) interface so that we can call
  // GetEncoderParameterList.
  Bitmap := TGPBitmap.Create(1, 1);

  // Get the parameter list for the JPEG encoder.
  Result := Bitmap.GetEncoderParameterList(TGPImageFormat.Jpeg.CodecId);

  TextOutput.Add(Format('There are %d TEncoderParameter records in the array.',
    [Result.Count]))
end;

/// Each of the <A>TEncoderParameter</A> records in the array has the following
/// four public data members:
///
///  -Guid (TGUID): GUID of the parameter
///  -NumberOfValues (LongWord): Number of the parameter values
///  -ValueType (TGPEncoderParameterValueType): Value type, like ValueTypeLONG etc.
///  -Value (Pointer): A pointer to the parameter values
///
/// The following example is a continuation of the preceding example. The code
/// looks at the second <A>TEncoderParameter</A> record in the array returned by
/// <A>GetEncoderParameterList</A>.

procedure TDemoEncoderParameters.ShowSecondJpegParameter(
  const Params: IGPEncoderParameters);
var
  Param: PGPNativeEncoderParameter;
begin
  Assert(Params.Count >= 2);
  Param := Params[1];
  TextOutput.Add('Parameter[1]');
  TextOutput.Add(Format('  The GUID is %s.', [GUIDToString(Param.Guid)]));
  TextOutput.Add(Format('  The value type is %d.', [Ord(Param.ValueType)]));
  TextOutput.Add(Format('  The number of values is %d.', [Param.NumberOfValues]));
end;

/// The preceding code should output the following:
///
///  Parameter[1]
///     The GUID is {1D5BE4B5-FA4A-452D-9CDD-5DB35105E7EB}.
///     The value type is 6.
///     The number of values is 1.
///
/// You can look up the GUID in GdiPlus.pas and find out that the category of
/// this <A>TEncoderParameter</A> record is EncoderQuality. You can use this
/// category (EncoderQuality) of parameter to set the compression level of a
/// JPEG image.
///
/// The <A>TGPEncoderParameterValueType</A> enumeration indicates that data type 6
/// is EncoderParameterValueTypeLongRange. A long range is a pair of LongInt
/// values.
///
/// The number of values is one, so we know that the Value member of the
/// <A>TEncoderParameter</A> record is a pointer to an array that has one
/// element. That one element is a pair of LongWord values.
///
/// The following example is a continuation of the preceding two examples. The
/// code defines a data type called PLongRange (pointer to a long range). A
/// variable of type PLongRange is used to extract the minimum and maximum
/// values that can be passed as a quality setting to the JPEG encoder.

procedure TDemoEncoderParameters.ShowJpegQualityParameter(
  const Params: IGPEncoderParameters);
type
  TLongRange = record
    Min: LongInt;
    Max: LongInt;
  end;
  PLongRange = ^TLongRange;
var
  Param: PGPNativeEncoderParameter;
  LongRange: PLongRange;
begin
  Assert(Params.Count >= 2);
  Param := Params[1];
  LongRange := Param.Value;
  TextOutput.Add(Format('  The minimum possible quality value is %d.', [LongRange.Min]));
  TextOutput.Add(Format('  The maximum possible quality value is %d.', [LongRange.Max]));
end;

/// The preceding code should produce the following output:
///
///  The minimum possible quality value is 0.
///  The maximum possible quality value is 100.
///
/// In the preceding example, the value returned in the <A>TEncoderParameter</A>
/// record is a pair of LongInt values that indicate the minimum and maximum
/// possible values for the quality parameter. In some cases, the values
/// returned in an <A>TEncoderParameter</A> record are members of the
/// <A>TEncoderValue</A> enumeration. The following examples discuss the
/// <A>TEncoderValue</A> enumeration and methods for listing possible parameter
/// values in more detail:
///
///  -<A>Using the TEncoderValue Enumeration</A>
///  -<A>Listing Parameters and Values for All Encoders</A>
{$ENDREGION}

class function TDemoEncoderParameters.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

procedure TDemoEncoderParameters.Run;
var
  Params: IGPEncoderParameters;
begin
  Params := GetJpegParameters;
  TextOutput.Add('');
  ShowSecondJpegParameter(Params);
  TextOutput.Add('');
  ShowJpegQualityParameter(Params);
end;

initialization
  RegisterDemo('Using Image Encoders and Decoders\Encoder Parameters\Determining the Parameters Supported by an Encoder', TDemoEncoderParameters);

end.
