unit uReadImageProperties;

interface

uses
  GraphicEx;

type
  TImageProperties = GraphicEx.TImageProperties;

function ReadImageProperties(const AFileName: string; var AProperties: TImageProperties): Boolean;

implementation

uses
  SysUtils,
  Classes,

  GraphicColor,

  uTypes,
  uEParseError;

procedure ReadJpegImageProperties(const AStream: TStream; var AProperties: TImageProperties);

  procedure ReadData(var ABuffer; const ACount: SG);
  var
    ReadedCount: SG;
  begin
    ReadedCount := AStream.Read(ABuffer, ACount);
    if ReadedCount <> ACount then
      raise EParseError.Create('Input is truncated.');
  end;

const
  SegmentPrefix = $FF;

  StartOfInput = $D8; // SOI / Header
  EndOfInput = $D9; // EOI
  StartOfScan = $DA; // SOS
  SOF0Segments = [$C0, $C1, $C2, $C3];
  TEM = $01;

  RST0 = $D0;
  RST1 = $D1;
  RST2 = $D2;
  RST3 = $D3;
  RST4 = $D4;
  RST5 = $D5;
  RST6 = $D6;
  RST7 = $D7;

  Parameterless = [TEM, RST0, RST1, RST2, RST3, RST4, RST5, RST6, RST7, StartOfInput, EndOfInput];
type
  TNumberOfComponents = (ncGreyScaled = 1, ncYCbCr{or YIQ} = 3, ncCMYK = 4);

  TSOF0 = packed record
    Length: U2;
    DataPrecision: U1; // Bits / sample
    ImageHeight: U2;
    ImageWidth: U2;
    NumberOfComponents: TNumberOfComponents;
  end;
var
  SOF0: TSOF0;
  SegmentId: TU2;
  SegmentSize: U2;
begin
  while True do
  begin
    ReadData(SegmentId, SizeOf(SegmentId));
    if SegmentId.B0 <> SegmentPrefix then
      raise EParseError.Create('Invalid segment id.');

    if SegmentId.B1 in SOF0Segments then
    begin
      ReadData(SOF0, SizeOf(SOF0));
      AProperties.Height := Swap(SOF0.ImageHeight);
      AProperties.Width := Swap(SOF0.ImageWidth);
      case SOF0.NumberOfComponents of
        ncGreyScaled: AProperties.ColorScheme := csG;
        ncYCbCr: AProperties.ColorScheme := csYCbCr;
        ncCMYK: AProperties.ColorScheme := csCMYK;
      end;
      AProperties.BitsPerSample := SOF0.DataPrecision;
      Break; // Skip other segments
//      AStream.Seek(Swap(SOF0.Length) - SizeOf(SOF0), soFromCurrent);
    end
    else if not (SegmentId.B1 in Parameterless) then
    begin
      ReadData(SegmentSize, SizeOf(SegmentSize));

      SegmentSize := Swap(SegmentSize);

      if SegmentId.B1 = StartOfScan then
        Exit;

      AStream.Seek(SegmentSize - SizeOf(SegmentSize), soFromCurrent);
    end
    else if (SegmentId.B1 = EndOfInput) then
      Exit;
  end;
end;

function ReadImageProperties(const AFileName: string; var AProperties: TImageProperties): Boolean;
var
  Ext: string;
  Stream: TFileStream;
begin
  Ext := UpperCase(ExtractFileExt(AFileName));
  if (Ext = '.JPG') or (Ext = '.JPEG') or (Ext = '.JFIF') then
  begin
    // Not supported by GraphicEx
    FillChar(AProperties, SizeOf(AProperties), 0);
    Stream := TFileStream.Create(AFileName, fmOpenRead);
    try
      ReadJpegImageProperties(Stream, AProperties);
      Result := True;
    finally
      Stream.Free;
    end;
  end
  else
    Result := GraphicEx.ReadImageProperties(AFileName, AProperties);
end;

end.
