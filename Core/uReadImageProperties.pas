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
  Classes;

function ReadMotorolaWord(const AStream: TStream): Word;
begin
  AStream.Read(Result, SizeOf(Result));
  Result := Swap(Result);
end;

procedure ReadJpegImageProperties(AStream: TStream; var AProperties: TImageProperties);
const
  ValidSig : array[0..1] of byte = ($FF, $D8);
  Parameterless = [$01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7];
var
  Sig: array[0..1] of byte;
  x: integer;
  Seg: byte;
  Dummy: array[0..15] of byte;
  Len: word;
  ReadLen: LongInt;
begin
  FillChar(Sig, SizeOf(Sig), #0);
  ReadLen := AStream.Read(Sig[0], SizeOf(Sig));
  for x := Low(Sig) to High(Sig) do
    if Sig[x] <> ValidSig[x] then
      ReadLen := 0;
  if ReadLen > 0 then
  begin
    ReadLen := AStream.Read(Seg, 1);
    while (Seg = $FF) and (ReadLen > 0) do
    begin
      ReadLen := AStream.Read(Seg, 1);
      if Seg <> $FF then
      begin
        if (Seg = $C0) or (Seg = $C1) then
        begin
          ReadLen := AStream.Read(Dummy[0], 3);  // don't need these bytes
          AProperties.Height := ReadMotorolaWord(AStream);
          AProperties.Width := ReadMotorolaWord(AStream);
        end
        else
        begin
          if not (Seg in Parameterless) then
          begin
            Len := ReadMotorolaWord(AStream);
            AStream.Seek(Len - 2, soFromCurrent);
            AStream.Read(Seg, soFromCurrent);
          end
          else
            Seg := $FF;  // Fake it to keep looping.
        end;
      end;
    end;
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
