unit uBufferedRawFile;

interface

uses
  uTypes,
  uReadBuffer,
  uSequentialWriteBuffer,
  uProtectedRawFile,
  uRawFile;

type
  TBufferedRawFile = class(TProtectedRawFile)
  private
    FSequentialReadBuffer: TReadBuffer;
    FSequentialWriteBuffer: TSequentialWriteBuffer;
    procedure OnSeek(const APosition: U8);
    procedure OnReadData(const AData: PByte; const ASize: UG);
    procedure OnWriteData(const AData: PByte; const ASize: UG);
  protected
    procedure SetFileMode(const AFileMode: TFileMode); override;
  public
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
    function Eof: BG; override;

    function ReadU1: U1;
    function ReadU2: U2;
    function ReadU4: U4;
    function ReadU8: U8;
    function ReadAnsiChar: AnsiChar;
    function ReadWideChar: WideChar;

    procedure WriteU1(const AValue: U1);
    procedure WriteU2(const AValue: U2);
    procedure WriteU4(const AValue: U4);
    procedure WriteU8(const AValue: U8);
    procedure WriteAnsiChar(const AValue: AnsiChar);
    procedure WriteWideChar(const AValue: WideChar);

    procedure BlockRead(const AData: Pointer; const ASize: UG); override;
    procedure BlockWrite(const AData: Pointer; const ASize: UG); override;
		procedure Seek(const APosition: U8); override;
    procedure FlushFileBuffers; override;
  end;

implementation

uses
  Math,
  SysUtils;

{ TBufferedRawFile }

procedure TBufferedRawFile.BlockRead(const AData: Pointer; const ASize: UG);
begin
  if Assigned(FSequentialReadBuffer) then
    FSequentialReadBuffer.ReadData(AData, ASize)
  else
    inherited BlockRead(AData, ASize);
end;

procedure TBufferedRawFile.BlockWrite(const AData: Pointer; const ASize: UG);
begin
  if Assigned(FSequentialWriteBuffer) then
    FSequentialWriteBuffer.WriteData(AData, ASize)
  else
    inherited BlockWrite(AData, ASize);
end;

procedure TBufferedRawFile.Close;
begin
  try
    if FSequentialWriteBuffer <> nil then
    begin
      FSequentialWriteBuffer.Save;
      FreeAndNil(FSequentialWriteBuffer);
    end;
  finally
    inherited;
  end;
end;

destructor TBufferedRawFile.Destroy;
begin
  try
  	FreeAndNil(FSequentialReadBuffer);
  	FreeAndNil(FSequentialWriteBuffer);
  finally
    inherited;
  end;
end;

function TBufferedRawFile.Eof: BG;
begin
  if Assigned(FSequentialReadBuffer) then
    Result := FSequentialReadBuffer.FilePosition >= FSequentialReadBuffer.DataCount
  else
    Result := inherited;
end;

procedure TBufferedRawFile.FlushFileBuffers;
begin
  FSequentialWriteBuffer.Save;

  inherited;
end;

procedure TBufferedRawFile.OnReadData(const AData: PByte; const ASize: UG);
begin
  inherited BlockRead(AData, ASize);
end;

procedure TBufferedRawFile.OnSeek(const APosition: U8);
begin
  inherited Seek(APosition);
end;

procedure TBufferedRawFile.OnWriteData(const AData: PByte; const ASize: UG);
begin
  inherited BlockWrite(AData, ASize);
end;

procedure TBufferedRawFile.Open;
begin
  inherited; // Initialize FileSize

  if (FSequentialReadBuffer <> nil) and (FileMode in [fmReadOnly]) then
  begin
    FSequentialReadBuffer.BufferSize := Min(DefFileBuffer, FileSize);
    FSequentialReadBuffer.DataCount := FileSize;
  end;
end;

function TBufferedRawFile.ReadAnsiChar: AnsiChar;
begin
  BlockRead(@Result, SizeOf(Result));
end;

function TBufferedRawFile.ReadU1: U1;
begin
  BlockRead(@Result, SizeOf(Result));
end;

function TBufferedRawFile.ReadU2: U2;
begin
  BlockRead(@Result, SizeOf(Result));
end;

function TBufferedRawFile.ReadU4: U4;
begin
  BlockRead(@Result, SizeOf(Result));
end;

function TBufferedRawFile.ReadU8: U8;
begin
  BlockRead(@Result, SizeOf(Result));
end;

function TBufferedRawFile.ReadWideChar: WideChar;
begin
  BlockRead(@Result, SizeOf(Result));
end;

procedure TBufferedRawFile.Seek(const APosition: U8);
begin
  if APosition >= FileSize then
    raise EInvalidArgument.Create('Seek position is after end of file');

  if FileMode in [fmAppend, fmRewrite] then
    raise Exception.Create('Seek not supported.');

  if Assigned(FSequentialReadBuffer) then
    FSequentialReadBuffer.Seek(APosition)
  else
    inherited;
end;

procedure TBufferedRawFile.SetFileMode(const AFileMode: TFileMode);
begin
  inherited;

  if AFileMode in [fmReadOnly] then
  begin
    if FSequentialReadBuffer = nil then
    begin
      FSequentialReadBuffer := TReadBuffer.Create;
      FSequentialReadBuffer.OnSeek := OnSeek;
      FSequentialReadBuffer.OnReadData := OnReadData;
      FSequentialReadBuffer.DataCount := FileSize;
    end;
  end
  else if FSequentialReadBuffer <> nil then
    FreeAndNil(FSequentialReadBuffer);

  if AFileMode in [fmRewrite, fmAppend] then
  begin
    if FSequentialWriteBuffer = nil then
    begin
      FSequentialWriteBuffer := TSequentialWriteBuffer.Create;
      FSequentialWriteBuffer.OnWriteData := OnWriteData;
      FSequentialWriteBuffer.BufferSize := DefFileBuffer
    end;
  end
  else if FSequentialWriteBuffer <> nil then       
  begin
    FSequentialWriteBuffer.Save;
    FreeAndNil(FSequentialWriteBuffer);
  end;
end;

procedure TBufferedRawFile.WriteAnsiChar(const AValue: AnsiChar);
begin
  BlockWrite(@AValue, SizeOf(AValue));
end;

procedure TBufferedRawFile.WriteU1(const AValue: U1);
begin
  BlockWrite(@AValue, SizeOf(AValue));
end;

procedure TBufferedRawFile.WriteU2(const AValue: U2);
begin
  BlockWrite(@AValue, SizeOf(AValue));
end;

procedure TBufferedRawFile.WriteU4(const AValue: U4);
begin
  BlockWrite(@AValue, SizeOf(AValue));
end;

procedure TBufferedRawFile.WriteU8(const AValue: U8);
begin
  BlockWrite(@AValue, SizeOf(AValue));
end;

procedure TBufferedRawFile.WriteWideChar(const AValue: WideChar);
begin
  BlockWrite(@AValue, SizeOf(AValue));
end;

end.
