unit uReadBuffer;
// For random access change buffer size to 4 KB

{
	File   |********************************|
	Buffer                 |*******|
}

interface

uses
  uTypes,
  uCustomBuffer;

type
  TOnSeek = procedure(const APosition: U8) of object;
  TOnReadData = procedure(const AData: PByte; const ASize: UG) of object;

  TReadBuffer = class(TCustomBuffer)
  private
    // Local
    FFilePosition: U8;
    FBufferPos: S8;

    FOnSeek: TOnSeek;
    FOnReadData: TOnReadData;
    FDataCount: S8;
    procedure ChangeBufferIfNeeded;
    procedure ChangeBuffer;
    procedure ReadNextBuffer;

    // Properties
    procedure SetOnReadData(const Value: TOnReadData);
    procedure SetDataCount(const Value: S8);
    procedure SetOnSeek(const Value: TOnSeek);
  public
    property OnSeek: TOnSeek read FOnSeek write SetOnSeek;
    property OnReadData: TOnReadData read FOnReadData write SetOnReadData;
    property FilePosition: U8 read FFilePosition;
    property DataCount: S8 read FDataCount write SetDataCount;

    procedure Seek(const APosition: U8);
    procedure ReadData(const AData: Pointer; const ASize: UG);
  end;

implementation

uses
  SysUtils,
  Math,

  uMath,
  uCPU;

{ TSequentialReadBuffer }

procedure TReadBuffer.ReadData(const AData: Pointer; const ASize: UG);
var
  Data: PByte;
  RemainReadCount, ReadSize: UG;
begin
  if ASize > UG(U8(FDataCount) - FFilePosition) then
    ClearMemory(AData^, ASize);
// TODO : raise exception is 30% slowest in the case if is not called
{    raise EInvalidArgument.Create('Not enought requested ' + IntToStr(ASize) + ' bytes, ' +
      IntToStr(U8(DataCount) - FFilePosition) + ' remains');}

  Data := PByte(AData);
  RemainReadCount := ASize;
  while RemainReadCount > 0 do
  begin
    if FBufferAllocation.Remain = 0 then
      ReadNextBuffer;

    ReadSize := Min(FBufferAllocation.Remain, S8(RemainReadCount));

    // Transfer buffer data
    Move(PByte(PByte(FAlignedMemory.Data) + FBufferAllocation.Used)^, Data^, ReadSize);
    FBufferAllocation.Used := U8(FBufferAllocation.Used) + ReadSize;

    Inc(Data, ReadSize);
    Dec(RemainReadCount, ReadSize);
  end;
  Inc(FFilePosition, ASize);
end;

procedure TReadBuffer.ChangeBuffer;
begin
  FBufferPos := (FFilePosition div U8(FAlignedMemory.Size)) * U8(FAlignedMemory.Size);
  OnSeek(FBufferPos);
  // Invalidate buffer
  FBufferAllocation.Used := 0;
  FBufferAllocation.Total := 0;
  Assert(FBufferAllocation.Remain = 0);
end;

procedure TReadBuffer.ChangeBufferIfNeeded;
begin
  if (FFilePosition < FBufferPos) or (FFilePosition >= FBufferPos + FBufferAllocation.Total) then
  begin
    ChangeBuffer;
  end
  else
    FBufferAllocation.Used := S8(FFilePosition) - FBufferPos;
end;

procedure TReadBuffer.ReadNextBuffer;
begin
  Inc(FBufferPos, FBufferAllocation.Total);
  // No seek required (sequential read)
  FBufferAllocation.Total := Min(FAlignedMemory.Size, FDataCount - FBufferPos);
  OnReadData(FAlignedMemory.Data, FBufferAllocation.Total);
  FBufferAllocation.Used := 0;
end;

procedure TReadBuffer.Seek(const APosition: U8);
begin
  if FFilePosition <> APosition then
  begin
    FFilePosition := APosition;
    ChangeBufferIfNeeded;
  end;
end;

procedure TReadBuffer.SetDataCount(const Value: S8);
begin
  FDataCount := Value;
end;

procedure TReadBuffer.SetOnReadData(const Value: TOnReadData);
begin
  FOnReadData := Value;
end;

procedure TReadBuffer.SetOnSeek(const Value: TOnSeek);
begin
  FOnSeek := Value;
end;

end.
