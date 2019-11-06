unit uSequentialWriteBuffer;
{
	File   |********************************|
	Buffer                 |*******|
}

interface

uses
  uTypes,
  uCustomBuffer;

type
  TOnWriteData = procedure(const AData: PByte; const ASize: UG) of object;

  TSequentialWriteBuffer = class(TCustomBuffer)
  private
    // Local
    FFilePosition: U8;
    FOnWriteData: TOnWriteData;

    // Properties
    procedure SetOnWriteData(const Value: TOnWriteData);
  protected
    procedure SetBufferSize(const Value: UG); override;
  public
    property OnWriteData: TOnWriteData read FOnWriteData write SetOnWriteData;

    procedure WriteData(const AData: Pointer; const ASize: UG);
    procedure Save;
  end;

implementation

uses
  uCPU;

{ TSequentialWriteBuffer }

procedure TSequentialWriteBuffer.Save;
begin
  if FBufferAllocation.Used > 0 then
  begin
    OnWriteData(FAlignedMemory.Data, FBufferAllocation.Used);
    FBufferAllocation.Used := 0;
  end;
end;

procedure TSequentialWriteBuffer.SetBufferSize(const Value: UG);
begin
  if Value <> UG(FAlignedMemory.Size) then
  begin
    Save;
    FAlignedMemory.Size := Value;
    FBufferAllocation.Total := Value;
  end;
end;

procedure TSequentialWriteBuffer.SetOnWriteData(const Value: TOnWriteData);
begin
  FOnWriteData := Value;
end;

procedure TSequentialWriteBuffer.WriteData(const AData: Pointer; const ASize: UG);
var
  Data: PByte;
  RemainWriteCount: UG;
  WriteSize: UG;
  SaveBuf: BG;
begin
  Data := PByte(AData);

  RemainWriteCount := ASize;
  WriteSize := ASize;

  while RemainWriteCount > 0 do
  begin
    if WriteSize >= FBufferAllocation.Remain then
    begin
      WriteSize := FBufferAllocation.Remain;
      SaveBuf := True;
    end
    else
    begin
      SaveBuf := False;
    end;

    // Transfer buffer data
    Move(Data^, PByte(PByte(FAlignedMemory.Data) + FBufferAllocation.Used)^, WriteSize);
    FBufferAllocation.Used := FBufferAllocation.Used + S8(WriteSize);

    if SaveBuf then
      Save
    else
      Break;

    Inc(Data, WriteSize);
    Dec(RemainWriteCount, WriteSize);
  end;
  Inc(FFilePosition, ASize);
end;

end.
