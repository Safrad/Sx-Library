unit uCustomBuffer;

interface

uses
  uTypes,
  uAlignedMemory,
  uRatioValue;

type
  TCustomBuffer = class
  private
    function GetBufferSize: UG;
  protected
    FAlignedMemory: TAlignedMemory;
    FBufferAllocation: TRatioValue;
    procedure SetBufferSize(const Value: UG); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property BufferSize: UG read GetBufferSize write SetBufferSize;
  end;

implementation

uses
  uCPU;

{ TBuffer }

constructor TCustomBuffer.Create;
begin
  inherited;

	FAlignedMemory := TAlignedMemory.Create;
  FAlignedMemory.AlignSize := GCPU.PageSize;
end;

destructor TCustomBuffer.Destroy;
begin
	FAlignedMemory.Free;

  inherited;
end;

function TCustomBuffer.GetBufferSize: UG;
begin
  Result := FBufferAllocation.Total;
end;

procedure TCustomBuffer.SetBufferSize(const Value: UG);
begin
  if Value <> UG(FAlignedMemory.Size) then
  begin
    FAlignedMemory.Size := Value;
  end;
end;

end.
