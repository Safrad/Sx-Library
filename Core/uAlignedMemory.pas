unit uAlignedMemory;

interface

uses
  uTypes;

type
  TAlignedMemory = class
  private
    FBuffer: Pointer;
    FData: Pointer;
    FSize: SG;
    FAlignSize: SG;
    procedure Update;
    procedure SetSize(const Value: SG);
    procedure SetAlignSize(const Value: SG);
  public
    destructor Destroy; override;

    procedure Clear;
    property Size: SG read FSize write SetSize;
    property AlignSize: SG read FAlignSize write SetAlignSize;
    property Data: Pointer read FData;
  end;

implementation

{ TAlignedMemory }

procedure TAlignedMemory.Clear;
begin
  FillChar(Data^, Size, 0);
end;

destructor TAlignedMemory.Destroy;
begin
  FreeMem(FBuffer);

  inherited;
end;

procedure TAlignedMemory.SetAlignSize(const Value: SG);
begin
  if FAlignSize <> Value then
  begin
    FAlignSize := Value;
    Update;
  end;
end;

procedure TAlignedMemory.SetSize(const Value: SG);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Update;
  end;
end;

procedure TAlignedMemory.Update;
begin
  FreeMem(FBuffer);
  FBuffer := nil;
  FData := nil;
  if Size > 0 then
  begin
    GetMem(FBuffer, Size + AlignSize - 1);
    FData := Pointer((NativeInt(FBuffer) + AlignSize - 1) and (not (AlignSize - 1)));
  end;
end;

end.
