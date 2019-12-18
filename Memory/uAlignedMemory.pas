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

uses
  uMath;

{ TAlignedMemory }

procedure TAlignedMemory.Clear;
begin
  ClearMemory(Data^, Size);
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
  if FSize > 0 then
  begin
    GetMem(FBuffer, FSize + FAlignSize - 1);
    FData := Pointer((NativeInt(FBuffer) + FAlignSize - 1) and (not (FAlignSize - 1)));
  end;
end;

end.
