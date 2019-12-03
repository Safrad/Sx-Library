unit uAndroidSystemMemory;

interface

uses
  uTypes,
  uCustomSystemMemory;

type
  TAndroidSystemMemory = class(TCustomSystemMemory)
  public
    procedure Update; override;
    function ProcessAllocatedVirtualMemory: U8; override;
  end;

implementation

uses
  Math;

{ TAndroidSystemMemory }

function TAndroidSystemMemory.ProcessAllocatedVirtualMemory: U8;
begin
  Result := 0;
end;

procedure TAndroidSystemMemory.Update;
begin
  // TODO : Get from system
  Physical.Total := 3 * S8(GB);
  Physical.Remain := 1 * S8(GB);

  Virtual.Total := 4 * S8(GB);
  Virtual.Remain := 2 * S8(GB);

  PageFile.Total := Virtual.Total - Physical.Total;
  PageFile.Remain := Max(0, Virtual.Remain - Physical.Remain);
end;

end.
