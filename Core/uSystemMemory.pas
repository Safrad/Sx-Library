unit uSystemMemory;

interface

uses
  uTypes,
  uRatioValue;

type
  {$if CompilerVersion < 20}
  PMemoryStatusEx = ^TMemoryStatusEx;
  LPMEMORYSTATUSEX = PMemoryStatusEx;
  {$EXTERNALSYM LPMEMORYSTATUSEX}
  _MEMORYSTATUSEX = packed record
    dwLength : U4;
    dwMemoryLoad : U4;
    ullTotalPhys : U8;
    ullAvailPhys : U8;
    ullTotalPageFile: U8;
    ullAvailPageFile: U8;
    ullTotalVirtual : U8;
    ullAvailVirtual : U8;
    ullAvailExtendedVirtual : U8;
  end;
  {$EXTERNALSYM _MEMORYSTATUSEX}
  TMemoryStatusEx = _MEMORYSTATUSEX;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  {$EXTERNALSYM MEMORYSTATUSEX}
  {$ifend}

  TSystemMemory = class
  private
    FVirtual: TRatioValue;
    FPhysical: TRatioValue;
    FPageFile: TRatioValue;
    procedure SetPageFile(const Value: TRatioValue);
    procedure SetPhysical(const Value: TRatioValue);
    procedure SetVirtual(const Value: TRatioValue);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Update;
    function MaxPhysicalMemorySize: U8;
    function ProcessAllocatedVirtualMemory: U8;
    function CanAllocateMemory(const Size: UG): BG;

    property Physical: TRatioValue read FPhysical write SetPhysical;
    property PageFile: TRatioValue read FPageFile write SetPageFile;
    property Virtual: TRatioValue read FVirtual write SetVirtual;
  end;

function SystemMemory: TSystemMemory;

implementation

uses
  SysUtils,
  Windows,
  Math,
  psAPI;

var
  GSystemMemory: TSystemMemory;

function SystemMemory: TSystemMemory;
begin
  Result := GSystemMemory;
end;

{$if CompilerVersion < 20}
procedure GlobalMemoryStatus(var lpBuffer: TMemoryStatus); stdcall;
  external kernel32;
{$EXTERNALSYM GlobalMemoryStatus}

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall;
type
  TFNGlobalMemoryStatusEx = function(var msx: TMemoryStatusEx): BOOL; stdcall;
var
  FNGlobalMemoryStatusEx: TFNGlobalMemoryStatusEx;
begin
  FNGlobalMemoryStatusEx := TFNGlobalMemoryStatusEx(
    GetProcAddress(GetModuleHandle(kernel32), 'GlobalMemoryStatusEx'));
  if not Assigned(FNGlobalMemoryStatusEx) then
  begin
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result := False;
  end
  else
  begin
    Result := FNGlobalMemoryStatusEx(lpBuffer);
  end;
end;
{$ifend}

{ TSystemMemory }

{
function MaxAllocationSize: U8;
begin
	FillMemoryStatus(MS);
  Result := Max(0, MaxPhysicalMemorySize - ProcessAllocatedVirtualMemory);

  Result := 2 * Result div 3; // Fragmentation
end;
}

function TSystemMemory.CanAllocateMemory(const Size: UG): BG;
const
  ReservedSize = 8 * MB;
var
  P: Pointer;
begin
//  Result := Size + ReservedSize < MaxAllocationSize;
  try
    GetMem(P, Size + ReservedSize);
    Result := P <> nil;
    FreeMem(P);
  except
    Result := False;
  end;
end;

constructor TSystemMemory.Create;
begin
  inherited;

  FPhysical := TRatioValue.Create;
  FPageFile := TRatioValue.Create;
  FVirtual := TRatioValue.Create;
end;

destructor TSystemMemory.Destroy;
begin
  FVirtual.Free;
  FPageFile.Free;
  FPhysical.Free;

  inherited;
end;

function TSystemMemory.MaxPhysicalMemorySize: U8;
begin
	Update;
  Result := Min(2 * Physical.Used div 3 {66%}, Virtual.Total);
end;

function TSystemMemory.ProcessAllocatedVirtualMemory: U8;
var
  MemCounters: TProcessMemoryCounters;
begin
  MemCounters.cb := SizeOf(MemCounters);
  Result := 0;
  if GetProcessMemoryInfo(GetCurrentProcess,
      @MemCounters,
      SizeOf(MemCounters)) then
  begin
    // MemCounters.PagefileUsage is defined as SIZE_T (size is 4 bytes in 32 bit version and 8 bytes in 64 bit version)
    Result := MemCounters.PagefileUsage;
  end
  else
    RaiseLastOSError;
end;

procedure TSystemMemory.SetPageFile(const Value: TRatioValue);
begin
  FPageFile := Value;
end;

procedure TSystemMemory.SetPhysical(const Value: TRatioValue);
begin
  FPhysical := Value;
end;

procedure TSystemMemory.SetVirtual(const Value: TRatioValue);
begin
  FVirtual := Value;
end;

procedure TSystemMemory.Update;
var
  MemoryStatusEx: TMemoryStatusEx;
begin
	MemoryStatusEx.dwLength := SizeOf(MemoryStatusEx);
	GlobalMemoryStatusEx(MemoryStatusEx);
  Physical.Total := MemoryStatusEx.ullTotalPhys;
  Physical.Remain := MemoryStatusEx.ullAvailPhys;

  Virtual.Total := MemoryStatusEx.ullTotalPageFile;
  Virtual.Remain := MemoryStatusEx.ullAvailPageFile;

  PageFile.Total := Virtual.Total - Physical.Total;
  PageFile.Remain := Virtual.Remain - Physical.Remain;
end;

//function MMUsedMemory: U8;
//var
//    st: TMemoryManagerState;
//    sb: TSmallBlockTypeState;
//    i: SG;
//begin
//  GetMemoryManagerState(st);
//  Result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
//  for i := Low(st.SmallBlockTypeStates) to High(st.SmallBlockTypeStates) do
//  begin
//    sb := st.SmallBlockTypeStates[i];
//      Inc(Result, sb.UseableBlockSize * sb.AllocatedBlockCount);
//  end;
//end;

initialization
{$IFNDEF NoInitialization}
  GSystemMemory := TSystemMemory.Create;
  GSystemMemory.Update;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GSystemMemory);
{$ENDIF NoFinalization}
end.
