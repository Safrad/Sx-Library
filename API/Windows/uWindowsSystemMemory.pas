unit uWindowsSystemMemory;

interface

uses
  uTypes,
  uCustomSystemMemory;

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

  TWindowsSystemMemory = class(TCustomSystemMemory)
  public
    procedure Update; override;
    function ProcessAllocatedVirtualMemory: U8; override;
  end;

implementation

uses
  SysUtils,
  Math,

  Winapi.Windows,
  Winapi.psAPI;

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

{ TWindowsSystemMemory }

function TWindowsSystemMemory.ProcessAllocatedVirtualMemory: U8;
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

procedure TWindowsSystemMemory.Update;
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
  PageFile.Remain := Max(0, Virtual.Remain - Physical.Remain);
end;

end.
