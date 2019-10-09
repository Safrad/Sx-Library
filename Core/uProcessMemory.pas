unit uProcessMemory;

interface

uses
  uTypes;

type
  TProcessMemoryCounters = record
    WorkingSetSize: U8;
    PeekWorkingSetSize: U8;

    procedure Add(AProcessMemoryCounters: TProcessMemoryCounters);
  end;

function GetProcessMemoryCounters(const AHandle: THandle): TProcessMemoryCounters;
function GetProcessMemoryCountersRecursive(const ARootProcessId: U4): TProcessMemoryCounters;

implementation

uses
  SysUtils,

  WinApi.Windows,
  WinApi.PsApi,

  uProcessInfos;

function GetProcessMemoryCounters(const AHandle: THandle): TProcessMemoryCounters;
var
  ProcessMemoryCounters: _PROCESS_MEMORY_COUNTERS;
begin
  FillChar(ProcessMemoryCounters, SizeOf(ProcessMemoryCounters), 0);
  ProcessMemoryCounters.cb := SizeOf(ProcessMemoryCounters);
  if not GetProcessMemoryInfo(AHandle, @ProcessMemoryCounters, ProcessMemoryCounters.cb) then
    RaiseLastOSError;
  Result.WorkingSetSize := ProcessMemoryCounters.WorkingSetSize;
  Result.PeekWorkingSetSize := ProcessMemoryCounters.PeakWorkingSetSize;
end;

function GetProcessMemoryCountersRecursive(const ARootProcessId: U4): TProcessMemoryCounters;
var
  Handle: THandle;
  ProcessInfo: TProcessInfoItem;
begin
  Handle := OpenProcess(PROCESS_QUERY_INFORMATION, False, ARootProcessId);
  Result := GetProcessMemoryCounters(Handle);

  for ProcessInfo in ProcessInfos.CompleteList do
  begin
    if ProcessInfo.ParentProcessId = ARootProcessId then
    begin
      Result.Add(GetProcessMemoryCountersRecursive(ProcessInfo.ProcessId));
    end;
  end;
end;

{ TProcessMemoryCounters }

procedure TProcessMemoryCounters.Add(AProcessMemoryCounters: TProcessMemoryCounters);
begin
  Inc(WorkingSetSize, AProcessMemoryCounters.WorkingSetSize);
  Inc(PeekWorkingSetSize, AProcessMemoryCounters.PeekWorkingSetSize);
end;

end.
