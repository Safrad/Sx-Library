unit SafeMMInstall;

interface

{.$MESSAGE warn 'SafeMM on'}
{$WARN SYMBOL_PLATFORM OFF}


implementation

uses
  SafeMM;

var
  FOldManager: TMemoryManagerEx;

procedure InstallSafeMemoryManager;
begin
 Assert(GetHeapStatus.TotalAllocated=0);
 GetMemoryManager(FOldManager);

 SetMemoryManager(SafeMemoryManager);

 SafeMMPrepare;
end;

procedure UninstallSafeMemoryManager;
begin
 SetMemoryManager(FOldManager);
end;

initialization

  InstallSafeMemoryManager;

finalization

  UninstallSafeMemoryManager;

end.
