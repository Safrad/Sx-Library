{$IMAGEBASE $00D20000}

library BorlndMM;

uses
  SafeMM in 'SafeMM.pas',  // See this file for usage information
  Windows;

{$R *.RES}

function GetHeapStatus: THeapStatus;
begin
  with Result do
  begin
    TotalAddrSpace := 0;
    TotalUncommitted := 0;
    TotalCommitted := 0;
    TotalAllocated := 0;
    TotalFree := 0;
    FreeSmall := 0;
    FreeBig := 0;
    Unused := 0;
    Overhead := 0;
    HeapErrorCode := 0;
  end;
end;

function GetAllocMemCount: integer;
begin
  Result := 0;
end;

function GetAllocMemSize: integer;
begin
  Result := 0;
end;

procedure DumpBlocks;
begin
  {Do nothing}
end;

exports
  GetAllocMemSize name 'GetAllocMemSize',
  GetAllocMemCount name 'GetAllocMemCount',
  GetHeapStatus name 'GetHeapStatus',
  DumpBlocks name 'DumpBlocks',
  SafeMM.SafeReallocMem name '@Borlndmm@SysReallocMem$qqrpvi',
  SafeMM.SafeFreeMem name '@Borlndmm@SysFreeMem$qqrpv',
  SafeMM.SafeGetMem name '@Borlndmm@SysGetMem$qqri';

begin
  IsMultiThread := True;
  SafeMMPrepare;
end.
