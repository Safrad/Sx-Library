(*

SafeMM Debug Memory Manager 0.4

  (C) Ben Taylor

  based on code from:
  (C) 2000 Per B. Larsen & TurboPower Software Co.
  All rights reserved.
  Used with permission.

Description:
 A "proof of concept" unit for using OS functionality to raise an
 AV when an improper attempt is made to read or write memory.

Usage:
 To use SafeMM as a replacement memory manager in a standalone application
 add the SafeMMInstall as the first unit in the project uses clause.
 Then run the application under the debugger.  If an invalid
 memory access is detected an Access Violation will be generated.

 For applications using ShareMem, compile the borlndmm.dpr project
 and copy the resulting borlndmm.dll into the application bin directory.

 To use SafeMM with the IDE (BDS.exe), backup and replace the borlndmm.dll
 file in the directory where BDS.exe is installed.

Notes:
 - A problem is that many functions require/assume 4/8-byte
   alignment. enabling alignment in the mm means that its guarding
   ability is reduced.

 - A typical app might experience 4x the normal memory usage.
   Depending on how many small-block pools are preallocated, there may
   also be a minimum memory usage of about 60Mb.

 - When used with the IDE (version 8 or later), disable the exceptiondiag
   IDE package to prevent an infinite loop if an unhandled exception is raised.
   To do this, rename the exceptiondiagXXX.bpl file in the diretory with
   bds.exe at the same time borlndmm.dll is replaced.
   When the IDE is started, reply "yes" to the missing package prompt.
   When restoring the original borlndmm.dll file, re-enable the exceptdiag
   package by removing the registry entry under:

    HKEY_CURRENT_USER\Software\CodeGear\BDS\X.0\Disabled IDE Packages

- Because of the increased memory requirements, when running the IDE,
  it may be necessary to disable unused IDE personalities and features
  to avoid out of memory errors.  Out of memory errors may also occur
  after running the application for a period of time.

Examples:
 //with alignment=8
 var
  p:pchar;
 begin
  p:=AllocMem(2);
  p[0]:='a';
  p:=ReallocMem(p,3);
  Assert(p[0]='a');
  p[1]:='b';
  //ideally should fail, but wont due to alignment
  p[3]:='b';
  //should give a write-av
  p[10]:='c';
  //should give a read-av
  if p[10]='c' then ...

Small Block Layout:
 64k is divided into 16 x 4k blocks:

 info/g/mem1/g/mem2/g/mem3/g/mem4/g/mem5/g/mem6/g/mem7/g

 info  : contains info about allocation of the small blocks
 g     : guard blocks. these are always marked as NoAccess
 memX  : returned by the memory routines.
         marked as NoAccess when not in use.
         marked as ReadWrite when allocated

 There are 7 info records in the info block.
 This gives about 500bytes/record.

 Good reason to not release pool back to os when empty is that you can
 store info about data that was previously there.

Change log:
 Version 0.1
  - Initial release

 Version 0.2
  - Improved memory usage.  Small blocks are now reused less frequently,
    leading to a higher chance that improper memory usage is caught.

 Version 0.3 (March 29, 2007)
  - Added support for delaying reuse of large-blocks.

 Version 0.4 (October 16, 2009)
  - Mark Edington (Embarcadero): Fixed compilation with older RTL that does
    not have TMemoryManagerEx defined.
    Moved SafeMM_readme.txt contents into this file and added addition notes.

Todo (from Ben):
 - Implement SafeMMMode again
 - store stack traces for the memory alloc/free calls
 - Add guard bytes to allow detection of modification in the area
   not used due to alignment
 - Implement data structures to allow better management of the
   allocated memory. this would allow:
   - walking a list of allocated large and small blocks
     ideally these structures should be marked as NoAccess
     unless being changed by the memory routines.
 - Rewrite by someone who knows this better than i do :)
 - Nice to have a callback like GetPointerInfo(p:pointer;info:pchar);
   this would allow the rtl to ask the memory-manager for additional info
   about the given pointer (eg stack trace), which can then be displayed
   in an error message to the user.

*)

unit SafeMM;

{$ASSERTIONS ON}

interface

{$IF RTLVersion >= 18.0}
  {$DEFINE MEMORY_MANAGER_EX}
{$IFEND}

type
  TSafeProtect = (spReadWrite,spReadOnly,spNoAccess);

function SafeGetMem(Size: Integer): Pointer;
function SafeFreeMem(P: Pointer): Integer;
function SafeReallocMem(P: Pointer; Size: Integer): Pointer;
function SafeAllocMem(ASize: Cardinal): Pointer;
function SafeRegisterExpectedMemoryLeak(P: Pointer): Boolean;
function SafeUnregisterExpectedMemoryLeak(P: Pointer): Boolean;

{
caller can specifically set protection on a block of memory.
this is useful eg if you have a block of memory that you know
shouldn't be written to.
}
procedure SafeMMProtect(const p:Pointer;const aProtect:TSafeProtect);

{
this preallocates a number of small-block pools.
this is done so that the same small-block isn't immediately
reused by the next memory request.
}
procedure SafeMMPrepare;

const

{$IFDEF MEMORY_MANAGER_EX}
  SafeMemoryManager: TMemoryManagerEx = (
   GetMem: SafeGetMem;
   FreeMem: SafeFreeMem;
   ReallocMem: SafeReallocMem;
   AllocMem: SafeAllocMem;
   RegisterExpectedMemoryLeak: SafeRegisterExpectedMemoryLeak;
   UnregisterExpectedMemoryLeak: SafeUnregisterExpectedMemoryLeak;
  );
{$ELSE}
  SafeMemoryManager: TMemoryManager = (
   GetMem: SafeGetMem;
   FreeMem: SafeFreeMem;
   ReallocMem: SafeReallocMem;
  );

type
  TMemoryManagerEx = TMemoryManager;  // Simple alias to avoid IFDEF in SafeMMInstall.
{$ENDIF}

type

  {
  selects how the returned block of memory is aligned inside its
  guard pages.

  eg header:
  Guard|returned block|trailing space|guard
  will protect against eg p[-1]:=nil;

  footer:
  Guard|leading space|returned block|guard
  will protect against eg p[size+100]:=nil;
  }
  TSafeGuardMode  = (
   gmHeader, //mostly protect against access before block
   gmFooter  //mostly protect against access after block
   );

var
  //0=no alignment
  SafeMMAlign:Integer;
  SafeMMMode:TSafeGuardMode;

implementation

uses
  Windows;

const
  //how many 4k blocks in a 64k pool
  cSubCount=7;

type

  PPoolInfo = ^TPoolInfo;
  TPoolInfo = record
    Pool:pointer;
    //is each subblock available?
    //could be better to use 7 bits and bitwise ops?
    //eg 01111111. then full=127,empty=0, inuse>0
    avail:array[0..cSubCount-1] of boolean;
    //helper routines
  end;

  PBlockInfo = ^TBlockInfo;
  TBlockInfo = record
    //indicate valid info
    Magic:Cardinal;
    //pool. nil=it was alloc seperately
    Pool:PPoolInfo;
    PoolIndex:integer;
    //
    Issued:Boolean;
    //requested size
    RequestSize:Cardinal;
    //pointer to returned memory
    Start:Pointer;
    //use a fixed size list of pointers for a stack
    //Stack:Pointer;
  end;

const
  //should read page size from system?
  cPage=4*1024;
  //used to indicate if we're located at a valid location
  cMagic=123123;
  //how many small-block pools are allowed
  cMaxAvail=30000;

var
  FHeap:cardinal;
  FAvailCount:cardinal;
  FAvailList:array[0..cMaxAvail] of PPoolInfo;
  //used to decrease reuse of large blocks
  //the larger the array, the better, but also increases ram usage
  FLargeList:array[0..100] of PBlockInfo;
  FLargeIndex:Integer;
  FHoldStart:Cardinal;
  FHoldEnd:Cardinal;
  //no reason to keep full list unless mem leak checking
  //FTotalList:array[1..32000] of PPoolInfo;
  FCritical:TRTLCriticalSection;

function offset(const p:pointer;const b:integer):Pointer;
begin
 Assert(p<>nil);
 {$WARNINGS OFF}
 result:=pointer(cardinal(p)+b);
 {$WARNINGS ON}
end;

procedure PushAvail(const aPool:PPoolInfo);
begin
 Assert(aPool<>nil);
 Assert(FAvailList[fholdend]=nil);

 FAvailList[fholdend]:=aPool;
 inc(FAvailCount);
 Inc(fholdend);
 if fholdend=High(FAvailList) then fholdend:=Low(FAvailList);
end;

function PopAvail:PPoolInfo;
begin
 Assert(fholdstart<>fholdend);//cant be empty

 dec(FAvailCount);
 Result:=FAvailList[fholdstart];
 FAvailList[fholdstart]:=nil;
 Assert(Result<>nil);
 Inc(fholdstart);
 if fholdstart=High(FAvailList) then fholdstart:=Low(FAvailList);
end;

function IsEmpty(const aPool:PPoolInfo): boolean;
var
 i:integer;
begin
 result:=True;
 for i:=low(aPool.avail) to high(aPool.avail) do
  if not aPool.avail[i] then
   begin
   result:=False;
   exit;
   end;
end;

function IsFull(const aPool:PPoolInfo): boolean;
var
 i:integer;
begin
 result:=True;
 for i:=low(aPool.avail) to high(aPool.avail) do
  if aPool.avail[i] then
   begin
   result:=False;
   exit;
   end;
end;

procedure CheckValid(const aBlock:PBlockInfo);
begin
 Assert(aBlock<>nil);
 Assert(aBlock.Magic=cMagic);
end;

procedure Init(const aBlock:PBlockInfo);
begin
 aBlock.Magic:=cMagic;
 aBlock.Issued:=False;
end;

procedure lock(const aBlock:PBlockInfo);
var
 old:cardinal;
begin
 //could also use PAGE_readonly here as a lower protection
 VirtualProtect(aBlock.Start,aBlock.RequestSize,PAGE_NOACCESS,old);
end;

procedure unlock(const aBlock:PBlockInfo);
//unlock 1 4k page
var
 old:cardinal;
 aSuccess:Boolean;
begin
 aSuccess:=VirtualProtect(aBlock.start,1,PAGE_READWRITE,old);
 Assert(aSuccess);
end;

function PointerToBlock(const p:Pointer):PBlockInfo;
// info|guard|mem0|guard|mem1|guard...
var
 aBase:Pointer;
 aIndex:Cardinal;
begin
 Assert(p<>nil);
 aBase:=Pointer((Cardinal(p) div (64*1024))*(64*1024));
 //which 4kblock are we in?
 aIndex:=(Cardinal(p)-Cardinal(aBase)) div (4*1024);
 //convert that to info index: 2=0,4=1,6=2
 aIndex:=(aindex div 2)-1;

 //then add that*infosize to the base
 Result:=offset(aBase,aIndex*SizeOf(TBlockInfo));

 Assert(Result.magic=cMagic);
end;

procedure InitPool(const aPool:PPoolInfo);
var
 aSub:pointer;
 old:cardinal;
 i:integer;
 aBlock:PBlockInfo;
 aSuccess:Boolean;
begin
 Assert(aPool<>nil);
 //msdn: Memory allocated by VirtualAlloc is automatically
 //initialized to zero, unless MEM_RESET is specified.
 aPool.Pool:=VirtualAlloc(nil,64*1024,MEM_COMMIT, PAGE_NOACCESS);
 if aPool.Pool=nil then exit;

 //ensure 64k aligned. safemm routines depend on this.
 assert(cardinal(aPool.Pool) mod (64*1024)=0);

 for i:=Low(aPool.avail) to high(aPool.avail) do aPool.avail[i]:=True;

 aSub:=aPool.Pool;
 aSuccess:=VirtualProtect(aSub,4096,PAGE_READWRITE,old);
 Assert(aSuccess);

 //first block is info. init the 7
 for i:=0 to cSubCount-1 do
  begin
  aBlock:=offset(aSub,i*SizeOf(TBlockInfo));
  Init(aBlock);
  aBlock.Pool:=aPool;
  aBlock.PoolIndex:=i;
  //0=2 1=4
  aBlock.Start:=offset(aPool.Pool,(2+(i*2))*4096);
  end;

end;

function GetLargeBlock(const aRequest:cardinal):PBlockInfo;
var
 aActual:cardinal;
 p:pointer;
 Old:cardinal;
begin
 aActual:=aRequest div 4096;
 if aRequest mod 4096>0 then inc(aActual);
 aActual:=aActual*4096;

 result:=VirtualAlloc(nil,aActual+(3*4096),MEM_COMMIT, PAGE_READWRITE);
 if result=nil then exit;

 //setup info
 Init(Result);
 Result.RequestSize:=aRequest;
 Result.Start:=offset(result,8*1024);

 p:=offset(Result,4096);
 VirtualProtect(p,1,PAGE_NOACCESS,old);
 p:=offset(p,4096+aActual);
 VirtualProtect(p,1,PAGE_NOACCESS,old);
end;

function FreeLargeBlock(const aBlock:PBlockInfo):integer;
//retain the block so it isn't immediately reallocated
var
 p:PBlockInfo;
begin
 Assert(ablock<>nil);


 Result:=0;

 lock(aBlock);

 Assert(FLargeList[FLargeIndex]=nil);
 FLargeList[FLargeIndex]:=ablock;

 if FLargeIndex=high(FLargeList) then FLargeIndex:=Low(FLargeList)
 else Inc(FLargeIndex);

 p:=FLargeList[FLargeIndex];
 if p<>nil then
  begin
  if VirtualFree(p, 0, MEM_RELEASE) then Result:=0
  else Result:=-1;
  FLargeList[FLargeIndex]:=nil;
  end;

end;

function CreateInfo:PPoolInfo;
const
  HEAP_ZERO_MEMORY = $00000008;  // Was not defined in older Windows.pas files
begin
  Result:=HeapAlloc(FHeap,HEAP_ZERO_MEMORY,sizeof(tpoolinfo));
  InitPool(Result);
end;

function GetSmallBlock(const aSize:cardinal):PBlockInfo;
var
 aPool:PPoolInfo;
 i:integer;
begin
 result:=nil;

 EnterCriticalSection(fcritical);
 try

 assert(FAvailCount<cMaxAvail);

 //is there an avail pool?
 if FAvailCount>0 then
  begin
  aPool:=PopAvail;
  end
 else
  begin
  aPool:=createinfo;
  if aPool.Pool=nil then Exit;
  end;

 Assert(aPool.pool<>nil);

 //return a block
 result:=nil;
 for i:=Low(aPool.avail) to high(aPool.avail) do
  begin
  if aPool.avail[i] then
   begin
   result:=offset(aPool.Pool,i*sizeof(TBlockInfo));
   aPool.avail[i]:=false;
   break;
   end;
  end;
 assert(result<>nil);

 //if the pool is still avail then push
 if not IsFull(aPool) then
  begin
  PushAvail(aPool);
  end;

 finally
 LeaveCriticalSection(fcritical);
 end;

 //we've acquired small block, can now prepare it outside of lock
 assert(result<>nil);
 CheckValid(Result);
 Result.RequestSize:=aSize;
 unlock(result);
 FillChar(Result.Start^,4096,0);
end;

function FreeSmallBlock(const aBlock:PBlockInfo):Integer;
var
 aPool:PPoolInfo;
begin
 assert(ablock<>nil);
 result:=0;
 CheckValid(aBlock);

 //prevent further access. should clear here too?
 //leave content means you could find out what was previously there when av happens?
 lock(aBlock);

 EnterCriticalSection(fcritical);
 try
 aPool:=aBlock.Pool;

 //if pool was full then it goes back into avail list
 if IsFull(aPool) then
  begin
  PushAvail(aPool);
  end;

  //tag the block as being available again
 aPool.avail[ablock.PoolIndex]:=true;

 //
 if IsEmpty(aPool) then
  begin
  //either keep in avail list or delete based on count
  //VirtualFree(aPool.Pool,0,MEM_RELEASE);
  //also heapfree the pool info
  end;

 finally
 LeaveCriticalSection(fcritical);
 end;
end;

function BlockToPointer(const aBlock:PBlockInfo):pointer;
var
 aPartial,aoffset:integer;
begin
 Assert(ablock<>nil);
  assert(ablock.Magic=cMagic);

 //header align
 //result:=aBlock.Start;

 //footer aligned
 aPartial:=(ablock.RequestSize mod cPage);
 if aPartial=0 then aOffset:=0
 else aOffset:=cPage-aPartial;
 if SafeMMAlign>0 then aOffset:=(aOffset div SafeMMAlign)*SafeMMAlign;
 Result:=offset(aBlock.Start,aoffset);

end;

function SafeGetMem(Size: Integer): Pointer;
var
 aBlock:PBlockInfo;
begin
 Assert(Size>0);

 Result:=nil;

 if size<=4*1024 then aBlock:=GetSmallBlock(size)
 else aBlock:=GetLargeBlock(size);
 if aBlock=nil then exit;

 //setup
 CheckValid(aBlock);
 Assert(aBlock.Issued=false);
 aBlock.Issued:=True;

 //and quit
 result:=BlockToPointer(aBlock);

end;

function SafeAllocMem(ASize: Cardinal): Pointer;
begin
  Assert(aSize>0);
  Result := SafeGetMem(ASize);
end;

function SafeRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function SafeUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function Min(const i1, i2: Integer): Integer;
begin
  Result := i1;
  if i2 < Result then Result := i2;
end;

function SafeReallocMem(P: Pointer; Size: Integer): Pointer;
//force-move realloc will help find invalid pointer usage
var
  aActual:Integer;
  aSource:PBlockInfo;
begin
  Assert(p<>nil);
  Assert(Size>0);

  //find size of original block
  aSource:=PointerToBlock(p);

  //after checks have passed, get new block and copy
  Result := SafeAllocMem(Size);
  if result=nil then exit;

  aActual:=min(aSource.RequestSize,Size);
  Move(p^,Result^,aactual);

  SafeFreeMem(p);
end;

function SafeFreeMem(P: Pointer): Integer;
var
  aBlock:PBlockInfo;
begin
  Assert(p<>nil);

  //on free also check if it held a class. store classname as text in
  //the info block, makes easier?

  aBlock:=PointerToBlock(p);
  CheckValid(aBlock);

  Assert(aBlock.Issued);
  aBlock.Issued:=False;

  if aBlock.Pool=nil then
   begin
   Result:=FreeLargeBlock(aBlock);
   end
  else
   begin
   Result:=FreeSmallBlock(aBlock);
   end;

end;

procedure SafeMMProtect(const p:Pointer;const aProtect:TSafeProtect);
var
 old:Cardinal;
 aBlock:PBlockInfo;
const
 cProtect:array[TSafeProtect] of cardinal
 = (PAGE_READWRITE,PAGE_READONLY,PAGE_NOACCESS);
begin
 Assert(p<>nil);
 aBlock:=PointerToBlock(p);
 Assert(aBlock.Issued);
 VirtualProtect(aBlock.Start,aBlock.RequestSize,cProtect[aProtect],old)
end;

procedure SafeMMPrepare;
var
 aPool:PPoolInfo;
 i:Integer;
begin
 for i:=1 to 1000 do
  begin
  aPool:=createinfo;
  PushAvail(aPool);
  end;
end;

initialization

  {$WARNINGS OFF}
  //check that enough info can fit into 1 page
  Assert(SizeOf(TBlockInfo)*(cSubCount+1)<=(4*1024));
  {$WARNINGS ON}

  FHoldStart:=0;
  FHoldEnd:=0;
  FLargeIndex:=0;
  FAvailCount:=0;
  SafeMMAlign:=8;
  SafeMMMode:=gmFooter;
  InitializeCriticalSection(fcritical);
  FHeap:=GetProcessHeap;

finalization

  DeleteCriticalSection(fcritical);

end.



