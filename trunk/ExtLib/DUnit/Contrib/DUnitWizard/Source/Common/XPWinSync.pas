unit XPWinSync;

{
 $Source: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPWinSync.pas,v $
 $Revision: 7 $
 $Date: 2008-04-24 07:59:47 -0400 (Thu, 24 Apr 2008) $
 Last amended by $Author: judc $
 $State: Exp $

 XPWinSync:
 Interfaces and implementing classes which wrap Windows
 synchronisation mechanisms:
 Events (AutoReset and Manual)
 Mutexes
 Semaphores
 CriticalSections
 SharedCounters
 RWSynchronisers

 Copyright (c) 2001 by The Excellent Programming Company Pty Ltd
 (Australia) (ABN 27 005 394 918).

 Contact Paul Spain via email: paul@xpro.com.au

 This unit is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This unit is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this unit; if not, the license can be viewed at:
 http://www.gnu.org/copyleft/lesser.html
 or write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA  02111-1307  USA
 }

interface

uses
  XPWinBase,
  XPRestore,            // IXPRestore, TXPRestore
  XPSyncRW,             // IXPSyncRW
  Windows,              // THandle,  CreateXXX(), OpenXXX
  SysUtils;             // Exception, Trim(), FmtStr(), AnsiPos(),
                        // AnsiLowerCase()

type

{$IFDEF XPW32E}
  EXPWin32Event = class (EXPWin32) end;

  EXPWin32Synchro = class (EXPWin32) end;
    EXPWin32Mutex = class (EXPWin32Synchro) end;
    EXPWin32Semaphore = class (EXPWin32Synchro) end;

  EXPWin32SharedCounter = class (EXPWin32) end;
  EXPWin32SerialAccess = class (EXPWin32) end;
{$ENDIF}

//////////////////////////////////////////////////////////////////////////////
///   IXPWinXXXEvent
//////////////////////////////////////////////////////////////////////////////

  IXPWinEvent = interface(IXPWinNamedKernelObject)
    ['{832555D0-0C82-11D5-A261-00608CF441D9}']
    // Behaviour of Signal is the prime difference between Auto and Manual
    // events. For Auto events, Signal will signal (only) the first waiting
    // thread and reset itself. For Manual events, Signal will stay signalled
    // until Reset is called
    procedure Signal;
    end;

  IXPWinAutoEvent = interface(IXPWinEvent)
    ['{7F4EBC21-16E6-11D5-A271-00608CF441D9}']
    end;

  IXPWinManualEvent = interface(IXPWinEvent)
    ['{88C4F111-1500-11D5-A26D-00608CF441D9}']
    procedure Reset;
    // Pulse behaves like Signal for an Auto event, except that it signals
    // _all_ waiting threads (not just the first thread) before resetting
    // itself.
    procedure Pulse;
    end;

//////////////////////////////////////////////////////////////////////////////
///   IXPSynchro: Mutexes, semaphores and critical sections
//////////////////////////////////////////////////////////////////////////////

  IXPWinSynchro = interface(IXPWinError)
    ['{7F4EBC22-16E6-11D5-A271-00608CF441D9}']
    function Enter: boolean;
    function Leave: boolean;
    end;

  TXPWinSerialAccess = class(TXPWinError, IXPRestore)
    private

    FSync: IXPWinSynchro;

    public

    constructor Create(const ASync: IXPWinSynchro);
    destructor Destroy; override;
    end;

//////////////////////////////////////////////////////////////////////////////
///   IXPWinMutex
//////////////////////////////////////////////////////////////////////////////

  IXPWinMutex = interface(IXPWinNamedKernelObject)
    ['{BC7BDA82-4151-11D5-A2C1-00608CF441D9}']
    end;

//////////////////////////////////////////////////////////////////////////////
///   IXPWinXXXSemaphore
//////////////////////////////////////////////////////////////////////////////

  IXPWinSemaphore = interface(IXPWinNamedKernelObject)
    ['{F65C8F71-1700-11D5-A271-00608CF441D9}']
    // Count is number of free entries, not occupied entries
    //
    // There are some caveats on usage of GetCount, as Windows does
    // not provide the facility to retrieve the current count value. The
    // previous Count value can be retrieved via successful calls to
    // Windows.ReleaseSemaphore()
    // Caveats:
    // GetCount is always accurate for a single-handle semaphore kernel object
    // ie a semaphore which has (only) been created, not opened (see
    // IXPWinNamedKernelObject.Instance) and which has been manipulated only
    // by the interface methods, not via Windows API calls on the handle.
    // If the semaphore has more than one handle opened, the count value is only
    // guaranteed correct immediately following IXPSynchro.Leave and
    // IXPWinSemaphore.Release calls.
    // Note that this and the following interface are thread-safe, so the
    // interfaces can be passed among threads rather than creating new handles
    // via calls to GetSemaphore().
    function GetCount: integer;
    function Acquire: boolean;
    procedure Release;
    property Count: integer read GetCount;
    end;

  // This interface can be used when the semaphore has been created rather than
  // opened, ie when Instance = koCreated (IXPWinNamedKernelObject.Instance)
  IXPWinCreatedSemaphore = interface(IXPWinSemaphore)
    ['{64F2B390-26F8-11D5-8CAD-0080ADB62643}']
    function GetCapacity: integer;
    // Use Open as the first call on a new interface which has been created via
    // GetSemaphore() with the CreateOpen parameter as false. This scenario is
    // useful to initialize resources upon creation, before releasing them for
    // use.
    function Open: boolean;
    property Capacity: integer read GetCapacity;
    end;

//////////////////////////////////////////////////////////////////////////////
///   IXPSharedCounter
//////////////////////////////////////////////////////////////////////////////

const
  XPCounterError = System.Low(integer);

type

  IXPSharedCounter = interface
    ['{BC7BDA81-4151-11D5-A2C1-00608CF441D9}']
    function GetValue: integer;
    procedure SetValue(const Value: integer);
    function Inc(const Delta: integer = 1): integer;
    function Dec(const Delta: integer = 1): integer;

    property Value: integer read GetValue write SetValue;
    end;

//////////////////////////////////////////////////////////////////////////////
///   Creator functions: unit entry points
//////////////////////////////////////////////////////////////////////////////

function GetAutoEvent(const CreateAsSignaled: boolean = false;
  const AName: string = '';
  const Inheritable: boolean = true;
  const SecurityDescriptor: Pointer = nil): IXPWinAutoEvent;

function GetManualEvent(const CreateAsSignaled: boolean = false;
  const AName: string = '';
  const Inheritable: boolean = true;
  const SecurityDescriptor: Pointer = nil): IXPWinManualEvent;

{ Mutexes and Semaphores must be instantiated  at a scope which is *local* to
  each interested thread. The common Name value ensures we are connecting
  with the same kernel object. }

function GetMutex(const AName: string = ''; const Inheritable: boolean = true;
  const SecurityDescriptor: Pointer = nil): IXPWinMutex;

function GetSemaphore(const Capacity: integer; const AName: string = '';
  const CreateOpen: boolean = true; const Inheritable: boolean = true;
  const SecurityDescriptor: Pointer = nil): IXPWinSemaphore;

{ Critical sections must be instantiated at a scope which is common to all
  interested threads, ie at process *global* scope. }

function CreateCriticalSection: IXPWinSynchro;

function GetSharedCounter(const InitialValue: integer = 0;
  const AName: string = ''; const Inheritable: boolean = true;
  const SecurityDescriptor: Pointer = nil): IXPSharedCounter;

{ CreateThreadIXPSyncRW creates an interface for intra-process thread
 synchronisation. }
function CreateThreadRWSynchroniser(
  const SyncPriority: TXPSyncPriority = spReaders): IXPSyncRW;

{ CreateProcessIXPSyncRW creates an interface for inter-process thread
 synchronisation. }
function GetProcessRWSynchroniser(const Name: string = '';
  const SyncPriority: TXPSyncPriority = spReaders;
  const Inheritable: boolean = true;
  const SecurityDescriptor: Pointer = nil): IXPSyncRW;

implementation

const CVSID: string = '$Header: /cvsroot/dunit/dunit/Contrib/DUnitWizard/Source/Common/XPWinSync.pas,v 1.3 2008/04/18 02:32:53 judc Exp $';

///////////////////////////////////////////////////////////////////////////////
///     TXPSerialAccess implementation
///////////////////////////////////////////////////////////////////////////////

constructor TXPWinSerialAccess.Create(const ASync: IXPWinSynchro);
  begin
{$IFDEF XPW32E}
  inherited Create(EXPWin32SerialAccess);
{$ELSE}
  inherited Create;
{$ENDIF}
  FSync := ASync;

  if not FSync.Enter then
    Error('TXPSerialAccess.Create');

  end;

destructor TXPWinSerialAccess.Destroy;
  begin

  if not FSync.Leave then
    Error('TXPSerialAccess.Destroy');

  inherited;
  end;

///////////////////////////////////////////////////////////////////////////////
///     IXPWinXXXEvent implementation
///////////////////////////////////////////////////////////////////////////////

type

  TXPWinEvent = class(TXPWinNamedKernelObject, IXPWinEvent)
    private

    //
    // IXPWinEvent implementation
    //

    procedure Signal;

    public

    constructor Create(const AName: string; const CreateAsSignaled: boolean;
      const ManualReset: boolean; const Inheritable: boolean;
      const SecurityDescriptor: Pointer);
    end;

  TXPWinAutoEvent = class(TXPWinEvent, IXPWinAutoEvent)
    public

    constructor Create(const AName: string; const CreateAsSignaled: boolean;
      const Inheritable: boolean; const SecurityDescriptor: Pointer);
    end;

  TXPWinManualEvent = class(TXPWinEvent, IXPWinManualEvent)
    private

    //
    // IXPWinManualEvent implementation
    //

    procedure Reset;
    procedure Pulse;

    public

    constructor Create(const AName: string;  const CreateAsSignaled: boolean;
      const Inheritable: boolean; const SecurityDescriptor: Pointer);
    end;

constructor TXPWinEvent.Create(const AName: string;
  const CreateAsSignaled: boolean; const ManualReset: boolean;
  const Inheritable: boolean; const SecurityDescriptor: Pointer);
  begin
  inherited Create(AName, Inheritable, SecurityDescriptor);
{$IFDEF XPW32E}
  SetException(EXPWin32Event);
{$ENDIF}
  FHandle := Windows.CreateEvent(@FSecurityAttributes, ManualReset,
    CreateAsSignaled, PChar(GetName));

  if FHandle = 0 then
    Error('TXPWinEvent.Create: Windows.CreateEvent failure')
  else if Windows.GetLastError = 0 then
    FInstance := koCreated
  else if Windows.GetLastError = ERROR_ALREADY_EXISTS then
    FInstance := koOpened;

  end;

procedure TXPWinEvent.Signal;
  begin
  Windows.SetEvent(FHandle);
  end;

{ TXPWinAutoEvent }

constructor TXPWinAutoEvent.Create(const AName: string;
  const CreateAsSignaled: boolean; const Inheritable: boolean;
  const SecurityDescriptor: Pointer);
  const
  ManualReset = false;

  begin
  inherited Create(AName, CreateAsSignaled, ManualReset, Inheritable,
    SecurityDescriptor);
  end;

{ TXPWinManualEvent }

constructor TXPWinManualEvent.Create(const AName: string;
  const CreateAsSignaled: boolean; const Inheritable: boolean;
  const SecurityDescriptor: Pointer);
  const
  ManualReset = true;

  begin
  inherited Create(AName, CreateAsSignaled, ManualReset, Inheritable,
    SecurityDescriptor);
  end;

procedure TXPWinManualEvent.Pulse;
  begin
  Windows.PulseEvent(GetHandle);
  end;

procedure TXPWinManualEvent.Reset;
  begin
  Windows.ResetEvent(GetHandle);
  end;

//////////////////////////////////////////////////////////////////////////////
///   TXPWinMutex implementation
//////////////////////////////////////////////////////////////////////////////

type

  TXPWinMutex = class(TXPWinNamedKernelObject, IXPWinMutex, IXPWinSynchro)
    private

    //
    // IXPSynchro implementation
    //

    function Enter: boolean;
    function Leave: boolean;

    public

    constructor Create(const AName: string; const Inheritable: boolean;
      const SecurityDescriptor: Pointer);
    end;

constructor TXPWinMutex.Create(const AName: string; const Inheritable: boolean;
  const SecurityDescriptor: Pointer);
  const
  EnterOnCreate = false;

  begin
  inherited Create(AName, Inheritable, SecurityDescriptor);
{$IFDEF XPW32E}
  SetException(EXPWin32Mutex);
{$ENDIF}
  FHandle := Windows.CreateMutex(@FSecurityAttributes, EnterOnCreate,
    PChar(GetName));

  if FHandle = 0 then
    Error('TXPWinMutex.Create: Windows.CreateMutex failure')
  else if Windows.GetLastError = 0 then
    FInstance := koCreated
  else if Windows.GetLastError = ERROR_ALREADY_EXISTS then
    FInstance := koOpened;

  end;

function TXPWinMutex.Enter: boolean;
  begin
  Result := Wait;

  if not Result then
    SetLastContext('TXPWinMutex.Enter: ' + GetLastContext);

  end;

function TXPWinMutex.Leave: boolean;
  begin
  Result := Windows.ReleaseMutex(FHandle);

  if not Result then
    Error('TXPWinMutex.Leave: Windows.ReleaseMutex failure');

  end;

//////////////////////////////////////////////////////////////////////////////
///   TXPWinSemaphore implementation
//////////////////////////////////////////////////////////////////////////////

type

  TXPWinSemaphore = class(TXPWinNamedKernelObject, IXPWinSynchro,
    IXPWinSemaphore, IXPWinCreatedSemaphore)
    private

    FCapacity: integer;
    FCount: integer;

    //
    // IXPSynchro implementation
    //

    function Enter: boolean;
    function Leave: boolean;

    //
    // IXPWinSemaphore implementation
    //

    function GetCount: integer;
    function Acquire: boolean;
    procedure Release;

    //
    // IXPWinCreatedSemaphore implementation
    //

    function Open: boolean;
    function GetCapacity: integer;

    protected

    function IsSignaled: boolean; override;
    function Wait: boolean; override;
    function WaitFor(const Millisecs: cardinal): boolean; override;

    public

    constructor Create(const ACapacity: integer; const AName: string;
      const CreateOpen: boolean; const Inheritable: boolean;
      const SecurityDescriptor: Pointer);
    end;

constructor TXPWinSemaphore.Create(const ACapacity: integer;
  const AName: string; const CreateOpen: boolean; const Inheritable: boolean;
  const SecurityDescriptor: Pointer);
  var
  InitCount: integer;

  begin
  inherited Create(AName, Inheritable, SecurityDescriptor);
{$IFDEF XPW32E}
  SetException(EXPWin32Semaphore);
{$ENDIF}
  FCapacity := -1;
  FCount := -1;

  if CreateOpen then
    InitCount := ACapacity
  else
    InitCount := 0;

  FHandle := Windows.CreateSemaphore(@FSecurityAttributes, InitCount,
    ACapacity, PChar(GetName));

  if FHandle = 0 then
    Error('TXPWinSemaphore.Create: Windows.CreateSemaphore failure')
  else if Windows.GetLastError = 0 then
    begin
    FInstance := koCreated;
    FCapacity := ACapacity;
    FCount := InitCount;
    end
  else if Windows.GetLastError = ERROR_ALREADY_EXISTS then
    FInstance := koOpened;

  end;

function TXPWinSemaphore.GetCapacity: integer;
  begin
  Result := FCapacity;
  end;

function TXPWinSemaphore.GetCount: integer;
  begin
  Result := FCount;
  end;

function TXPWinSemaphore.Enter: boolean;
  begin
  Result := Wait;

  if not Result then
    SetLastContext('TXPWinSemaphore.Enter: ' + GetLastContext);

  end;

function TXPWinSemaphore.IsSignaled: boolean;
  begin
  Result := inherited IsSignaled;

  if Result then
    Windows.InterlockedDecrement(FCount)
  else
    SetLastContext('TXPWinSemaphore.IsSignaled: ' + GetLastContext);

  end;

function TXPWinSemaphore.Wait: boolean;
  begin
  Result := inherited Wait;

  if Result then
    Windows.InterlockedDecrement(FCount)
  else
    SetLastContext('TXPWinSemaphore.Wait: ' + GetLastContext);

  end;

function TXPWinSemaphore.WaitFor(const Millisecs: cardinal): boolean;
  begin
  Result := inherited WaitFor(Millisecs);

  if Result then
    Windows.InterlockedDecrement(FCount)
  else
    SetLastContext('TXPWinSemaphore.WaitFor: ' + GetLastContext);

  end;

function TXPWinSemaphore.Leave: boolean;
  const
  ReleaseCount = 1;

  begin
  Result := Windows.ReleaseSemaphore(FHandle, ReleaseCount, @FCount);

  if Result then
    Windows.InterlockedIncrement(FCount)
  else
    Error('TXPWinSemaphore.Leave: Windows.ReleaseSemaphore failure');

  end;

function TXPWinSemaphore.Acquire: boolean;
  begin
  Result := Enter;

  if not Result then
    SetLastContext('TXPWinSemaphore.Acquire: ' + GetLastContext);

  end;

procedure TXPWinSemaphore.Release;
  begin
  Leave;
  end;

function TXPWinSemaphore.Open: boolean;
  begin

  Result := (FCount = 0);

  if not Result then
    exit;

  if not Windows.ReleaseSemaphore(FHandle, FCapacity, @FCount) then
    Error('TXPWinSemaphore.Open: Windows.ReleaseSemaphore failure')
  else
    // Set new Count value
    Windows.InterlockedExchange(FCount, FCount + FCapacity);

  end;

//////////////////////////////////////////////////////////////////////////////
///   TXPCriticalSectiom implementation
//////////////////////////////////////////////////////////////////////////////

type

  TXPWinCriticalSection = class(TXPWinError, IXPWinSynchro)
    private

    FCriticalSection: TRTLCriticalSection;

    //
    // IXPSynchro implementation
    //

    function Enter: boolean;
    function Leave: boolean;

    public

    constructor Create;
    destructor Destroy; override;
    end;

constructor TXPWinCriticalSection.Create;
  begin
{$IFDEF XPW32E}
  inherited Create(EXPWin32);
{$ELSE}
  inherited Create;
{$ENDIF}
  Windows.InitializeCriticalSection(FCriticalSection);
  end;

destructor TXPWinCriticalSection.Destroy;
  begin
  Windows.DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
  end;

function TXPWinCriticalSection.Enter: boolean;
  begin
  Windows.EnterCriticalSection(FCriticalSection);
  Result := true;
  end;

function TXPWinCriticalSection.Leave: boolean;
  begin
  Result := true;
  Windows.LeaveCriticalSection(FCriticalSection);
  end;

//////////////////////////////////////////////////////////////////////////////
///   IXPWinCounter implementation
//////////////////////////////////////////////////////////////////////////////

type

  PInteger = ^integer;

  TXPWinSharedCounter = class(TXPWinNamedKernelObject, IXPSharedCounter)
    private

    FMutex: IXPWinMutex;
    FSync: IXPWinSynchro;
    FCounter: PInteger;

    function GetView: boolean;
    procedure ReleaseView;

    //
    // IXPCounter implementation
    //

    function GetValue: integer;
    procedure SetValue(const Value: integer);
    function Inc(const Delta: integer): integer;
    function Dec(const Delta: integer): integer;

    public

    constructor Create(const InitialValue: integer; const AName: string;
      const Inheritable: boolean; const SecurityDescriptor: Pointer);

    end;

constructor TXPWinSharedCounter.Create(const InitialValue: integer;
  const AName: string; const Inheritable: boolean;
  const SecurityDescriptor: Pointer);
  var
  SerialAccess: IXPRestore;

  const
  PageFileHandle = $FFFFFFFF;
  HiSize = 0;

  begin
  inherited Create(AName, Inheritable, SecurityDescriptor);
{$IFDEF XPW32E}
  SetException(EXPWin32SharedCounter);
{$ENDIF}
  FMutex := XPWinSync.GetMutex(GetName + '.XPCounter.Mutex', Inheritable,
    SecurityDescriptor);
  FSync := FMutex as IXPWinSynchro;
  SerialAccess := TXPWinSerialAccess.Create(FSync);
                                                                     
  // if mutex created - so we can set init value. However, the EnterOnCreate
  // argument doesn't work in Windows NT SP5 as documented, ie you don't get
  // initial ownership when requested if the mutex is created

  // Use this call to create or open file mapping object. Check last error
  // value to determine case
  FHandle := Windows.CreateFileMapping(PageFileHandle,
    @FSecurityAttributes, PAGE_READWRITE, HiSize, System.Sizeof(integer),
    PChar(GetName));

  if FHandle = 0 then
    begin
    Error('TXPWinSharedCounter.Create: Windows.CreateFileMapping failure');
    exit;
    end;

  // Store result from GetLastError
  SetLastError;

  if GetLastError = 0 then
    begin
    FInstance := koCreated;

    // Set initial value
    if GetView then
      begin
      FCounter^ := InitialValue;
      ReleaseView;
      end;

    end
  else if GetLastError = ERROR_ALREADY_EXISTS then
    FInstance := koOpened;

  end;

function TXPWinSharedCounter.GetView: boolean;
  const
  HiOffset = 0;
  LoOffset = 0;
  Length = System.Sizeof(integer);

  begin

  if not FSync.Enter then
    begin
    Error('TXPWinSharedCounter.GetView');
    Result := false;
    end
  else
    begin
    // Create view onto file mapping
    FCounter := Windows.MapViewOfFile(FHandle, FILE_MAP_WRITE,
      HiOffset, LoOffset, Length);
    Result := FCounter <> nil;

    if not Result then
      Error('TXPWinSharedCounter.GetView: Windows.MapViewOfFile failure');

    end;

  end;

procedure TXPWinSharedCounter.ReleaseView;
  begin
  if not FSync.Leave then
    Error('TXPWinSharedCounter.ReleaseView');

  if (FCounter <> nil) and (not Windows.UnmapViewOfFile(FCounter)) then
    Error('TXPWinSharedCounter.ReleaseView: Windows.UnmapViewOfFile failure');

  end;

function TXPWinSharedCounter.Dec(const Delta: integer): integer;
  begin
  Result := Inc(-Delta);
  end;

function TXPWinSharedCounter.GetValue: integer;
  begin

  if GetView then
    begin
    Result := FCounter^;
    ReleaseView;
    end
  else
    Result := XPCounterError;

  end;

function TXPWinSharedCounter.Inc(const Delta: integer): integer;
  begin

  if GetView then
    begin
    System.Inc(FCounter^, Delta);
    Result := FCounter^;
    ReleaseView;
    end
  else
    Result := XPCounterError;

  end;

procedure TXPWinSharedCounter.SetValue(const Value: integer);
  begin

  if GetView then
    begin
    FCounter^ := Value;
    ReleaseView;
    end

  end;

//////////////////////////////////////////////////////////////////////////////
///   IXPSyncRW implementation
//////////////////////////////////////////////////////////////////////////////

  // "Readers and writers" implementation (1)
  //
  // Single writer and multiple (unlimited) readers
  // Priority given to readers, ie writer must wait till all
  // readers have finished. (Can lead to writer starvation)
  // Implementation uses critical sections and is therefore limited
  // to a single process.

type TXPWinThreadRWSynchroniser = class(TSyncRWBase)
    private

    FReaders: integer;
    FAccess: IXPWinSynchro;
    FMutex: IXPWinSynchro;

    protected

    // Use ReadBegin/ReadEnd with a try..finally context
    procedure ReadBegin; override;
    procedure ReadEnd; override;
    // Use WriteBegin/WriteEnd with a try..finally context
    procedure WriteBegin; override;
    procedure WriteEnd; override;

    public

    constructor Create;

    end;

type TXPWinProcessRWSynchroniser = class(TSyncRWBase)
    private

    FName: string;
    FReaders: IXPSharedCounter;
    FAccess: IXPWinSynchro;
    FBlockReaders: IXPWinSynchro;

    protected

    // Use ReadBegin/ReadEnd with a try..finally context
    procedure ReadBegin; override;
    procedure ReadEnd; override;
    // Use WriteBegin/WriteEnd with a try..finally context
    procedure WriteBegin; override;
    procedure WriteEnd; override;

    public

    constructor Create(const Name: string; const Inheritable: boolean;
      const SecurityDescriptor: Pointer);

    end;

  // "Readers and writers" implementation (2)
  //
  // Single writer and multiple (unlimited) readers
  // Priority given to writers, ie no more readers allowed once a writer
  // is waiting, and waiting writers given priority over waiting readers.
  // ( Can lead to reader starvation. )
  // Implementation uses critical sections and is therefore limited
  // to a single process.

type TXPWinThreadWRSynchroniser = class(TSyncRWBase)
    private

    FReaders: integer;
    FWriters: integer;
    FAccess: IXPWinSynchro;
    FQueueReader: IXPWinSynchro;
    FBlockReaders: IXPWinSynchro;
    FRMutex: IXPWinSynchro;
    FWMutex: IXPWinSynchro;

    protected

    // Use ReadBegin/ReadEnd with a try..finally context
    procedure ReadBegin; override;
    procedure ReadEnd; override;
    // Use WriteBegin/WriteEnd with a try..finally context
    procedure WriteBegin; override;
    procedure WriteEnd; override;

    public

    constructor Create;
    end;

type TXPWinProcessWRSynchroniser = class(TSyncRWBase)
    private

    FReaders: IXPSharedCounter;
    FWriters: IXPSharedCounter;
    FName: string;
    FAccess: IXPWinSynchro;
    FBlockReaders: IXPWinSynchro;

    protected

    // Use ReadBegin/ReadEnd with a try..finally context
    procedure ReadBegin; override;
    procedure ReadEnd; override;
    // Use WriteBegin/WriteEnd with a try..finally context
    procedure WriteBegin; override;
    procedure WriteEnd; override;

    public

    constructor Create(const Name: string; const Inheritable: boolean;
      const SecurityDescriptor: Pointer);
    end;

/////////////////////////////////////////////////////////////////////////////
//  TThreadSyncRW implementation
//  Based on: Courtois et al, Communications of the ACM,
//            Vol 14, No 10 Oct 1971, pp. 667-668
/////////////////////////////////////////////////////////////////////////////

constructor TXPWinThreadRWSynchroniser.Create;
  begin
  inherited Create;
  FAccess := XPWinSync.CreateCriticalSection;
  FMutex := XPWinSync.CreateCriticalSection;
  end;

procedure TXPWinThreadRWSynchroniser.ReadBegin;
  begin
  FMutex.Enter;

  try
    System.Inc(FReaders);

    if FReaders = 1 then
      FAccess.Enter;

  finally
    FMutex.Leave;
    end;

  end;

procedure TXPWinThreadRWSynchroniser.ReadEnd;
  begin
  FMutex.Enter;

  try
    System.Dec(FReaders);

    if FReaders = 0 then
      FAccess.Leave;

  finally
    FMutex.Leave;
    end;

  end;

procedure TXPWinThreadRWSynchroniser.WriteBegin;
  begin
  FAccess.Enter;
  end;

procedure TXPWinThreadRWSynchroniser.WriteEnd;
  begin
  FAccess.Leave;
  end;

/////////////////////////////////////////////////////////////////////////////
//  TProcessSyncRW implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPWinProcessRWSynchroniser.Create(const Name: string;
  const Inheritable: boolean; const SecurityDescriptor: Pointer);
  begin
  inherited Create;
  FName := Name;
  FReaders := XPWinSync.GetSharedCounter(0, FName + '.FReaders', Inheritable,
    SecurityDescriptor);
  FAccess := XPWinSync.GetMutex(FName + '.FAccess', Inheritable,
    SecurityDescriptor) as IXPWinSynchro;
  FBlockReaders := XPWinSync.GetMutex(FName + '.FMutex', Inheritable,
    SecurityDescriptor) as IXPWinSynchro;
  end;

procedure TXPWinProcessRWSynchroniser.ReadBegin;
  begin
  FBlockReaders.Enter;

  try

    if FReaders.Inc = 1 then
      FAccess.Enter;

  finally
    FBlockReaders.Leave;
    end;

  end;

procedure TXPWinProcessRWSynchroniser.ReadEnd;
  begin

  if FReaders.Dec = 0 then
    FAccess.Leave;

  end;

procedure TXPWinProcessRWSynchroniser.WriteBegin;
  begin
  FAccess.Enter;
  end;

procedure TXPWinProcessRWSynchroniser.WriteEnd;
  begin
  FAccess.Leave;
  end;

/////////////////////////////////////////////////////////////////////////////
//  TThreadSyncWR implementation
//  Based on: Courtois et al, Communications of the ACM,
//            Vol 14, No 10 Oct 1971, pp. 667-668
/////////////////////////////////////////////////////////////////////////////

constructor TXPWinThreadWRSynchroniser.Create;
  begin
  inherited Create;
  FAccess := XPWinSync.CreateCriticalSection;
  FQueueReader := XPWinSync.CreateCriticalSection;
  FBlockReaders := XPWinSync.CreateCriticalSection;
  FRMutex := XPWinSync.CreateCriticalSection;
  FWMutex := XPWinSync.CreateCriticalSection;
  end;

procedure TXPWinThreadWRSynchroniser.ReadBegin;
  begin
  FQueueReader.Enter;

  try
    FBlockReaders.Enter;

    try
      FRMutex.Enter;

      try
        System.Inc(FReaders);

        if FReaders = 1 then
          FAccess.Enter;

      finally
        FRMutex.Leave;
        end;

    finally
      FBlockReaders.Leave;
      end;

  finally
    FQueueReader.Leave;
    end;

  end;

procedure TXPWinThreadWRSynchroniser.ReadEnd;
  begin
  FRMutex.Enter;

  try
    System.Dec(FReaders);

    if FReaders = 0 then
      FAccess.Leave;

  finally
    FRMutex.Leave;
    end;

  end;

procedure TXPWinThreadWRSynchroniser.WriteBegin;
  begin
  FWMutex.Enter;

  try
    System.Inc(FWriters);

    if (FWriters = 1) then
      FBlockReaders.Enter;

  finally
    FWMutex.Leave;
    end;

  FAccess.Enter;
  end;

procedure TXPWinThreadWRSynchroniser.WriteEnd;
  begin
  FAccess.Leave;
  FWMutex.Enter;

  try
    System.Dec(FWriters);

    if FWriters = 0 then
      FBlockReaders.Leave;

  finally
    FWMutex.Leave;
    end;

  end;

/////////////////////////////////////////////////////////////////////////////
//  TProcessSyncWR implementation
/////////////////////////////////////////////////////////////////////////////

constructor TXPWinProcessWRSynchroniser.Create(const Name: string;
  const Inheritable: boolean; const SecurityDescriptor: Pointer);
  const
  InitialValue = 0;

  begin
  inherited Create;
  FName := Name;
  FReaders := XPWinSync.GetSharedCounter(InitialValue, FName + '.FReaders',
    Inheritable, SecurityDescriptor);
  FWriters := XPWinSync.GetSharedCounter(InitialValue, FName + '.FWriters',
    Inheritable, SecurityDescriptor);
  FAccess := XPWinSync.GetMutex(FName + '.FAccess', Inheritable,
    SecurityDescriptor) as IXPWinSynchro;
  FBlockReaders := XPWinSync.GetMutex(FName + '.FBlockReaders', Inheritable,
    SecurityDescriptor) as IXPWinSynchro;
  end;

procedure TXPWinProcessWRSynchroniser.ReadBegin;
  begin
  FBlockReaders.Enter;

  try

    if FReaders.Inc = 1 then
      FAccess.Enter;

  finally
    FBlockReaders.Leave;
    end;

  end;

procedure TXPWinProcessWRSynchroniser.ReadEnd;
  begin

  if FReaders.Dec = 0 then
    FAccess.Leave;

  end;

procedure TXPWinProcessWRSynchroniser.WriteBegin;
  begin

  if FWriters.Inc = 1 then
    FBlockReaders.Enter;

  FAccess.Enter;
  end;

procedure TXPWinProcessWRSynchroniser.WriteEnd;
  begin
  FAccess.Leave;

  if FWriters.Dec = 0 then
    FBlockReaders.Leave;

  end;

///////////////////////////////////////////////////////////////////////////////
///    Global functions
///////////////////////////////////////////////////////////////////////////////

function GetAutoEvent(const CreateAsSignaled: boolean;
  const AName: string; const Inheritable: boolean;
  const SecurityDescriptor: Pointer): IXPWinAutoEvent;
  begin
  Result := TXPWinAutoEvent.Create(AName, CreateAsSignaled, Inheritable,
    SecurityDescriptor);
  end;

function GetManualEvent(const CreateAsSignaled: boolean;
  const AName: string; const Inheritable: boolean;
  const SecurityDescriptor: Pointer): IXPWinManualEvent;
  begin
  Result := TXPWinManualEvent.Create(AName, CreateAsSignaled, Inheritable,
    SecurityDescriptor);
  end;

function GetMutex(const AName: string; const Inheritable: boolean;
  const SecurityDescriptor: Pointer): IXPWinMutex;
  begin
  Result := TXPWinMutex.Create(AName, Inheritable, SecurityDescriptor);
  end;

function GetSemaphore(const Capacity: integer; const AName: string;
  const CreateOpen: boolean; const Inheritable: boolean;
  const SecurityDescriptor: Pointer): IXPWinSemaphore;
  begin
  Result := TXPWinSemaphore.Create(Capacity, AName, CreateOpen, Inheritable,
    SecurityDescriptor);
  end;

function CreateCriticalSection: IXPWinSynchro;
  begin
  Result := TXPWinCriticalSection.Create;
  end;

function GetSharedCounter(const InitialValue: integer;
  const AName: string; const Inheritable: boolean;
  const SecurityDescriptor: Pointer): IXPSharedCounter;
  begin
  Result := TXPWinSharedCounter.Create(InitialValue, AName, Inheritable,
    SecurityDescriptor);
  end;

function CreateThreadRWSynchroniser(
  const SyncPriority: TXPSyncPriority): IXPSyncRW;
  begin

  if SyncPriority = spWriters then
    Result := TXPWinThreadWRSynchroniser.Create
  else
    Result := TXPWinThreadRWSynchroniser.Create;

  end;

function GetProcessRWSynchroniser(const Name: string;
  const SyncPriority: TXPSyncPriority; const Inheritable: boolean;
  const SecurityDescriptor: Pointer): IXPSyncRW;
  begin

  if SyncPriority = spWriters then
    Result := TXPWinProcessWRSynchroniser.Create(Name, Inheritable,
      SecurityDescriptor)
  else
    Result := TXPWinProcessRWSynchroniser.Create(Name, Inheritable,
      SecurityDescriptor);

  end;

end.

