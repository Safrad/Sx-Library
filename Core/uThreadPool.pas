unit uThreadPool;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  uTypes, uData, uAsyncTask, uNumericalIntervalArgument,
  SyncObjs,
  Classes;

// TODO: Wait for task, task priority

type
  TThreads = array of TThread;

  TThreadPool = class
  private
    FNoThreadEvent: TEvent;
    FNoWorkEvent: TEvent;

    FMaxThreads: TNumericalIntervalArgument;
    FRunThreads: S4;
    FWorking: S4;
    FThreads: TThreads;
    FQueue: TData; // array of TAsyncTask;
    FQueueCriticalSection: TCriticalSection;
    FThreadPriority: TThreadPriority;
    FOnTasksFinished: TNotifyEvent;
    procedure SetRunThreads(Value: UG);
    procedure MaxThreadsChanged(Sender: TObject);
    procedure QueueToThread;
//    procedure WaitForWorkers;
    procedure WorkerCreate(const Index: SG);
    procedure SetThreadPriority(const Value: TThreadPriority);
    procedure SetOnTasksFinished(const Value: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;

    // Process
    procedure AddTask(const AAsyncTask: TAsyncTask);
    procedure AddTasks(const AAsyncTasks: TAsyncTasks);
    procedure RandomizeTaskOrder;
    procedure SortTasks(const A: TArrayOfS4);
    procedure ClearTasks;
    procedure Pause;
    procedure Resume;
    procedure KillThreads;
    procedure WaitForNoWork;
    procedure WaitForNoThread;

    // Output
    function Working: BG;
    function RemainTaskCount: UG;

    // Called from Worker
    function PopAsyncTask: TAsyncTask;
    function PopAsyncTasks(const ACount: SG): TAsyncTasks;
    procedure WorkerStartWork;
    procedure WorkerFinishWork;
    procedure WorkerDestroy(const Index: SG);
    procedure InternalTasksFinished;

    // Input
    property MaxThreads: TNumericalIntervalArgument read FMaxThreads;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property OnTasksFinished: TNotifyEvent read FOnTasksFinished write SetOnTasksFinished;
  end;

implementation

uses
  SysUtils, Math,
  uStartState,
  uLog, uSorts, uMath, uCPU, uWorkerThread, uSxRandomGenerator;

{ TThreadPool }

procedure TThreadPool.AddTask(const AAsyncTask: TAsyncTask);
begin
  FQueueCriticalSection.Enter;
  try
    FQueue.Add(AAsyncTask);
  finally
    FQueueCriticalSection.Leave;
  end;
  QueueToThread;
end;

procedure TThreadPool.AddTasks(const AAsyncTasks: TAsyncTasks);
var
  i: SG;
begin
  FQueueCriticalSection.Enter;
  try
    for i := 0 to Length(AAsyncTasks) - 1 do
      FQueue.Add(AAsyncTasks[i]);
  finally
    FQueueCriticalSection.Leave;
  end;
  QueueToThread;
end;

procedure TThreadPool.ClearTasks;
begin
  FQueueCriticalSection.Enter;
  try
    FQueue.Clear;
  finally
    FQueueCriticalSection.Leave;
  end;
end;

constructor TThreadPool.Create;
begin
  inherited;

  FThreadPriority := tpLowest; // tpLower freezes other processes;

  FQueue := TData.Create;
  FQueueCriticalSection := TCriticalSection.Create;

  FRunThreads := 0;
  FMaxThreads := TNumericalIntervalArgument.Create;
  FMaxThreads.NumericalInterval.MinimalValue := 1;
  if TStartState.RunFromIDE then
  begin
    FMaxThreads.NumericalInterval.MaximalValue := 4; // CreateThread is too slow if run from IDE
  end
  else
    FMaxThreads.NumericalInterval.MaximalValue := 256;

  FMaxThreads.DefaultValue := Min(GCPU.LogicalProcessorCount, FMaxThreads.NumericalInterval.MaximalValue);
  FMaxThreads.Value := FMaxThreads.DefaultValue;
  FMaxThreads.OnChange := MaxThreadsChanged;

  FNoThreadEvent := TEvent.Create;
  FNoWorkEvent := TEvent.Create;
end;

destructor TThreadPool.Destroy;
begin
  try
    FQueueCriticalSection.Enter;
    try
      KillThreads;

      FreeAndNil(FQueue);
    finally
      FQueueCriticalSection.Leave;
    end;
    FreeAndNil(FQueueCriticalSection);

    FMaxThreads.Free;

    FreeAndNil(FNoThreadEvent);
    FreeAndNil(FNoWorkEvent);
  finally
    inherited;
  end;
end;

procedure TThreadPool.Pause;
var
  i: SG;
begin
  for i := 0 to Length(FThreads) - 1 do
    if FThreads[i] <> nil then
      FThreads[i].Suspend;
end;

procedure TThreadPool.QueueToThread;
begin
  if FRunThreads > FWorking then
    Resume;

  if FQueue.Count > (FRunThreads - FWorking) then
    SetRunThreads(FQueue.Count + FWorking);
end;

procedure TThreadPool.RandomizeTaskOrder;
var
  Count: SG;
  i, X: SG;
  RandomGenerator: TSxRandomGenerator;
begin
  FQueueCriticalSection.Enter;
  try
    RandomGenerator := TSxRandomGenerator.Create;
    try
      Count := FQueue.Count;
      if Count <= 1 then
        Exit;
      for i := 0 to Count - 1 do
      begin
        X := RandomGenerator.RangeU4(Count - 1);
        FQueue.Swap(i, X);
      {		T := FQueue[i];
        FQueue[i] := FQueue[X];
        FQueue[X] := T;}
      end;
    finally
      RandomGenerator.Free;
    end;
  finally
    FQueueCriticalSection.Leave;
  end;
end;

procedure TThreadPool.Resume;
var
  i: SG;
begin
  for i := 0 to Length(FThreads) - 1 do
    if FThreads[i] <> nil then
      FThreads[i].Resume;
end;

procedure TThreadPool.MaxThreadsChanged(Sender: TObject);
begin
  QueueToThread;
end;

procedure TThreadPool.SetOnTasksFinished(const Value: TNotifyEvent);
begin
  FOnTasksFinished := Value;
end;

procedure TThreadPool.SetRunThreads(Value: UG);
var
  i: SG;
begin
  Value := Range(1, Value, FMaxThreads.Value);
  if S4(Value) > FRunThreads then
  begin
    SetLength(FThreads, Value);
    for i := FRunThreads to Value - 1 do
    begin
      WorkerCreate(i);
    end;
  end;
end;

procedure TThreadPool.SortTasks(const A: TArrayOfS4);
var
  AIndex: TArrayOfSG;
  FQueue2: TData;
  i: SG;
  n: SG;
begin
  FQueueCriticalSection.Enter;
  try
    // Sort
    SetLength(AIndex, Length(A));
    FillOrderUG(AIndex[0], Length(AIndex));
    SortS4(False, False, PArraySG(AIndex), PArrayS4(A), Length(AIndex));

    // Add unsorted tasks
    n := FQueue.Count - Length(A);
    FQueue2 := TData.Create;
    for i := 0 to n - 1 do
    begin
      FQueue2.Add(TAsyncTask(FQueue[i]^));
    end;

    // Add sorted tasks
    for i := 0 to Length(A) - 1 do
    begin
      FQueue2.Add(TAsyncTask(FQueue[n + AIndex[i]]^));
    end;

    for i := 0 to FQueue.Count - 1 do
      FQueue.ReplaceObject(i, nil);

  //	FQueue[i] := nil;
    FQueue.Free;
    FQueue := FQueue2;
  finally
    FQueueCriticalSection.Leave;
  end;
end;

procedure TThreadPool.KillThreads;
begin
  ClearTasks;

  WaitForNoThread;
end;

procedure TThreadPool.WaitForNoWork;
begin
  if Working then
  begin
    FNoWorkEvent.WaitFor;
    FNoWorkEvent.ResetEvent;
  end;
{  while Working do
  begin
    WaitForWorkers;
  end;}
end;

procedure TThreadPool.WaitForNoThread;
begin
  if FRunThreads > 0 then
  begin
    FMaxThreads.Value := 0;
    QueueToThread;
    FNoThreadEvent.WaitFor;
    FNoThreadEvent.ResetEvent;
  end;
{  while (FRunThreads > 0) do
  begin
    WaitForWorkers;
  end;}
end;
{
procedure TThreadPool.WaitForWorkers;
begin
  Sleep(LoopSleepTime);
  CheckSynchronize; // If not called deadlock can appear
end;}

procedure TThreadPool.WorkerFinishWork;
begin
  AtomicDecrement(FWorking);
  if FWorking = 0 then
  begin
    if FQueue.Count = 0 then
      FNoWorkEvent.SetEvent;
  end;
end;

procedure TThreadPool.WorkerStartWork;
begin
  AtomicIncrement(FWorking);
end;

procedure TThreadPool.WorkerCreate(const Index: SG);
var
  WorkerThread: TWorkerThread;
begin
  WorkerThread := TWorkerThread.Create(Index, Self);
  AtomicIncrement(FRunThreads);
  FThreads[Index] := WorkerThread;
  WorkerThread.Resume;
end;

procedure TThreadPool.WorkerDestroy(const Index: SG);
begin
  FThreads[Index] := nil; // Write shared object
  AtomicDecrement(FRunThreads);
  if FRunThreads = 0 then
    FNoThreadEvent.SetEvent;
end;

function TThreadPool.PopAsyncTask: TAsyncTask;
begin
  FQueueCriticalSection.Enter;
  try
    if FQueue = nil then
      Result := nil
    else
      Result := FQueue.GetAndDeleteFirst as TAsyncTask;
//      Result := FQueue.GetAndDeleteLast as TAsyncTask;
  finally
    FQueueCriticalSection.Leave;
  end;
end;

function TThreadPool.PopAsyncTasks(const ACount: SG): TAsyncTasks;
var
  i: SG;
begin
  FQueueCriticalSection.Enter;
  try
    if FQueue = nil then
      Result := nil
    else
    begin
      SetLength(Result, ACount);
      for i := 0 to ACount - 1 do
      begin
        Result[i] := FQueue.GetAndDeleteFirst as TAsyncTask;
//        Result[i] := FQueue.GetAndDeleteLast as TAsyncTask;
      end;
    end;
  finally
    FQueueCriticalSection.Leave;
  end;
end;

function TThreadPool.RemainTaskCount: UG;
begin
  FQueueCriticalSection.Enter;
  try
    Result := FQueue.Count;
  finally
    FQueueCriticalSection.Leave;
  end;
end;

procedure TThreadPool.SetThreadPriority(const Value: TThreadPriority);
var
  i: SG;
begin
  if ThreadPriority <> Value then
  begin
    FThreadPriority := Value;
    for i := 0 to Length(FThreads) - 1 do
      if FThreads[i] <> nil then
        FThreads[i].Priority := FThreadPriority;
  end;
end;

function TThreadPool.Working: BG;
begin
  Result := (FQueue.Count > 0) or (FWorking > 0);
end;

procedure TThreadPool.InternalTasksFinished;
begin
  if Assigned(OnTasksFinished) then
    OnTasksFinished(Self);
end;

end.

