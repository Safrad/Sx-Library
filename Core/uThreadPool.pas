unit uThreadPool;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  uTypes, uData, uAsyncTask, Windows, Classes;

// TODO: Wait for task, task priority

type
  TThreads = array of TThread;

  TThreadPool = class
  private
    FMaxThreads: SG;
    FRunThreads: S4;
    FWorking: S4;
    FThreads: TThreads;
    FQueue: TData; // array of TAsyncTask;
    FQueueCriticalSection: TRTLCriticalSection;
    FThreadPriority: TThreadPriority;
    FOnTasksFinished: TNotifyEvent;
    FUseCoInitialize: BG;
    procedure SetRunThreads(Value: SG);
    procedure SetMaxThreads(Value: SG);
    procedure QueueToThread;
    procedure WaitForWorkers;
    procedure WorkerCreate(const Index: SG);
    procedure SetThreadPriority(const Value: TThreadPriority);
    procedure SetOnTasksFinished(const Value: TNotifyEvent);
    procedure SetUseCoInitialize(const Value: BG);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTask(const AAsyncTask: TAsyncTask);
    procedure AddTasks(const AAsyncTasks: TAsyncTasks);
    procedure RandomizeTaskOrder;
    procedure SortTasks(const A: TArrayOfS4);
    function PopAsyncTask: TAsyncTask;
    procedure ClearTasks;
    function Working: BG;
    function RemainTaskCount: UG;
    procedure Pause;
    procedure Resume;
    procedure KillThreads;
    procedure WaitForNoWork;
    procedure WaitForNoThread;
    procedure WorkerStartWork;
    procedure WorkerFinishWork;
    procedure WorkerDestroy(const Index: SG);
    procedure InternalTasksFinished;

    property MaxThreads: SG read FMaxThreads write SetMaxThreads;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property OnTasksFinished: TNotifyEvent read FOnTasksFinished write SetOnTasksFinished;
    property UseCoInitialize: BG read FUseCoInitialize write SetUseCoInitialize;
  end;

implementation

uses
  uLog, uSorts, uMath, uCPU, uWorkerThread, SysUtils;

{ TThreadPool }

procedure TThreadPool.AddTask(const AAsyncTask: TAsyncTask);
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    FQueue.Add(AAsyncTask);
  finally
    LeaveCriticalSection(FQueueCriticalSection);
  end;
  QueueToThread;
end;

procedure TThreadPool.AddTasks(const AAsyncTasks: TAsyncTasks);
var
  i: SG;
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    for i := 0 to Length(AAsyncTasks) - 1 do
      FQueue.Add(AAsyncTasks[i]);
  finally
    LeaveCriticalSection(FQueueCriticalSection);
  end;
  QueueToThread;
end;

procedure TThreadPool.ClearTasks;
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    FQueue.Clear;
  finally
    LeaveCriticalSection(FQueueCriticalSection);
  end;
end;

constructor TThreadPool.Create;
begin
  inherited;

  FThreadPriority := tpLowest; // tpLower freezes other proccesses;

  FQueue := TData.Create;
  InitializeCriticalSection(FQueueCriticalSection);

  FRunThreads := 0;
  FMaxThreads := GCPU.LogicalProcessorCount;
end;

destructor TThreadPool.Destroy;
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    KillThreads;

    FreeAndNil(FQueue);
  finally
    LeaveCriticalSection(FQueueCriticalSection);
  end;
  DeleteCriticalSection(FQueueCriticalSection);

  inherited;
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
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    Count := FQueue.Count;
    if Count <= 1 then
      Exit;
    for i := 0 to Count - 1 do
    begin
      X := Random(Count);
      FQueue.Swap(i, X);
  {		T := FQueue[i];
      FQueue[i] := FQueue[X];
      FQueue[X] := T;}
    end;
  finally
    LeaveCriticalSection(FQueueCriticalSection);
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

procedure TThreadPool.SetMaxThreads(Value: SG);
begin
  if Value <> FMaxThreads then
  begin
    FMaxThreads := Value;
    QueueToThread;
  end;
end;

procedure TThreadPool.SetOnTasksFinished(const Value: TNotifyEvent);
begin
  FOnTasksFinished := Value;
end;

procedure TThreadPool.SetRunThreads(Value: SG);
var
  i: SG;
begin
  Value := Range(1, Value, FMaxThreads);
  if Value > FRunThreads then
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
  EnterCriticalSection(FQueueCriticalSection);
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
    LeaveCriticalSection(FQueueCriticalSection);
  end;
end;

procedure TThreadPool.KillThreads;
begin
  ClearTasks;

  WaitForNoThread;
end;

procedure TThreadPool.WaitForNoWork;
begin
  while Working do
  begin
    WaitForWorkers;
  end;
end;

procedure TThreadPool.WaitForNoThread;
begin
  FMaxThreads := 0;
  QueueToThread;
  while (FRunThreads > 0) do
  begin
    WaitForWorkers;
  end;
end;

procedure TThreadPool.WaitForWorkers;
begin
  Sleep(LoopSleepTime);
  CheckSynchronize; // If not called deadlock can appear
end;

procedure TThreadPool.WorkerFinishWork;
begin
  InterlockedDecrement(FWorking);
end;

procedure TThreadPool.WorkerStartWork;
begin
  InterlockedIncrement(FWorking);
end;

procedure TThreadPool.WorkerCreate(const Index: SG);
var
  WorkerThread: TWorkerThread;
begin
  WorkerThread := TWorkerThread.Create(Index, Self);
  WorkerThread.UseCoInitialize := UseCoInitialize;
  InterlockedIncrement(FRunThreads);
  FThreads[Index] := WorkerThread;
  WorkerThread.Resume;
end;

procedure TThreadPool.WorkerDestroy(const Index: SG);
begin
  FThreads[Index] := nil; // Write shared object
  InterlockedDecrement(FRunThreads);
end;

function TThreadPool.PopAsyncTask: TAsyncTask;
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    if FQueue = nil then
      Result := nil
    else
      Result := FQueue.GetAndDeleteFirst as TAsyncTask;
  finally
    LeaveCriticalSection(FQueueCriticalSection);
  end;
end;

function TThreadPool.RemainTaskCount: UG;
begin
  EnterCriticalSection(FQueueCriticalSection);
  try
    Result := FQueue.Count;
  finally
    LeaveCriticalSection(FQueueCriticalSection);
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

procedure TThreadPool.SetUseCoInitialize(const Value: BG);
begin
  FUseCoInitialize := Value;
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

