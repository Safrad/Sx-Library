unit uWorkerThread;

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  uTypes, uAsyncTask, uThreadPool, uSxThread;

type
  TWorkerThread = class(TSxThread)
  private
    FId: SG;
    FThreadPool: TThreadPool;
  protected
    procedure Execute; override;
  public
    constructor Create(const AId: SG; const AThreadPool: TThreadPool);
    property ThreadPool: TThreadPool read FThreadPool write FThreadPool;
  end;

implementation

uses
  Math, SysUtils;

{ TWorkerThread }

constructor TWorkerThread.Create(const AId: SG; const AThreadPool: TThreadPool);
begin
  FreeOnTerminate := True;
  inherited Create;

  Priority := AThreadPool.ThreadPriority;
  Name := 'Worker Id ' + IntToStr(AId);

  FId := AId;
  FThreadPool := AThreadPool;
end;

procedure TWorkerThread.Execute;
var
  AsyncTask: TAsyncTask;
  AsyncTasks: TAsyncTasks;
  i: SG;
begin
  inherited;

  try
    while FId < {FThreadPool.FRunThreads <=} FThreadPool.MaxThreads.Value do // Read shared object
    begin
//      AsyncTask := FThreadPool.PopAsyncTask;
      AsyncTasks := FThreadPool.PopAsyncTasks(Max(1, FThreadPool.RemainTaskCount div UG(FThreadPool.MaxThreads.Value) div 10));
      for i := 0 to Length(AsyncTasks) - 1 do
      begin
        AsyncTask := AsyncTasks[i];
        if AsyncTask <> nil then
        begin
          FThreadPool.WorkerStartWork;
          try
            AsyncTask.Thread := Self;
            try
              AsyncTask.Execute;
            except
              // Task failed
              on E: Exception do
              begin
                try
                  AsyncTask.SynchronizedFail;
                except
                  // No code
                end;
              end;
            end;
          finally
            try
              AsyncTask.SynchronizedFinish;
            except
              // No code
            end;
            FThreadPool.WorkerFinishWork;
            if not FThreadPool.Working then
            begin
              if Assigned(FThreadPool.OnTasksFinished) then
              begin
                try
                  AsyncTask.Synchronize(FThreadPool.InternalTasksFinished);
                except
                  // No code
                end;
              end;
            end;
            try
              AsyncTask.Free;
            except
              // No code
            end;
          end;
        end
        else
        begin
          // No task available, "ThreadPool.AddTask" can wake up thread
          Suspend;
  //          Exit; // Can be used for debbuging
  //         Sleep(FThreadPool.RunThreads * LoopSleepTime); // Can be used for debbuging
        end;
      end;
    end;
  finally
    FThreadPool.WorkerDestroy(FId);
  end;
end;

end.

