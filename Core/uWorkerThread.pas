unit uWorkerThread;

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
  Windows, SysUtils, Classes;

{ TWorkerThread }

constructor TWorkerThread.Create(const AId: SG; const AThreadPool: TThreadPool);
begin
  FreeOnTerminate := True;
  inherited Create(True);

  Priority := AThreadPool.ThreadPriority;
  Name := 'Worker Thread Id ' + IntToStr(AId);
  {$ifdef UNICODE}
  if IsDebug then
  begin
    RegisterExpectedMemoryLeak(WorkerThread);
    NameThreadForDebugging(AnsiString('Process ' + IntToStr(i)));
  end;
  {$endif}

  FId := AId;
  FThreadPool := AThreadPool;
end;

procedure TWorkerThread.Execute;
var
  AsyncTask: TAsyncTask;
begin
  inherited;

  try
    while FId < {FThreadPool.FRunThreads <=} FThreadPool.MaxThreads do // Read shared object
    begin
      AsyncTask := FThreadPool.PopAsyncTask;
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
        // No command available, add new command can wake up thread
        Suspend;
//          Exit; // Can be used for debbuging
//         Sleep(FThreadPool.RunThreads * LoopSleepTime); // Can be used for debbuging
      end;
    end;
  finally
    FThreadPool.WorkerDestroy(FId);
  end;
end;

end.

