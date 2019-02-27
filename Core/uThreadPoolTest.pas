unit uThreadPoolTest;

interface

uses
  TestFramework;

type
  TThreadPoolTest = class(TTestCase)
  published
    procedure EmptyTest;
    procedure Test;
    procedure ManyTasksTest;
  end;

implementation

uses
  uTypes, Windows, uAsyncTaskForTest, uThreadPool, uMath;

{ TThreadPoolTest }

procedure TThreadPoolTest.EmptyTest;
var
  ThreadPool: TThreadPool;
begin
  ThreadPool := TThreadPool.Create;
  try
    ThreadPool.MaxThreads.Value := 4;
    ThreadPool.WaitForNoWork;

    ThreadPool.MaxThreads.Value := 1;
    ThreadPool.WaitForNoThread;
  finally
    ThreadPool.Free;
  end;
end;

procedure TThreadPoolTest.Test;
var
  ThreadPool: TThreadPool;
  AsyncTask: TAsyncTaskForTest;
begin
  ThreadPool := TThreadPool.Create;
  try
    ThreadPool.MaxThreads.Value := 4;

    AsyncTask := TAsyncTaskForTest.Create;
    ThreadPool.AddTask(AsyncTask);

    Sleep(100);
    ThreadPool.Pause;
    Sleep(100);
    ThreadPool.Resume;
    Sleep(100);
    ThreadPool.ClearTasks;

    ThreadPool.WaitForNoWork;
  finally
    ThreadPool.Free;
  end;
end;

procedure TThreadPoolTest.ManyTasksTest;
var
  ThreadPool: TThreadPool;
  AsyncTask: TAsyncTaskForTest;
  i: SG;
begin
  ThreadPool := TThreadPool.Create;
  try
    ThreadPool.MaxThreads.Value := 4;

    for i := 0 to 999 do
    begin
      AsyncTask := TAsyncTaskForTest.Create;
      ThreadPool.AddTask(AsyncTask);
    end;
    ThreadPool.RandomizeTaskOrder;

    ThreadPool.WaitForNoWork;
  finally
    ThreadPool.Free;
  end;
end;

initialization
  RegisterTest('Thread Pool Test', TThreadPoolTest.Suite);

end.

