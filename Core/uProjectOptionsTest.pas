unit uProjectOptionsTest;

interface

uses TestFrameWork;

type
  TProjectOptionsTest = class(TTestCase)
  published
    procedure Test;
    procedure TestThreads;
  end;

implementation

uses
  SysUtils,
  uThreadPool,
  uAsyncTaskForProjectOptionsTest,
	uTypes, uFiles, uProjectOptions;

{ TProjectOptionsTest }

procedure TProjectOptionsTest.Test;
var
  ProjectOptions: TProjectOptions;
begin
  ProjectOptions := TProjectOptions.Create;
  try
    ProjectOptions.RWBDSProj(DataDir + 'Test.bdsproj', False);
  finally
    ProjectOptions.Free;
  end;
end;

procedure TProjectOptionsTest.TestThreads;
var
  ThreadPool: TThreadPool;
  AsyncTask: TAsyncTaskForProjectOptionsTest;
  i: SG;
begin
  ThreadPool := TThreadPool.Create;
  try
    for i := 0 to 99 do
    begin
      AsyncTask := TAsyncTaskForProjectOptionsTest.Create;
      ThreadPool.AddTask(AsyncTask);
    end;

    ThreadPool.WaitForNoWork;
  finally
    ThreadPool.Free;
  end;
end;

initialization
	RegisterTest('Project Options Test', TProjectOptionsTest.Suite);
end.
