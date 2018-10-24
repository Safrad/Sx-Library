unit uAsyncTask;

interface

uses
  uSxThread;

type
  TAsyncTask = class
  private
    procedure Fail;
    procedure Finish;
  public
    Thread: TSxThread;
    OnFail: TNotifyEvent; // Synchronized method
    OnFinish: TNotifyEvent; // Synchronized method

    procedure SynchronizedFail; virtual;
    procedure SynchronizedFinish; virtual;
    procedure Execute; virtual; abstract;
    procedure Synchronize(const AThreadMethod: TThreadMethod);
    procedure SynchronizeOrSkip(const AThreadMethod: TThreadMethod);
  end;

  TAsyncTasks = array of TAsyncTask;

implementation

uses uTypes, Windows;

{ TAsyncTask }

procedure TAsyncTask.Fail;
begin
  if Assigned(OnFail) then
    OnFail(Self);
end;

procedure TAsyncTask.Finish;
begin
  if Assigned(OnFinish) then
    OnFinish(Self);
end;

procedure TAsyncTask.SynchronizedFail;
begin
  if Assigned(OnFail) then
    Synchronize(Fail);
end;

procedure TAsyncTask.SynchronizedFinish;
begin
  if Assigned(OnFinish) then
    Synchronize(Finish);
end;


var
  LastCall: U4;

function CanCall: Boolean;
begin
  Result := (GetTickCount - LastCall) >= LoopSleepTime;
  if Result = True then
    LastCall := GetTickCount;
end;

procedure TAsyncTask.Synchronize(const AThreadMethod: TThreadMethod);
begin
  Thread.Synchronize(Thread, AThreadMethod);
end;

procedure TAsyncTask.SynchronizeOrSkip(const AThreadMethod: TThreadMethod);
begin
  if CanCall then
    Thread.Synchronize(Thread, AThreadMethod);
end;

end.
 
