unit uApplicationModuleThread;

interface

uses
  uSxThread,
  uTimeSpan,
  uApplicationModule;

type
  TApplicationModuleThread = class(TSxThread)
  private
    FModule: TApplicationModule;
    FDelay: TTimeSpan;
    procedure SetModule(const Value: TApplicationModule);
  protected
    procedure Execute; override;
    procedure Terminate; override;
  public
    constructor Create;

    property Module: TApplicationModule read FModule write SetModule;
  end;

implementation

uses
  SysUtils,

  uLog,
  uMath;

{ TApplicationModuleThread }

constructor TApplicationModuleThread.Create;
begin
  inherited;

  FreeOnTerminate := True;
end;

procedure TApplicationModuleThread.Execute;
begin
  Assert(FModule <> nil);
  Assert(FModule.StartupType = stDelayedStart);

  inherited;

  FDelay := FModule.DelayTime;
  PreciseSleep(FDelay);

  if Terminated then
    Exit;

  try
    FModule.Load;
  except
    on E: Exception do
      MainLog.LogException(E);
  end;
end;

procedure TApplicationModuleThread.SetModule(const Value: TApplicationModule);
begin
  FModule := Value;
end;

procedure TApplicationModuleThread.Terminate;
begin
  inherited Terminate;

  FDelay.Ticks := 0; // For fast leaving of PreciseSleep method
end;

end.
