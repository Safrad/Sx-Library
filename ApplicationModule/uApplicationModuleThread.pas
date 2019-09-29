unit uApplicationModuleThread;

interface

uses
  uSxThread,
  uTimeSpan,
  uDelayedApplicationModule;

type
  TApplicationModuleThread = class(TSxThread)
  private
    FModule: TDelayedApplicationModule;
    FDelay: TTimeSpan;
    procedure SetModule(const Value: TDelayedApplicationModule);
  protected
    procedure Execute; override;
  public
    constructor Create;

    procedure Terminate; override;
    property Module: TDelayedApplicationModule read FModule write SetModule;
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

end;

procedure TApplicationModuleThread.Execute;
begin
  Assert(FModule <> nil);

  inherited;

  FDelay := FModule.DelayTime;
  PreciseSleep(FDelay);

  if Terminated then
    Exit;

  try
    FModule.CallOnLoad;
  except
    on E: Exception do
      MainLog.LogException(E);
  end;
end;

procedure TApplicationModuleThread.SetModule(const Value: TDelayedApplicationModule);
begin
  FModule := Value;
end;

procedure TApplicationModuleThread.Terminate;
begin
  inherited Terminate;

  FDelay.Ticks := 0; // For fast leaving of PreciseSleep method
end;

end.
