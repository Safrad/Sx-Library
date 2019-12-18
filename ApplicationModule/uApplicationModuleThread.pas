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
    procedure SetDelay(const Value: TTimeSpan);
  protected
    procedure Execute; override;
  public
    constructor Create;

    procedure Terminate; override;
    property Delay: TTimeSpan read FDelay write SetDelay;
    property Module: TApplicationModule read FModule write SetModule;
  end;

implementation

uses
  SysUtils,

  uMainLog,
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

procedure TApplicationModuleThread.SetDelay(const Value: TTimeSpan);
begin
  FDelay := Value;
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
