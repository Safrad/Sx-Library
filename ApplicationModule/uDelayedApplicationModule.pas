unit uDelayedApplicationModule;

interface

uses
  uApplicationModule,
  uSxThread,
  uTimeSpan;

type
  TDelayedApplicationModule = class(TApplicationModule)
  private
    FThread: TSxThread;
    FDelayTime: TTimeSpan;
    procedure SetDelayTime(const Value: TTimeSpan);
  public
    constructor Create;

    procedure Load; override;
    procedure Unload; override;

    procedure CallOnLoad;

    property DelayTime: TTimeSpan read FDelayTime write SetDelayTime;
  end;

implementation

uses
  SysUtils,
  uApplicationModuleThread;

{ TDelayedApplicationModule }

procedure TDelayedApplicationModule.CallOnLoad;
begin
  OnLoad;
end;

constructor TDelayedApplicationModule.Create;
begin
  inherited;

  FDelayTime.Seconds := 10;
end;

procedure TDelayedApplicationModule.Load;
begin
  if FThread <> nil then
  begin
    FThread := TApplicationModuleThread.Create;
    TApplicationModuleThread(FThread).Module := Self;
    FThread.Start;
  end;
end;

procedure TDelayedApplicationModule.SetDelayTime(const Value: TTimeSpan);
begin
  FDelayTime := Value;
end;

procedure TDelayedApplicationModule.Unload;
begin
  if FThread <> nil then
  begin
    FThread.TerminateAndWaitFor;
    FreeAndNil(FThread);

    inherited;
  end;
end;

end.
