unit uSxServer;

interface

uses
  uTypes,
  Contnrs,
  ExtCtrls;

type
  TRunState = (rsStoped, rsStarted);

  TStopMode = (smNever {Green}, smAsPossible{Yellow}, smScheduled{Red});

  TSxServer = class
  private
    FEnableNewWork: Boolean;
    FBeforeRestartEventTime: SG;
    FStopMode: TStopMode;
    FRunState: TRunState;
    FRestartAfterStop: BG;
    FScheduler: TObjectList;

    FErrorTime: U8;
    FTimer: TTimer;
    FNextRestartTime: TDateTime;
    procedure FOnTimer(Sender: TObject);

    procedure SetEnableNewWork(const Value: Boolean);
    procedure SetBeforeRestartEventTime(const Value: SG);
    procedure SetRestartAfterStop(const Value: BG);
    procedure SetNextRestartTime(const Value: TDateTime);

  protected
    function CanStop: BG; virtual; abstract;
    procedure OnStop; virtual; abstract;
    procedure BeforeStopEvent; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure ForceStop;
    procedure Restart(const Force: BG);
    procedure ForceRestart;
    procedure CancelRestart;
    procedure ScheduleRestart(const StopMode: TStopMode);
    procedure ReportError(const Critical: BG);
    function CanAddWork: BG;

    property EnableNewWork: BG read FEnableNewWork write SetEnableNewWork;
    property BeforeRestartEventTime: SG read FBeforeRestartEventTime write SetBeforeRestartEventTime;
    property StopMode: TStopMode read FStopMode;
    property RunState: TRunState read FRunState;
    property RestartAfterStop: BG read FRestartAfterStop write SetRestartAfterStop;
    property NextRestartTime: TDateTime read FNextRestartTime write SetNextRestartTime;
    property Scheduler: TObjectList read FScheduler;
  end;

(*
external
	cmd line (windows message) - caused by manual or timer V1
	power message
	external watch dog

internal
	out of memory
	hang out thread
	timer V2 - EventScheduler dialog
*)

implementation

uses
  Forms,
  SysUtils,
  uSimulation,
  uSchedule,
  uMultiIns;

{ TSxServer }

constructor TSxServer.Create;
begin
  inherited;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := FOnTimer;
  if IsDebug then
    FTimer.Interval := 1000 // 1 second
  else
    FTimer.Interval := 5000; // 5 seconds
  FTimer.Enabled := True;

  FScheduler := TObjectList.Create;
end;

destructor TSxServer.Destroy;
begin
  FTimer.Enabled := False;
  FreeAndNil(FTimer);

  FreeAndNil(FScheduler);

  inherited;
end;

procedure TSxServer.CancelRestart;
begin
  FStopMode := smNever;
  FNextRestartTime := 0;
end;

procedure TSxServer.ForceRestart;
begin
  RestartAfterStop := True;
  ForceStop;
end;

procedure TSxServer.ForceStop;
begin
  OnStop;
  FStopMode := smNever;
  FRunState := rsStoped;
  if RestartAfterStop then
  begin
    RestartAfterClose := True;
    Application.Terminate;
  end;
end;

procedure TSxServer.ReportError(const Critical: BG);
var
  Increment: SG;
begin
  if FRunState <> rsStarted then
    raise Exception.Create('Error reported but server is stopped!');
  if FErrorTime = 0 then
  begin
    FErrorTime := GTime;
    if IsDebug then
      Increment := 5 * Second
    else
      Increment := 1 * Minute;
    FNextRestartTime := Now + Increment / MSecsPerDay;
  end;
  Restart(Critical);
end;

procedure TSxServer.Restart(const Force: BG);
begin
  if Force then
    ForceRestart
  else
    ScheduleRestart(smAsPossible);
end;

procedure TSxServer.ScheduleRestart(const StopMode: TStopMode);
begin
  RestartAfterStop := True;
  FStopMode := StopMode;
  if FStopMode = smAsPossible then
    BeforeStopEvent;
end;

procedure TSxServer.SetBeforeRestartEventTime(const Value: SG);
begin
  if FBeforeRestartEventTime <> Value then
  begin
    FBeforeRestartEventTime := Value;
  end;
end;

procedure TSxServer.SetEnableNewWork(const Value: BG);
begin
  if FEnableNewWork <> Value then
  begin
    FEnableNewWork := Value;
  end;
end;

procedure TSxServer.SetRestartAfterStop(const Value: BG);
begin
  FRestartAfterStop := Value;
end;

procedure TSxServer.Start;
begin
  FRunState := rsStarted;
  FStopMode := smNever;
end;

procedure TSxServer.Stop;
begin
  if FRunState = rsStoped then Exit;

  if CanStop then
  begin
    ForceStop;
  end
  else
  begin
    FStopMode := smAsPossible;
    BeforeStopEvent;
  end;
end;

procedure TSxServer.FOnTimer(Sender: TObject);
var
  i: SG;
begin
  for i := 0 to Scheduler.Count - 1 do
  begin
    if TSchedule(Scheduler[i]).IsActive then
    begin
      TSchedule(Scheduler[i]).UpdateNextRun;
      Stop;
    end;
  end;

  if FNextRestartTime <> 0 then //(StopMode = smAsPossible) then
//    if (IntervalFrom(FErrorTime) > 1 * Minute) or CanStop then
    if (FNextRestartTime <= Now) and CanStop then
    begin
      ForceStop;
    end;
end;

function TSxServer.CanAddWork: BG;
begin
  Result := (RunState = rsStarted) and (StopMode <> smAsPossible);
end;

procedure TSxServer.SetNextRestartTime(const Value: TDateTime);
begin
  if FNextRestartTime <> Value then
  begin
    FNextRestartTime := Value;
  end;
end;

end.
