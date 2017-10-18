unit uSxThreadTimer;

interface

uses
  uTypes,
  Classes,
  uSxThread;

type
  TSxThreadTimer = class(TSxThread)
  private
    FRun: BG;
    FEnabled: BG;
    FInterval: SG;
    FOnTimer: TNotifyEvent;
    FWorkingTime: U8;
    FIdleTime: U8;
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetEnabled(const Value: BG);
    procedure SetInterval(const Value: SG);

    procedure InternalExecute;
  protected
    procedure Execute; override;


  public
    constructor Create(CreateSuspended: Boolean);
    procedure StopAndDestroy;

    property Enabled: BG read FEnabled write SetEnabled;
    property Interval: SG read FInterval write SetInterval;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;

    property WorkingTime: U8 read FWorkingTime;
    property IdleTime: U8 read FIdleTime;
  end;

implementation

uses
  uMsg, uStopwatch, uMath,
  Windows, Forms, SysUtils, Math;

{ TSxThreadTimer }

constructor TSxThreadTimer.Create;
begin
  inherited Create(CreateSuspended);

  FEnabled := True;
end;

procedure TSxThreadTimer.Execute;
begin
  FRun := True;
  inherited;
  try
    InternalExecute;
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
end;

procedure TSxThreadTimer.InternalExecute;
var
  StopWatch: TStopWatch;
  StartTime, SleepTime: U8;
begin
  StopWatch := TStopwatch.Create;
  try
    StartTime := PerformanceCounter;
    while FRun and (not Application.Terminated) do
    begin
      StopWatch.Start;
      FOnTimer(Self);
      StopWatch.Stop;
      Inc(FWorkingTime, StopWatch.ElapsedTicks);



      SleepTime := Interval - RoundDivU8((PerformanceCounter - StartTime) * 1000, PerformanceFrequency) mod Interval;
      StopWatch.Start;
      Sleep(Max(0, SleepTime));
      StopWatch.Stop;
      Inc(FIdleTime, StopWatch.ElapsedTicks);
    end;
  finally
    StopWatch.Free;
  end;
end;

procedure TSxThreadTimer.SetEnabled(const Value: BG);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      Resume
    else
      Suspend;
  end;
end;

procedure TSxThreadTimer.SetInterval(const Value: SG);
begin
  FInterval := Value;
end;

procedure TSxThreadTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  FOnTimer := Value;
end;

procedure TSxThreadTimer.StopAndDestroy;
begin
  FRun := False;
end;

end.
