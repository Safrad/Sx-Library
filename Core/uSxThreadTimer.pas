unit uSxThreadTimer;

interface

uses
  uTypes,
  Classes,
  uTimeSpan,
  uSxThread;

type
  TSxThreadTimer = class(TSxThread)
  private
    FEnabled: BG;
    FInterval: TTimeSpan;
    FOnTimer: TNotifyEvent;
    FWorkingTime: U8;
    FIdleTime: U8;
    FSleepTime: TTimeSpan;
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetEnabled(const Value: BG);

    procedure InternalExecute;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate; reintroduce;

    property Enabled: BG read FEnabled write SetEnabled;
    property Interval: TTimeSpan read FInterval;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;

    property WorkingTime: U8 read FWorkingTime;
    property IdleTime: U8 read FIdleTime;
  end;

implementation

uses
  uMsg, uStopwatch, uMath,
  Windows, SysUtils, Math;

{ TSxThreadTimer }

constructor TSxThreadTimer.Create;
begin
  inherited Create;

  FEnabled := False;
  FInterval := TTimeSpan.Create;
  FInterval.Seconds := 1;

  FSleepTime := TTimeSpan.Create;
end;

destructor TSxThreadTimer.Destroy;
begin
  FreeAndNil(FSleepTime);
  FreeAndNil(FInterval);

  inherited;
end;

procedure TSxThreadTimer.Execute;
begin
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
  Stopwatch: TStopwatch;
  StartTime: U8;
begin
  Stopwatch := TStopwatch.Create;
  try
    StartTime := PerformanceCounter;
    while (not Terminated) do
    begin
      Stopwatch.Start;
      FOnTimer(Self);
      Stopwatch.Stop;
      Inc(FWorkingTime, Stopwatch.Elapsed.Ticks);

      if FInterval.Ticks <= 0 then
        FSleepTime.Ticks := 0
      else
        FSleepTime.Ticks := FInterval.Ticks - IntervalFrom(StartTime) mod FInterval.Ticks;
      Stopwatch.Start;
      PreciseSleep(FSleepTime);
      Stopwatch.Stop;
      Inc(FIdleTime, Stopwatch.Elapsed.Ticks);
    end;
  finally
    Stopwatch.Free;
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

procedure TSxThreadTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  FOnTimer := Value;
end;

procedure TSxThreadTimer.Terminate;
begin
  inherited Terminate;

  FSleepTime.Ticks := 0; // For fast leaving of PreciseSleep method 
end;

end.
