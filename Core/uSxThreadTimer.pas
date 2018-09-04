unit uSxThreadTimer;

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  uTypes,
  Classes,
  uTimeSpan,
  uStopwatch,
  uSxThread;

type
  TSxThreadTimer = class(TSxThread)
  private
    FEnabled: BG;
    FInterval: TTimeSpan;
    FOnTimer: TNotifyEvent;
    FWorkingStopwatch: TStopwatch;
    FIdleStopwatch: TStopwatch;
    FSleepTime: TTimeSpan;
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetEnabled(const Value: BG);

    procedure InternalExecute;
    function GetIdleTime: TTimeSpan;
    function GetWorkingTime: TTimeSpan;
  protected
    procedure Execute; override;
    procedure ExecuteOnTimer; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate; override;

    property Enabled: BG read FEnabled write SetEnabled;
    property Interval: TTimeSpan read FInterval;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;

    property WorkingTime: TTimeSpan read GetWorkingTime;
    property IdleTime: TTimeSpan read GetIdleTime;
  end;

implementation

uses
  uMsg, uMath,
  Windows, SysUtils, Math;

{ TSxThreadTimer }

constructor TSxThreadTimer.Create;
begin
  inherited Create;

  FEnabled := False;
  FInterval := TTimeSpan.Create;
  FInterval.Seconds := 1;

  FWorkingStopwatch := TStopwatch.Create;
  FIdleStopwatch := TStopwatch.Create;
  FSleepTime := TTimeSpan.Create;
end;

destructor TSxThreadTimer.Destroy;
begin
  FreeAndNil(FWorkingStopwatch);
  FreeAndNil(FIdleStopwatch);
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

function TSxThreadTimer.GetIdleTime: TTimeSpan;
begin
  Result := FIdleStopwatch.Elapsed;
end;

function TSxThreadTimer.GetWorkingTime: TTimeSpan;
begin
  Result := FWorkingStopwatch.Elapsed;
end;

procedure TSxThreadTimer.InternalExecute;
var
  StartTime: U8;
begin
  StartTime := PerformanceCounter;
  while (not Terminated) do
  begin
    FWorkingStopwatch.Start;
    ExecuteOnTimer;
    FWorkingStopwatch.Stop;

    if FInterval.Ticks <= 0 then
      FSleepTime.Ticks := 0
    else
      FSleepTime.Ticks := FInterval.Ticks - IntervalFrom(StartTime) mod FInterval.Ticks;
    FIdleStopwatch.Start;
    PreciseSleep(FSleepTime);
    FIdleStopwatch.Stop;
  end;
end;

procedure TSxThreadTimer.ExecuteOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
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
