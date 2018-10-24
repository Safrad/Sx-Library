unit uStopwatch;

interface

uses
  uTypes,
  uTimeSpan,
  Windows;

type
  TStopwatch = class
  private
    FStartTime: U8;
    FStopTime: U8;
    FElapsed: TTimeSpan;
    function GetIsRunning: BG;
    function GetElapsedTicks: U8;
    function GetElapsed: TTimeSpan;
  public
    constructor Create;
    destructor Destroy; override;

    // Stops time interval measurement and resets the elapsed time to zero.
    procedure Reset;

    // Stops time interval measurement, resets the elapsed time to zero, and starts measuring elapsed time.
    procedure Restart;

    // Starts, or resumes, measuring elapsed time for an interval.
    procedure Start;

    // Stops or pauses measuring elapsed time for an interval.
    procedure Stop;

    // Gets the total elapsed time measured by the current instance.
    property Elapsed: TTimeSpan read GetElapsed;

    // Gets a value indicating whether the Stopwatch timer is running.
    property IsRunning: BG read GetIsRunning;
  end;

implementation

uses
  uMath, uMainTimer, SysUtils;

{ TStopwatch }

function TStopwatch.GetElapsedTicks: U8;
begin
  if FStartTime = 0 then
    Result := 0 // Start method no called
  else if FStopTime = 0 then
    Result := MainTimer.IntervalFrom(FStartTime) // Stop method no called
  else
    Result := TimeDifference(FStopTime, FStartTime);
end;

function TStopwatch.GetElapsed: TTimeSpan;
begin
  FElapsed.Ticks := GetElapsedTicks;
  Result := FElapsed;
end;

function TStopwatch.GetIsRunning: BG;
begin
  Result := (FStartTime > 0) and (FStopTime = 0);
end;

procedure TStopwatch.Reset;
begin
  FStartTime := 0;
  FStopTime := 0;
end;

procedure TStopwatch.Restart;
begin
  Reset;
  Start;
end;

procedure TStopwatch.Start;
begin
  if FStopTime <> 0 then
  begin
    Assert(FStartTime <> 0);
    Inc(FStartTime, MainTimer.IntervalFrom(FStopTime));
    FStopTime := 0;
  end
  else
  begin
    // Stop method not called
    if FStartTime = 0 then
    begin
      // Start method not called
      FStartTime := MainTimer.Value.Ticks;
    end
    else
      raise Exception.Create('Stopwatch already started.');
  end;
end;

procedure TStopwatch.Stop;
begin
  if FStartTime = 0 then
  begin
    raise Exception.Create('Stopwatch is not started.');
  end;
  FStopTime := MainTimer.Value.Ticks;
end;

constructor TStopwatch.Create;
begin
  inherited;

  FElapsed := TTimeSpan.Create;
end;

destructor TStopwatch.Destroy;
begin
  FreeAndNil(FElapsed);

  inherited;
end;

end.

