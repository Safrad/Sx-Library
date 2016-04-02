unit uStopwatch;

interface

uses
  uTypes, Windows;

type
  TStopwatch = class
  private
    FStartTime: U8;
    FStopTime: U8;
  public
    // Stops time interval measurement and resets the elapsed time to zero.
    procedure Reset;

    // Stops time interval measurement, resets the elapsed time to zero, and starts measuring elapsed time.
    procedure Restart;

    // Starts, or resumes, measuring elapsed time for an interval.
    procedure Start;

    // Stops measuring elapsed time for an interval.
    procedure Stop;

    // Gets the total elapsed time measured by the current instance, in milliseconds.
    function ElapsedMilliseconds: U8;

    // Gets the total elapsed time measured by the current instance, in timer ticks.
    function ElapsedTicks: U8;

    // Gets the total elapsed time measured by the current instance.
    function ElapsedTime: TDateTime;

    // Gets a value indicating whether the Stopwatch timer is running.
    function IsRunning: BG;
  end;

implementation

uses
  uMath, SysUtils, Classes;

{ TStopwatch }

function TStopwatch.ElapsedMilliseconds: U8;
begin
  Result := RoundDivU8(Second * ElapsedTicks, PerformanceFrequency);
end;

function TStopwatch.ElapsedTicks: U8;
begin
  if FStopTime = 0 then
    Result := PerformanceCounter - FStartTime
  else
    Result := FStopTime - FStartTime;
end;

function TStopwatch.ElapsedTime: TDateTime;
begin
  Result := ElapsedTicks / (PerformanceFrequency * (Day div Second));
end;

function TStopwatch.IsRunning: BG;
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
var
  LastSleepInterval: U8;
begin
  if FStopTime <> 0 then
  begin
    Assert(FStartTime <> 0);
    LastSleepInterval := FStopTime - FStartTime;
    Inc(FStartTime, LastSleepInterval);
    FStopTime := 0;
  end
  else
  begin
    // Stop method not called
    if FStartTime = 0 then
    begin
      // Start method not called
      FStartTime := PerformanceCounter;
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
  FStopTime := PerformanceCounter;
end;

end.

