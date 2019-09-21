unit uDTimerTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TDTimerTest = class(TTestCase)
  private
    FEventCount: UG;
    FEventCount2: UG;
    procedure OnTimer(Sender: TObject);
    procedure OnTimer2(Sender: TObject);
  published
    procedure TestInterval;
    procedure TestIntervalInSecond;
    procedure TestFrequency;
    procedure TestTwoTimers;
  end;

implementation

uses
  SysUtils,
  Forms,

  uStopwatch,
  uDIdleTimer,
  uDTimer,
  uLog;

{ TDTimerTest }

procedure TDTimerTest.OnTimer(Sender: TObject);
begin
  Inc(FEventCount);

  if LogDebug then
    MainLog.Add('OnTimer', mlDebug);
end;

procedure TDTimerTest.TestInterval;
var
  DTimer: TDTimer;
  Stopwatch: TStopwatch;
begin
  FEventCount := 0;
  DTimer := TDTimer.Create(nil);
  Stopwatch := TStopwatch.Create;
  try
    DTimer.EventStep := esInterval;
    DTimer.Interval := 51; // ms
    DTimer.OnTimer := OnTimer;
    DTimer.Enabled := True;

    Stopwatch.Start;
    while Stopwatch.Elapsed.SecondsAsF < 1 do
    begin
      Application.ProcessMessages;
      Sleep(10);
      DIdleTimer.ExecuteTimers;
    end;
    Stopwatch.Stop;
    DTimer.Enabled := False;

    CheckEquals(20, FEventCount);
  finally
    Stopwatch.Free;
    DTimer.Free;
  end;
end;

procedure TDTimerTest.OnTimer2(Sender: TObject);
begin
  Inc(FEventCount2);
end;

procedure TDTimerTest.TestFrequency;
var
  DTimer: TDTimer;
  Stopwatch: TStopwatch;
begin
  FEventCount := 0;
  DTimer := TDTimer.Create(nil);
  Stopwatch := TStopwatch.Create;
  try
    DTimer.EventStep := esFrequency;
    DTimer.Interval := 20; // Hz
    DTimer.OnTimer := OnTimer;
    DTimer.Enabled := True;

    Stopwatch.Start;
    while Stopwatch.Elapsed.SecondsAsF < 1 do
    begin
      Application.ProcessMessages;
      Sleep(10);
      DIdleTimer.ExecuteTimers;
    end;
    Stopwatch.Stop;
    DTimer.Enabled := False;

    CheckEquals(19, FEventCount);
  finally
    Stopwatch.Free;
    DTimer.Free;
  end;
end;

procedure TDTimerTest.TestIntervalInSecond;
var
  DTimer: TDTimer;
  Stopwatch: TStopwatch;
begin
  FEventCount := 0;
  DTimer := TDTimer.Create(nil);
  Stopwatch := TStopwatch.Create;
  try
    DTimer.EventStep := esIntervalInSeconds;
    DTimer.IntervalInSeconds := 0.051; // s
    DTimer.OnTimer := OnTimer;
    DTimer.Enabled := True;

    Stopwatch.Start;
    while Stopwatch.Elapsed.SecondsAsF < 1 do
    begin
      Application.ProcessMessages;
      Sleep(10);
      DIdleTimer.ExecuteTimers;
    end;
    Stopwatch.Stop;
    DTimer.Enabled := False;

    CheckEquals(20, FEventCount);
  finally
    Stopwatch.Free;
    DTimer.Free;
  end;
end;

procedure TDTimerTest.TestTwoTimers;
var
  DTimer1, DTimer2: TDTimer;
  Stopwatch: TStopwatch;
begin
  FEventCount := 0;
  DTimer1 := TDTimer.Create(nil);
  DTimer2 := TDTimer.Create(nil);
  Stopwatch := TStopwatch.Create;
  try
    DTimer1.EventStep := esInterval;
    DTimer1.Interval := 51; // ms
    DTimer1.OnTimer := OnTimer;
    DTimer1.Enabled := True;

    DTimer2.EventStep := esInterval;
    DTimer2.Interval := 51; // ms
    DTimer2.OnTimer := OnTimer2;
    DTimer2.Enabled := True;

    Stopwatch.Start;
    while Stopwatch.Elapsed.SecondsAsF < 1 do
    begin
      Application.ProcessMessages;
      Sleep(10);
      DIdleTimer.ExecuteTimers;
    end;
    Stopwatch.Stop;
    DTimer1.Enabled := False;
    DTimer2.Enabled := False;

    CheckEquals(20, FEventCount);
    CheckEquals(20, FEventCount2);
  finally
    Stopwatch.Free;
    DTimer2.Free;
    DTimer1.Free;
  end;
end;

initialization
	RegisterTest('DTimer Test', TDTimerTest.Suite);
end.
