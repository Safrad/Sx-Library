unit uSxThreadTimerTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TSxThreadTimerTest = class(TTestCase)
  private
    FEventCount: UG;
    FEventCount2: UG;
    procedure OnTimer(Sender: TObject);
    procedure OnTimer2(Sender: TObject);
  published
    procedure TestOneTimer;
    procedure TestTwoTimers;
  end;

implementation

uses
  SysUtils,
  Forms,

  uStopwatch,
  uSxThreadTimer,
  uLog;

{ TSxThreadTimerTest }

procedure TSxThreadTimerTest.OnTimer(Sender: TObject);
begin
  Inc(FEventCount);

  if LogDebug then
    MainLog.Add('OnTimer', mlDebug);
end;

procedure TSxThreadTimerTest.TestOneTimer;
var
  DTimer: TSxThreadTimer;
  Stopwatch: TStopwatch;
begin
  FEventCount := 0;
  DTimer := TSxThreadTimer.Create;
  Stopwatch := TStopwatch.Create;
  try
    DTimer.Interval.Milliseconds := 51;
    DTimer.OnTimer := OnTimer;

    DTimer.Enabled := True;

    Stopwatch.Start;
    while Stopwatch.Elapsed.SecondsAsF < 1 do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
    Stopwatch.Stop;
    DTimer.Enabled := False;

    CheckEquals(20, FEventCount);
  finally
    Stopwatch.Free;
    DTimer.Free;
  end;
end;

procedure TSxThreadTimerTest.OnTimer2(Sender: TObject);
begin
  Inc(FEventCount2);
end;

procedure TSxThreadTimerTest.TestTwoTimers;
var
  DTimer1, DTimer2: TSxThreadTimer;
  Stopwatch: TStopwatch;
begin
  FEventCount := 0;
  FEventCount2 := 0;
  DTimer1 := TSxThreadTimer.Create;
  DTimer2 := TSxThreadTimer.Create;
  Stopwatch := TStopwatch.Create;
  try
    DTimer1.Interval.Milliseconds := 51;
    DTimer1.OnTimer := OnTimer;

    DTimer2.Interval.Milliseconds := 51;
    DTimer2.OnTimer := OnTimer2;

    DTimer1.Enabled := True;
    DTimer2.Enabled := True;

    Stopwatch.Start;
    while Stopwatch.Elapsed.SecondsAsF < 1 do
    begin
      Sleep(10);
    end;
    Stopwatch.Stop;
    DTimer1.Enabled := False;
    DTimer2.Enabled := False;

    CheckEquals(20, FEventCount, 'Timer1');
    CheckEquals(20, FEventCount2, 'Timer2');
  finally
    Stopwatch.Free;
    DTimer2.Free;
    DTimer1.Free;
  end;
end;

initialization
	RegisterTest('Sx Thread Timer Test', TSxThreadTimerTest.Suite);
end.
