unit uStopwatchTest;

interface

uses TestFrameWork;

type
  TStopwatchTest = class(TTestCase)
  published
    procedure TestSimple;
    procedure TestRepeat;
  end;

implementation

uses
  SysUtils,
  uMath,
  uStopwatch;

{ TStopwatchTest }

procedure TStopwatchTest.TestRepeat;
const
  SleepTime1 = 100;
  SleepTime2 = 200;
  SleepTime3 = 50;
var
  Stopwatch:TStopwatch;
begin
  Stopwatch := TStopwatch.Create;
  try
    Stopwatch.Start;
    Sleep(SleepTime1);
    Stopwatch.Stop;
    Sleep(SleepTime2);
    Stopwatch.Start;
    Sleep(SleepTime3);
    Stopwatch.Stop;
    Check(EqualRelative(Stopwatch.Elapsed.Milliseconds, SleepTime1 + SleepTime3, 0.1), 'Out of time tolerance ' + IntToStr(SleepTime1 + SleepTime3) + ' -> ' + FloatToStr(Stopwatch.Elapsed.Milliseconds));
  finally
    Stopwatch.Free;
  end;
end;

procedure TStopwatchTest.TestSimple;
const
  SleepTime = 50;
var
  Stopwatch:TStopwatch;
begin
  Stopwatch := TStopwatch.Create;
  try
    Stopwatch.Start;
    Sleep(SleepTime);
    Stopwatch.Stop;
    Check(EqualRelative(Stopwatch.Elapsed.Milliseconds, SleepTime, 0.1), 'Out of time tolerance ' + IntToStr(SleepTime) + ' -> ' + FloatToStr(Stopwatch.Elapsed.Milliseconds));
  finally
    Stopwatch.Free;
  end;
end;

initialization
	RegisterTest('Stopwatch Test', TStopwatchTest.Suite);
end.
