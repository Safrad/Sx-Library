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
  Windows,
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
    CheckTrue(EqualRelative(Stopwatch.Elapsed.Milliseconds, SleepTime1 + SleepTime3, 0.1));
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
    CheckTrue(EqualRelative(Stopwatch.Elapsed.Milliseconds, SleepTime, 0.1));
  finally
    Stopwatch.Free;
  end;
end;

initialization
	RegisterTest('Stopwatch Test', TStopwatchTest.Suite);
end.
