unit uMainTimerTest;

interface

uses
  TestFrameWork,
  uTypes;

type
  TMainTimerTest = class(TTestCase)
  private
    procedure DelayAndPresiceSleepTest(const APreciseSleep: BG);
  published
    procedure Test;
    procedure DelayTest;
    procedure PreciseSleepTest;
  end;

implementation

uses
  SysUtils,

  uMainTimer,
  uTimeSpan;

{ TMainTimerTest }

const
  TestTimeInMs: array[0..8] of UG = (0, 1, 9, 11, 15, 25, 50, 333, 1000);

procedure TMainTimerTest.DelayAndPresiceSleepTest(const APreciseSleep: BG);
var
  TestTime: TTimeSpan;
  i: SG;
  StartTime: TTimeSpan;
  Dif: FG;
  MeasuredTime: TTimeSpan;
begin
  for i := Low(TestTimeInMs) to High(TestTimeInMs) do
  begin
    TestTime.Milliseconds := TestTimeInMs[i];
    StartTime := MainTimer.Value;
    if APreciseSleep then
      MainTimer.PreciseSleep(TestTime)
    else
      MainTimer.Delay(TestTime);
    MeasuredTime := MainTimer.IntervalFrom(StartTime);
    Dif := MeasuredTime.Milliseconds - TestTimeInMs[i];
    // 1 ms tolerance
    Check(Abs(Dif) <= 0.1, 'Out of time tolerance [ms] ' + IntToStr(TestTimeInMs[i]) + ' -> ' + IntToStr(MeasuredTime.Milliseconds));
  end;
end;

procedure TMainTimerTest.DelayTest;
begin
  DelayAndPresiceSleepTest(False);
end;

procedure TMainTimerTest.PreciseSleepTest;
begin
  DelayAndPresiceSleepTest(True);
end;

procedure TMainTimerTest.Test;
var
  MainTimer: TMainTimer;
  Value: U8;
begin
  MainTimer := TMainTimer.Create;
  try
    MainTimer.MeasureType := mtGetTickCount32;
    Value := MainTimer.Value.Ticks;
    CheckTrue(MainTimer.Frequency = 1000);
    CheckTrue(Value <> 0);
    CheckTrue(Value <= $FFFFFFFF);
    Sleep(50);
    CheckTrue(Value <> MainTimer.Value.Ticks);

    MainTimer.MeasureType := mtGetTickCount64;
    CheckTrue(MainTimer.Frequency = 1000);
    CheckTrue(Value <> 0);
    CheckTrue(Value <= $FFFFFFFF);
    Sleep(50);
    CheckTrue(Value <> MainTimer.Value.Ticks);

    MainTimer.MeasureType := mtPerformanceCounter;
    CheckTrue(MainTimer.Frequency > 1000000);
    CheckTrue(Value <> 0);
    Sleep(50);
    CheckTrue(Value <> MainTimer.Value.Ticks);

    MainTimer.MeasureType := mtTSC;
    CheckTrue(MainTimer.Frequency = 0);
    CheckTrue(Value <> 0);
    Sleep(50);
    CheckTrue(Value <> MainTimer.Value.Ticks);
  finally
    MainTimer.Free;
  end;
end;

initialization
	RegisterTest('Main Timer', TMainTimerTest.Suite);
end.
