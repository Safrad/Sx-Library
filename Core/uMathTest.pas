unit uMathTest;

interface

uses
  uTypes,
  SysUtils,
  TestFrameWork;

type
  TMathTest = class(TTestCase)
  private
    procedure DelayAndPresiceSleepTest(const APreciseSleep: BG);
  published
    procedure SgnTest;
    procedure AbsMinTest;
    procedure UnsignedDivMod10Test;
    procedure UnsignedModTest;
    procedure FastSqrtTest;
    procedure RoundNTest;
    procedure BitsToByteTest;
    procedure NopTest;
    procedure PauseTest;
    procedure DelayTest;
    procedure PreciseSleepTest;
  end;

implementation

uses
  uMath;

procedure TMathTest.SgnTest;
begin
  CheckEquals(-1, Sgn(-10.7));
  CheckEquals(0, Sgn(0));
  CheckEquals(+1, Sgn(10.7));
end;

procedure TMathTest.AbsMinTest;
begin
  CheckEquals(7, AbsMin(-10, 7));
  CheckEquals(-10, AbsMin(-10, 17));
  CheckEquals(0, AbsMin(0, 0));
end;

procedure TMathTest.UnsignedDivMod10Test;
var
  Result, Reminder: U4;
begin
  UnsignedDivMod10(77, Result, Reminder);
  CheckEquals(7, Result);
  CheckEquals(7, Reminder);

  UnsignedDivMod10(2561524169, Result, Reminder);
  CheckEquals(256152416, Result);
  CheckEquals(9, Reminder);
end;

procedure TMathTest.UnsignedModTest;
begin
  CheckEquals(5, UnsignedMod(77, 8));
  CheckEquals(207806648, UnsignedMod(2561524169, 261524169));
end;

procedure TMathTest.FastSqrtTest;
begin
  CheckEquals(0, FastSqrt(0));
  CheckEquals(1, FastSqrt(1));
  CheckEquals(4, FastSqrt(16));
  CheckEquals(68, FastSqrt(4745));
  CheckEquals(68, FastSqrt(4760));
  CheckEquals(69, FastSqrt(4761));
  CheckEquals(46340, FastSqrt(MaxInt));
  CheckEquals(65535, FastSqrt(High(U4)));
end;

procedure TMathTest.RoundNTest;
begin
  CheckEquals(0, RoundN(0));
  CheckEquals(4, RoundN(3.5));
  CheckEquals(3, RoundN(3.1));
  CheckEquals(-4, RoundN(-3.5));
  CheckEquals(-3, RoundN(-3.1));
end;

procedure TMathTest.BitsToByteTest;
begin
  CheckEquals(0, BitsToByte(0));
  CheckEquals(1, BitsToByte(1));
  CheckEquals(1, BitsToByte(7));
  CheckEquals(1, BitsToByte(8));
  CheckEquals(2, BitsToByte(9));
  CheckEquals(1186, BitsToByte(9485));
end;

procedure TMathTest.NopTest;
begin
  Nop;
  Nop;
  Nop;
  Nop;
end;

procedure TMathTest.PauseTest;
begin
  Pause;
  Pause;
  Pause;
  Pause;
end;

const
  TestTimeInMs: array[0..8] of SG = (0, 1, 9, 11, 15, 25, 50, 333, 1000);

procedure TMathTest.DelayAndPresiceSleepTest(const APreciseSleep: BG);
var
  TimeInMs: SG;
  i: SG;
  Tick: U8;
  Dif: FG;
  MeasuredTime: FG;
begin
  for i := Low(TestTimeInMs) to High(TestTimeInMs) do
  begin
    TimeInMs := TestTimeInMs[i];
    Tick := PerformanceCounter;
    if APreciseSleep then
      PreciseSleep(TimeInMs)
    else
      Delay(TimeInMs);
    Tick := PerformanceCounter - Tick;
    MeasuredTime := 1000 * Tick / PerformanceFrequency;
    Dif := MeasuredTime - TimeInMs;
    // 1 ms tolerance
    Check(Abs(Dif) <= 0.1, 'Out of time tolerance ' + IntToStr(TimeInMs) + ' -> ' + FloatToStr(MeasuredTime));
  end;
end;

procedure TMathTest.DelayTest;
begin
  DelayAndPresiceSleepTest(False);
end;

procedure TMathTest.PreciseSleepTest;
begin
  DelayAndPresiceSleepTest(True);
end;

initialization
	RegisterTest('Math Test', TMathTest.Suite);
end.
