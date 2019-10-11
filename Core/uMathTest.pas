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
    procedure FactorialTest;
    procedure FastSqrtTest;
    procedure RoundNTest;
    procedure BitsToByteTest;
    procedure NopTest;
    procedure PauseTest;
    procedure DelayTest;
    procedure PreciseSleepTest;
    procedure TimeDifferenceTest;
    procedure MultiplyTest;
    procedure MultiplyAndReturnMostSignificantHalfTest;
    procedure BitScanReverseU4Test;
    procedure BitScanReverseU8Test;
    procedure CountDigitsU4Test;
    procedure CountDigitsU8Test;
  end;

implementation

uses
  Math,
  GammaF,

  uTimeSpan,
  uMainTimer,
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

procedure TMathTest.MultiplyAndReturnMostSignificantHalfTest;
var
  ValueA, ValueB: U8;
  Result: U8;
begin
  ValueA := 50000000000;
  ValueB := 60000000000;
  Result := MultiplyAndReturnMostSignificantHalf(ValueA, ValueB);
  CheckEquals(162, Result);

  ValueA := $BA43B7400;
  ValueB := $DF8475800;
  Result := MultiplyAndReturnMostSignificantHalf(ValueA, ValueB);
  CheckEquals(162, Result);

  ValueA := 5000000000;
  ValueB := 6000000000;
  Result := MultiplyAndReturnMostSignificantHalf(ValueA, ValueB);
  CheckEquals(1, Result);

  ValueA := $200000000;
  ValueB := $300000000;
  Result := MultiplyAndReturnMostSignificantHalf(ValueA, ValueB);
  CheckEquals($6, Result);

  ValueA := $2000000000;
  ValueB := $3000000000;
  Result := MultiplyAndReturnMostSignificantHalf(ValueA, ValueB);
  CheckEquals($600, Result);
end;

procedure TMathTest.MultiplyTest;
var
  ValueA, ValueB: U8;
  HighResult, LowResult: U8;
begin
  ValueA := 97448;
  ValueB := 41548;
  Multiply(ValueA, ValueB, HighResult, LowResult);
  CheckEquals(ValueA * ValueB, LowResult);
  CheckEquals(0, HighResult);

  ValueA := $200000000;
  ValueB := $300000000;
  Multiply(ValueA, ValueB, HighResult, LowResult);
  CheckEquals(0, LowResult);
  CheckEquals($6, HighResult);
end;

procedure TMathTest.RoundNTest;
begin
  CheckEquals(0, RoundN(0));
  CheckEquals(4, RoundN(3.5));
  CheckEquals(3, RoundN(3.1));
  CheckEquals(-4, RoundN(-3.5));
  CheckEquals(-3, RoundN(-3.1));
end;

procedure TMathTest.BitScanReverseU4Test;
begin
  CheckEquals(0, BitScanReverse(0));
  CheckEquals(0, BitScanReverse(1));
  CheckEquals(1, BitScanReverse(2));
  CheckEquals(1, BitScanReverse(3));
  CheckEquals(2, BitScanReverse(4));
  CheckEquals(2, BitScanReverse(5));
  CheckEquals(2, BitScanReverse(6));
  CheckEquals(2, BitScanReverse(7));
  CheckEquals(3, BitScanReverse(8));
  CheckEquals(3, BitScanReverse(9));
  CheckEquals(3, BitScanReverse(15));
  CheckEquals(4, BitScanReverse(16));
  CheckEquals(31, BitScanReverse($FFFFFFFF));
end;

procedure TMathTest.BitScanReverseU8Test;
begin
  CheckEquals(0, BitScanReverse(U8(0)));
  CheckEquals(0, BitScanReverse(U8(1)));
  CheckEquals(1, BitScanReverse(U8(2)));
  CheckEquals(1, BitScanReverse(U8(3)));
  CheckEquals(2, BitScanReverse(U8(4)));
  CheckEquals(2, BitScanReverse(U8(5)));
  CheckEquals(2, BitScanReverse(U8(6)));
  CheckEquals(2, BitScanReverse(U8(7)));
  CheckEquals(3, BitScanReverse(U8(8)));
  CheckEquals(3, BitScanReverse(U8(9)));
  CheckEquals(3, BitScanReverse(U8(15)));
  CheckEquals(4, BitScanReverse(U8(16)));
  CheckEquals(31, BitScanReverse(U8($FFFFFFFF)));
  CheckEquals(63, BitScanReverse(U8($FFFFFFFFFFFFFFFF)));
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

procedure TMathTest.CountDigitsU4Test;
var
  Digits: UG;
  Value: U4;
begin
  Value := 1;
  Digits := 1;
  repeat
    CheckEquals(Max(1, Digits - 1), CountDigits(Value - 1));
    CheckEquals(Digits, CountDigits(Value));
    Value := Value * 10;
    Inc(Digits);
  until Value > High(Value) div 10;
  CheckEquals(10, CountDigits($FFFFFFFF));
end;

procedure TMathTest.CountDigitsU8Test;
var
  Digits: UG;
  Value: U8;
begin
  Value := 1;
  Digits := 1;
  repeat
    CheckEquals(Max(1, Digits - 1), CountDigits(Value - 1));
    CheckEquals(Digits, CountDigits(Value));
    Value := Value * 10;
    Inc(Digits);
  until Value > High(Value) div 10;
  CheckEquals(10, CountDigits(U8($FFFFFFFF)));
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
  TestTimeInMs: array[0..8] of UG = (0, 1, 9, 11, 15, 25, 50, 333, 1000);

procedure TMathTest.DelayAndPresiceSleepTest(const APreciseSleep: BG);
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
      PreciseSleep(TestTime)
    else
      Delay(TestTime);
    MeasuredTime := MainTimer.IntervalFrom(StartTime);
    Dif := MeasuredTime.Milliseconds - TestTimeInMs[i];
    // 1 ms tolerance
    Check(Abs(Dif) <= 0.1, 'Out of time tolerance [ms]' + IntToStr(TestTimeInMs[i]) + ' -> ' + IntToStr(MeasuredTime.Milliseconds));
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

procedure TMathTest.TimeDifferenceTest;
begin
  CheckEquals(20, TimeDifference(100, 80));
  CheckEquals(10, TimeDifference(9, High(U4)));
  CheckEquals(100, TimeDifference(99, $FFFFFFFFFFFFFFFF));
end;

procedure TMathTest.FactorialTest;
{$ifndef CPUX64}
const
  Factorial99: FA =  9.3326215443944152681E155; // 20 digits
  Factorial999: FA = 4.0238726007709377354E2564; // 20 digits
{$endif}
begin
  CheckEquals(1, Factorial(0));
  CheckEquals(1, Factorial(1));
  CheckEquals(2, Factorial(2));
  CheckEquals(6, Factorial(3));
  CheckEquals(24, Factorial(4));

  CheckEquals(1, Gamma(1));
  CheckEquals(1, Gamma(2));
  CheckEquals(2, Gamma(3));
  CheckEquals(6, Gamma(4));
  CheckEquals(24, Gamma(5));

  {$ifndef CPUX64}
  CheckTrue(EqualRelative(Gamma(100), Factorial99, 1e-17));
  CheckTrue(EqualRelative(Gamma(1000), Factorial999, 1e-15));
  {$endif}
end;

initialization
	RegisterTest('Math Test', TMathTest.Suite);
end.
