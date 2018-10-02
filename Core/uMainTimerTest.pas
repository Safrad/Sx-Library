unit uMainTimerTest;

interface

uses TestFrameWork;

type
  TMainTimerTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  Windows,
  uTypes,
  uMainTimer;

{ TMainTimerTest }

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
