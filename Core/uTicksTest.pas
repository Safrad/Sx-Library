unit uTicksTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TTickTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uTicks;

{ TTickTest }

procedure TTickTest.Test;
var
  Ticks: TTicks;
begin
  Ticks := TTicks.Create;
  try
    Ticks.MinimumValue := 0;
    Ticks.MaximumValue := 240;

    Ticks.UpdateTicks(9);

    CheckEquals(10, Ticks.TickStep);
    CheckEquals(0, Ticks.MinimumTickValue);
    CheckEquals(240, Ticks.MaximumTickValue);

    Ticks.UpdateTicks(0.9);
    CheckEquals(1, Ticks.TickStep);

    Ticks.NoEdge := True;
    Ticks.UpdateTicks(9);
    CheckEquals(10, Ticks.TickStep);
    CheckEquals(-10, Ticks.MinimumTickValue);
    CheckEquals(250, Ticks.MaximumTickValue);

  finally
    Ticks.Free;
  end;
end;

initialization
	RegisterTest('Tick Test', TTickTest.Suite);
end.
