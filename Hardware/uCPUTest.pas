unit uCPUTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TCPUTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
  uCPU;

var
  FirstCall: BG = True;

procedure TCPUTest.Test;
begin
  CPU.Update;

  CheckTrue(CPU.Name <> '');
  CheckTrue(CPU.Family <> 0);
  CheckTrue(CPU.Model <> 0);
  CheckTrue(CPU.Stepping <> 0);
  CheckTrue(CPU.Frequency <> 0);
  CheckTrue(CPU.DefaultFrequency <> 0);
  CheckTrue(Abs(CPU.Frequency / CPU.DefaultFrequency - 1) < 1);
  if FirstCall then
    CheckTrue(CPU.Usage = 0)
  else
    CheckTrue(CPU.Usage <> 0);

  FirstCall := False;
  Sleep(1000);
  CPU.Update;
  CheckTrue(CPU.Usage <> 0);
end;

initialization
	RegisterTest('CPU Test', TCPUTest.Suite);
end.
