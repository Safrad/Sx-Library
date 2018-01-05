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
  Windows,
  uCPU;

var
  FirstCall: BG = True;
  
procedure TCPUTest.Test;
begin
  GCPU.Update;

  CheckTrue(GCPU.Name <> '');
  CheckTrue(GCPU.Family <> 0);
  CheckTrue(GCPU.Model <> 0);
  CheckTrue(GCPU.Stepping <> 0);
  CheckTrue(GCPU.Frequency <> 0);
  CheckTrue(GCPU.DefaultFrequency <> 0);
  CheckTrue(Abs(GCPU.Frequency / GCPU.DefaultFrequency - 1) < 1);
  if FirstCall then
    CheckTrue(GCPU.Usage = 0)
  else
    CheckTrue(GCPU.Usage <> 0);

  FirstCall := False;
  Sleep(100);
  GCPU.Update;
  CheckTrue(GCPU.Usage <> 0);
end;

initialization
	RegisterTest('CPU Test', TCPUTest.Suite);
end.
