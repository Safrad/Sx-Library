{$APPTYPE CONSOLE}
program Project1Test;

uses
  TextTestRunner,
  TestFramework,
  Unit1Test,
  Unit2Test,
  SysUtils;

{$R *.res}

var
  ExitBehavior: TRunnerExitBehavior;
begin
  // sample test code will randomly pass or fail
  Randomize;

  WriteLn('To run with rxbPause, use -p switch');
  WriteLn('To run with rxbHaltOnFailures, use -h switch');
  WriteLn('No switch runs as rxbContinue');

  if FindCmdLineSwitch('p', ['-', '/'], true) then
    ExitBehavior := rxbPause
  else if FindCmdLineSwitch('h', ['-', '/'], true) then
    ExitBehavior := rxbHaltOnFailures
  else
    ExitBehavior := rxbContinue;

  TextTestRunner.RunRegisteredTests(ExitBehavior);
end.

