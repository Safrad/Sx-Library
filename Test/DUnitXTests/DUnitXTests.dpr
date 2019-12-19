program DUnitXTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  uDUnitXApplication,
  Unit1 in 'Unit1.pas';

var
  TestsApplication: TDUnitXApplication;
begin
  TestsApplication := TDUnitXApplication.Create;
  try
    TestsApplication.Run;
  finally
    TestsApplication.Free;
  end;
end.
