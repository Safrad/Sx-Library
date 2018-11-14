(*
Example of use:

MyProgramTests.dpr:

program MyProgramTests;

{$ifopt d-}
{$APPTYPE CONSOLE}
{$endif}

uses
  uTestsApplication,
  uMainTest;

{$R *.RES}

var
  TestsApplication: TTestsApplication;
begin
  TestsApplication := TTestsApplication.Create;
  try
    TestsApplication.Run;
  finally
    TestsApplication.Free;
  end;
end.

*)

unit uTestsApplication;

interface

uses
  uCommonApplication;

type
  TTestsApplication = class(TCommonApplication)
  protected
    procedure OnRun; override;
  end;

implementation

uses
  TextTestRunner,
  GUITestRunner;

{ TTestsApplication }

procedure TTestsApplication.OnRun;
begin
  inherited;

  if IsConsole then
  	TextTestRunner.RunRegisteredTests
  else
  	GUITestRunner.RunRegisteredTests;
end;

end.
