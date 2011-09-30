program LibTest;
uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  uEscapeTest in 'uEscapeTest.pas',
  uCharsetTest in 'uCharsetTest.pas',
  uDelphiTest in 'uDelphiTest.pas',
  uFileTest in 'uFileTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

