program LibTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  uEscapeTest in 'uEscapeTest.pas',
  uCharsetTest in 'uCharsetTest.pas',
  uDelphiTest in 'uDelphiTest.pas',
  uFileTest in 'uFileTest.pas',
  uGeometryTest in 'uGeometryTest.pas',
  uXYPolygonTest in 'uXYPolygonTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Lib Test';
  GUITestRunner.RunRegisteredTests;
end.

