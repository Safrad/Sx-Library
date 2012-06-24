program LibTest;

{$ifopt d-}
{$APPTYPE CONSOLE}
{$define Console}
{$endif}

uses
	uFirst,
  uTypes,
  Forms,
  TestFrameWork,
  TextTestRunner,
  GUITestRunner,
  uEscapeTest in 'uEscapeTest.pas',
  uCharsetTest in 'uCharsetTest.pas',
  uDelphiTest in 'uDelphiTest.pas',
  uFileTest in 'uFileTest.pas',
  uGeometryTest in 'uGeometryTest.pas',
  uXYPolygonTest in 'uXYPolygonTest.pas',
  uStringsTest in 'uStringsTest.pas',
  uCompareTest in 'uCompareTest.pas',
  uFindTest in 'uFindTest.pas',
  uOutputFormatTest in 'uOutputFormatTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Lib Test';
  {$ifdef Console}
  TextTestRunner.RunRegisteredTests;
  {$else}
  GUITestRunner.RunRegisteredTests
  {$endif}
end.

