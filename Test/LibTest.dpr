program LibTest;

{$ifopt d-}
{$APPTYPE CONSOLE}
{$define Console}
{$endif}

uses
  FastMM4,
  uFirst,
  uTypes,
  Forms,
  TestFramework,
  TextTestRunner,
  GUITestRunner,
  uEscapeTest in '..\Core\uEscapeTest.pas',
  uCharsetTest in '..\Core\uCharsetTest.pas',
  uDelphiTest in '..\Core\uDelphiTest.pas',
  uFileTest in '..\Core\uFileTest.pas',
  uFilesTest in '..\Core\uFilesTest.pas',
  uGeometry2DTest in '..\Geometry\uGeometry2DTest.pas',
  uPolygon2DTest in '..\Geometry\uPolygon2DTest.pas',
  uStringsTest in '..\Core\uStringsTest.pas',
  uCompareTest in '..\Core\uCompareTest.pas',
  uFindTest in '..\Core\uFindTest.pas',
  uOutputFormatTest in '..\Core\uOutputFormatTest.pas',
  uBackupTest in '..\Core\uBackupTest.pas',
  uObjectFactoryTest in '..\Core\uObjectFactoryTest.pas',
  uPictureFactoryTest in '..\GUI\uPictureFactoryTest.pas',
  uWebUpdateTest in '..\GUI\uWebUpdateTest.pas',
  uProjectOptionsTest in '..\Core\uProjectOptionsTest.pas',
  uThreadPoolTest in '..\Core\uThreadPoolTest.pas',
  uDivideSpaceTest in '..\Core\uDivideSpaceTest.pas',
  uCSVFileTest in '..\Core\uCSVFileTest.pas',
  uRegionalTest in '..\Core\uRegionalTest.pas',
  uMathTest in '..\Core\uMathTest.pas',
  uMidiPlayerTest in '..\Core\uMidiPlayerTest.pas',
  uCPUTest in '..\Core\uCPUTest.pas',
  uDParserTest in '..\Parser\uDParserTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Lib Test';
  if IsConsole then
	TextTestRunner.RunRegisteredTests
  else
	GUITestRunner.RunRegisteredTests;
end.

