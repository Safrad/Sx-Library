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
  uGeometryTest in '..\Core\uGeometryTest.pas',
  uXYPolygonTest in '..\Core\uXYPolygonTest.pas',
  uStringsTest in '..\Core\uStringsTest.pas',
  uCompareTest in '..\Core\uCompareTest.pas',
  uFindTest in '..\Core\uFindTest.pas',
  uOutputFormatTest in '..\Core\uOutputFormatTest.pas',
  uBackupTest in '..\Core\uBackupTest.pas',
  uObjectFactoryTest in '..\Core\uObjectFactoryTest.pas',
  uPictureFactoryTest in '..\GUI\uPictureFactoryTest.pas',
  uWebUpdateTest in '..\GUI\uWebUpdateTest.pas',
  uProjectOptionsTest in '..\Core\uProjectOptionsTest.pas',
  uDivideSpaceTest in '..\Core\uDivideSpaceTest.pas';

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

