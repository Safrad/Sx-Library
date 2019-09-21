program LibTest;

{$ifopt d-}
{$APPTYPE CONSOLE}
{$endif}

uses
  uTestsApplication,
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
  uCPUTest in '..\Core\uCPUTest.pas',
  uMathExpressionParserTest in '..\Parser\uMathExpressionParserTest.pas',
  uArgumentsForTest in '..\Arguments\uArgumentsForTest.pas',
  uArgumentsTest in '..\Arguments\uArgumentsTest.pas',
  uLoadDllTest in 'DllForTest\uLoadDllTest.pas',
  uPermutationTest in '..\Core\uPermutationTest.pas',
  uPermutationList in '..\Core\uPermutationList.pas',
  uStopwatchTest in '..\Core\uStopwatchTest.pas',
  uWavePlayerTest in '..\Sound\Players\uWavePlayerTest.pas',
  uTicksTest in '..\Core\uTicksTest.pas',
  uColorSequenceTest in '..\GUI\uColorSequenceTest.pas',
  uRandomGeneratorTest in '..\RandomGenerator\uRandomGeneratorTest.pas',
  uMainTimerTest in '..\Core\uMainTimerTest.pas',
  uDTimerTest in '..\GUI\uDTimerTest.pas',
  uSxThreadTimerTest in '..\Core\uSxThreadTimerTest.pas',
  uTextMacroTest in '..\Core\uTextMacroTest.pas',
  uStartupEnvironmentTest in '..\Core\uStartupEnvironmentTest.pas',
  uCyclicRedundancyCheckTest in '..\Core\uCyclicRedundancyCheckTest.pas',
  uSplitFileTest in '..\Core\uSplitFileTest.pas',
  uRegionalCodePageTest in '..\Regional\uRegionalCodePageTest.pas',
  uWaveTest in '..\Sound\SoundItems\uWaveTest.pas',
  uTimeExpressionParserTest in '..\Parser\uTimeExpressionParserTest.pas',
  uSxObjectListTest in '..\Core\uSxObjectListTest.pas',
  uSxStringListText in '..\Core\uSxStringListText.pas';

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
