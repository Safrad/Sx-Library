program DUnitTests;

{$ifopt d-}
{$APPTYPE CONSOLE}
{$endif}

uses
  uDUnitApplication,
  uEscapeTest in '..\Core\uEscapeTest.pas',
  uCharsetTest in '..\Core\uCharsetTest.pas',
  uStringsTest in '..\Core\uStringsTest.pas',
  uCompareTest in '..\Core\uCompareTest.pas',
  uFindTest in '..\Core\uFindTest.pas',
  uOutputFormatTest in '..\Core\uOutputFormatTest.pas',
  uBackupTest in '..\Core\uBackupTest.pas',
  uObjectFactoryTest in '..\Core\uObjectFactoryTest.pas',
  uDivideSpaceTest in '..\Core\uDivideSpaceTest.pas',
  uMathTest in '..\Core\uMathTest.pas',
  uPermutationTest in '..\Core\uPermutationTest.pas',
  uStopwatchTest in '..\Core\uStopwatchTest.pas',
  uTicksTest in '..\Core\uTicksTest.pas',
  uMainTimerTest in '..\Core\uMainTimerTest.pas',
  uTextMacroTest in '..\Core\uTextMacroTest.pas',
  uStartupEnvironmentTest in '..\Core\uStartupEnvironmentTest.pas',
  uCyclicRedundancyCheckTest in '..\Core\uCyclicRedundancyCheckTest.pas',
  uIntegerListTest in '..\Core\uIntegerListTest.pas',
  uSxObjectListTest in '..\Core\uSxObjectListTest.pas',
  uSxStringListTest in '..\Core\uSxStringListTest.pas',

  uRandomGeneratorTest in '..\RandomGenerator\uRandomGeneratorTest.pas',

  uArgumentsForTest in '..\Arguments\uArgumentsForTest.pas',
  uArgumentsTest in '..\Arguments\uArgumentsTest.pas',

  uGeometry2DTest in '..\Geometry\uGeometry2DTest.pas',
  uPolygon2DTest in '..\Geometry\uPolygon2DTest.pas',

  uFileTest in '..\Files\uFileTest.pas',
  uFilesTest in '..\Files\uFilesTest.pas',
  uSplitFileTest in '..\Files\uSplitFileTest.pas',
  uRawFileTest in '..\Files\uRawFileTest.pas',
  uTextFileTest in '..\Files\uTextFileTest.pas',
  uCSVFileTest in '..\Files\Database\uCSVFileTest.pas',
  uDBFTest in '..\Files\Database\uDBFTest.pas',

  uMathExpressionParserTest in '..\Parser\uMathExpressionParserTest.pas',
  uTimeExpressionParserTest in '..\Parser\uTimeExpressionParserTest.pas',

  uPercentFormatterTest in '..\Formatter\uPercentFormatterTest.pas',

  uSxThreadTimerTest in '..\ThreadPool\uSxThreadTimerTest.pas',
  uThreadPoolTest in '..\ThraedPool\uThreadPoolTest.pas',

  uRegionalTest in '..\Regional\uRegionalTest.pas',
  uRegionalCodePageTest in '..\Regional\uRegionalCodePageTest.pas',

  uExternalApplicationTest in '..\ExternalApplication\uExternalApplicationTest.pas',
  uPipedExternalApplicationTest in '..\ExternalApplication\uPipedExternalApplicationTest.pas',

  uCPUTest in '..\Hardware\uCPUTest.pas',

  uWavePlayerTest in '..\Sound\Players\uWavePlayerTest.pas',
  uWaveTest in '..\Sound\SoundItems\uWaveTest.pas',

  uGraphicObjectsRendererTest in '..\Renderer\uGraphicObjectsRendererTest.pas',

  uSVGReaderTest in '..\SVG\uSVGReaderTest.pas',

  uPictureFactoryTest in '..\GUI\uPictureFactoryTest.pas',
  uWebUpdateTest in '..\GUI\uWebUpdateTest.pas',
  uColorSequenceTest in '..\GUI\uColorSequenceTest.pas',
  uDBitmapTest in '..\GUI\uDBitmapTest.pas',
  uDesktopWindowTest in '..\GUI\uDesktopWindowTest.pas',
  uTaskbarWindowTest in '..\GUI\uTaskbarWindowTest.pas',
  uScreenTest in '..\GUI\uScreenTest.pas',

  uDTimerTest in '..\GUI\VCL\Components\uDTimerTest.pas',

  uDelphiTest in '..\Delphi IDE\uDelphiTest.pas',
  uProjectOptionsTest in '..\Delphi IDE\uProjectOptionsTest.pas',

  uLoadDllTest in 'DllForTest\uLoadDllTest.pas';

{$R *.RES}

var
  TestsApplication: TDUnitApplication;
begin
  TestsApplication := TDUnitApplication.Create;
  try
    TestsApplication.Run;
  finally
    TestsApplication.Free;
  end;
end.
