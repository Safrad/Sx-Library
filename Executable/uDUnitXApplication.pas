unit uDUnitXApplication;

interface

uses
  uCommonApplication;

type
  /// <summary>
  /// DUnitX test application template
  /// </summary>
  /// <remarks>
  /// Example of use:<para/>
  ///
  /// MyProgramTests.dpr:
  ///
  /// <code>
  /// program DUnitXTests;<para/>
  ///  <para/>
  /// {$IFNDEF TESTINSIGHT}<para/>
  /// {$APPTYPE CONSOLE}<para/>
  /// {$ENDIF}<para/>
  ///  <para/>
  /// {$STRONGLINKTYPES ON}<para/>
  ///  <para/>
  /// uses<para/>
  ///   uDUnitXApplication,<para/>
  ///   uMainTest;<para/>
  ///  <para/>
  /// {$R *.RES}<para/>
  ///  <para/>
  /// var<para/>
  ///   DUnitXApplication: TDUnitXApplication;<para/>
  /// begin<para/>
  ///   DUnitXApplication := TDUnitXApplication.Create;<para/>
  ///   try<para/>
  ///     DUnitXApplication.Run;<para/>
  ///   finally<para/>
  ///     DUnitXApplication.Free;<para/>
  ///   end;<para/>
  /// end.<para/>
  /// </code>
  /// </remarks>
  TDUnitXApplication = class(TCommonApplication)
  protected
    procedure OnRun; override;
  end;

implementation

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework;

{ TDUnitXApplication }

procedure TDUnitXApplication.OnRun;
var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  inherited;

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end;

end.
