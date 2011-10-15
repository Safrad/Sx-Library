{ $Id: UnitTestExtensions.pas 7 2008-04-24 11:59:47Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 7 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kenneth Semeijn <kennethsem@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

unit UnitTestExtensions;

interface

uses
  TestFramework,
  TestExtensions;

const
  COUNT_MAX = 5;

type
  ITestStub = interface(ITest)
    function GetCounter: integer;
  end;

  TTestStub = class(TTestCase, ITestStub)
  protected
    FCounter: integer;
  public
    function GetCounter: integer;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure test;
  end;

  TTestSetupStub = class(TTestSetup)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    SetUpCalled: boolean;
    TearDownCalled: boolean;
  end;

  TTestStubTest = class(TTestCase)
  private
    FTestResult: TTestResult;
    FTestStub: ITestStub;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestSetupTest = class(TTestStubTest)
  private
    FSetupTest: TTestSetupStub;
    FTest:      ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSetupTest;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDecoratedEnabling;
  end;

  TTestSetupStubExceptionInSetup = class(TTestSetupStub)
  protected
    procedure SetUp; override;
  end;

  TTestTestSetupExceptionInSetup = class(TTestStubTest)
  private
    FSetupTest: TTestSetupStub;
    FTest:      ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testNoExecutionOfTests;
  end;

  TTestSetupStubExceptionInTearDown = class(TTestSetupStub)
  protected
    procedure TearDown; override;
  end;

  TTestTestSetupExceptionInTearDown = class(TTestStubTest)
  private
    FSetupTest: TTestSetupStub;
    FTest:      ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testExecutionOfTests;
  end;

  TTestRepeatedTest = class(TTestStubTest)
  private
    FIterations: integer;
    FRepTest: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testRepeatedTest;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testWithCounting;
    procedure testWithCountingHaltOnTestFailed;
  end;

  TCountCase = class(TTestCase)
  private
    FCounter : Integer;
    FTotal   : Integer;
    FLast    : Integer;
  protected
    procedure SetUp; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure CountTest; virtual;
  end;

  TCountCaseFails = class(TTestCase)
  private
    FCounter : Integer;
    FTotal   : Integer;
    FLast    : Integer;
  protected
    procedure SetUp; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure CountTestFails; virtual;
  end;

  { TMemoryTest tests }

{$IFNDEF CLR}
  TTestMemoryTest = class(TTestStubTest)
  protected
    FObject : TObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testEmptyTest; virtual;
  end;

  TTestMemoryTestWithCheckError = class(TTestMemoryTest)
  published
    procedure testEmptyTest; override;
  end;

  TTestMemoryTestNotFreeing = class(TTestStubTest)
  protected
    FObject : TObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testEmptyTest;
  end;

  TBaseMemoryTest = class(TTestStubTest)
  private
    FMemTest: ITest;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TMemoryTestNoLeak = class(TBaseMemoryTest)
  public
    procedure SetUp; override;
  published
    procedure testNoLeak;
  end;

  TMemoryTestLeak = class(TBaseMemoryTest)
  public
    procedure SetUp; override;
  published
    procedure testLeak;
  end;

  TMemoryTestNoLeakReportingWithCheckError = class(TBaseMemoryTest)
  public
    procedure SetUp; override;
  published
    procedure testNoLeakWithCheckError;
  end;
{$ENDIF}

implementation

uses
  {$IFNDEF CONSOLE}
  {$IFNDEF CLR}
    UnitTestGUITesting,
  {$ENDIF}
  {$ENDIF}
  UnitTestFramework,
  SysUtils;

{ TTestStub }

function TTestStub.GetCounter: integer;
begin
  Result := FCounter;
end;

procedure TTestStub.test;
begin
  check(true);
  Inc(FCounter);
end;

{ TTestSetupStub }

procedure TTestSetupStub.SetUp;
begin
  SetUpCalled := true;
end;

procedure TTestSetupStub.TearDown;
begin
  TearDownCalled := true;
end;

{ TTestStubTest }

procedure TTestStubTest.SetUp;
begin
  inherited;
  FTestStub := TTestStub.Create('test');
  FTestResult := TTestResult.Create;
end;

procedure TTestStubTest.TearDown;
begin
  FTestResult.Free;
  FTestStub := nil;
  inherited;
end;
            
{ TTestSetupTest }

procedure TTestSetupTest.SetUp;
begin
  inherited;
  FSetupTest := TTestSetupStub.Create(FTestStub);
  FTest := FSetupTest;
end;

procedure TTestSetupTest.TearDown;
begin
  inherited;
  FTest := nil;
  FSetupTest := nil;
end;

procedure TTestSetupTest.TestDecoratedEnabling;
var
  childEnabled :boolean;
begin
  childEnabled := FSetupTest.Test.Enabled;
  FSetupTest.Enabled := true;
  check(FSetupTest.Enabled);
  check(childEnabled = FSetupTest.Test.Enabled);
  FSetupTest.Enabled := false;
  check(not FSetupTest.Enabled);
  check(childEnabled = FSetupTest.Test.Enabled);
end;

procedure TTestSetupTest.TestSetupTest;
begin
  { call the interface to ensure proper inheritence in TTestSetup.
    To make this test fail, remove the override directive from
    TTestSetup.Run. }
  ITestDecorator(FSetupTest).Run(FTestResult);
  check(FTestResult.wasSuccessful);
  check(FSetupTest.SetUpCalled);
  check(FSetupTest.TearDownCalled);
end;

{ TTestRepeatedTest }

procedure TTestRepeatedTest.SetUp;
begin
  inherited;
  FIterations := COUNT_MAX;
  FRepTest := TRepeatedTest.Create(FTestStub, FIterations);
end;

procedure TTestRepeatedTest.TearDown;
begin
  FRepTest := nil;
  FRepTest := nil;
  inherited;
end;

procedure TTestRepeatedTest.testRepeatedTest;
begin
  check(FRepTest.CountTestCases = COUNT_MAX);
  check(FTestStub.getEnabled);
  FRepTest.Run(FTestResult);
  check(FTestResult.wasSuccessful);
  check(FTestStub.GetCounter = COUNT_MAX);
end;

procedure TTestRepeatedTest.testWithCounting;
var
  CountCase :ITest;
  AREsult   :TTestResult;
begin
  CountCase := TRepeatedTest.Create(TCountCase.Create('CountTest'), COUNT_MAX);
  AResult := CountCase.Run;
  try
    check(AResult.runCount     = COUNT_MAX, 'wrong runCount, was ' + IntToStr(AResult.runCount) );
    check(AResult.failureCount = 0, 'wrong failureCount, was ' + IntToStr(AResult.failureCount) );
    check(AResult.errorCount   = 0, 'wrong errorCount, was ' + IntToStr(AResult.errorCount) );
  finally
    AResult.Free
  end
end;

{ TCountCase }

procedure TCountCase.CountTest;
begin
   Inc(FCounter);
   check(FCounter = 1,  'must be one, or SetUp was not called');
   Inc(FTotal);
   check(FTotal   >= 1, 'total should be at least one');
   check(FTotal   = (FLast+1),  'total should be increment');
   FLast := FTotal;
end;

procedure TCountCase.SetUp;
begin
  FCounter := 0;
end;

procedure TTestRepeatedTest.testWithCountingHaltOnTestFailed;
var
  CountCase :IRepeatedTest;
  AResult   :TTestResult;
begin
  CountCase := TRepeatedTest.Create(TCountCaseFails.Create('CountTestFails'), COUNT_MAX);
  CountCase.HaltOnError := True;
  AResult := (CountCase as ITest).Run;
  try
    check(AResult.runCount     = 1, 'wrong runCount, was ' + IntToStr(AResult.runCount) );
    check(AResult.failureCount = 1, 'wrong failureCount, was ' + IntToStr(AResult.failureCount) );
    check(AResult.errorCount   = 0, 'wrong errorCount, was ' + IntToStr(AResult.errorCount) );
  finally
    AResult.Free
  end
end;

{ TCountCaseFails }

procedure TCountCaseFails.CountTestFails;
begin
   Inc(FCounter);
   check(FCounter = 1,  'must be one, or SetUp was not called');
   Inc(FTotal);
   check(FTotal   >= 1, 'total should be at least one');
   check(FTotal   = (FLast+1),  'total should be increment');
   FLast := FTotal;
   Check(False, 'Forced Fail');
end;

procedure TCountCaseFails.SetUp;
begin
  FCounter := 0;
end;

{ TTestMemoryTest }

{$IFNDEF CLR}
procedure TTestMemoryTest.SetUp;
begin
  inherited;
  FObject := TObject.Create;
end;

procedure TTestMemoryTest.TearDown;
begin
  FObject.Free;
  FObject := nil;
  // Now the memory leak should be 0

  inherited;
end;

procedure TTestMemoryTest.testEmptyTest;
begin
  CheckNotNull(FObject);
end;

{ TTestMemoryTestNotFreeing }

procedure TTestMemoryTestNotFreeing.SetUp;
begin
  inherited;

  FObject := TObject.Create;
end;

procedure TTestMemoryTestNotFreeing.TearDown;
begin
  inherited;

  // Don't free object
  // Now the memory leak shouldnot be 0

end;

procedure TTestMemoryTestNotFreeing.testEmptyTest;
begin
  CheckNotNull(FObject);
end;

{ TBaseMemoryTest }

procedure TBaseMemoryTest.SetUp;
begin
  inherited;
  FMemTest := nil
end;

procedure TBaseMemoryTest.TearDown;
begin
  FMemTest := nil;
  inherited;
end;

{ TMemoryTestNoLeak }

procedure TMemoryTestNoLeak.SetUp;
begin
  inherited;
  FMemTest := TMemoryTest.Create(TTestMemoryTest.Suite);
end;

procedure TMemoryTestNoLeak.testNoLeak;
begin
  check(FTestStub.getEnabled);
  FMemTest.Run(FTestResult);
  if not FTestResult.wasSuccessful then
  begin
    if FTestResult.ErrorCount > 0 then
      Fail('Memory tested testcase failed: '
              + FTestResult.Errors[0].ThrownExceptionMessage
              );
    if FTestResult.FailureCount > 0 then
      Fail('Memory tested testcase failed: '
              + FTestResult.Failures[0].ThrownExceptionMessage
              );
  end;
end;

{ TMemoryTestLeak }

procedure TMemoryTestLeak.SetUp;
begin
  inherited;

  FMemTest := TMemoryTest.Create(TTestMemoryTestNotFreeing.Suite);
end;

procedure TMemoryTestLeak.testLeak;
begin
    try
      check(FTestStub.getEnabled);
      FMemTest.Run(FTestResult);
    finally
      // Errors will happen on non Window platform
      // Where no GetHeapStatus.TotalAllocated function exists.
      check(not FTestResult.wasSuccessful, 'Test was successful, should not be successful.');
      CheckEquals(0, FTestResult.errorCount, 'Wrong errorCount');
      CheckEquals(1, FTestResult.failureCount, 'Wrong failureCount');
    end;
end;

{ TMemoryTestNoLeakReportingWithCheckError }

procedure TMemoryTestNoLeakReportingWithCheckError.SetUp;
begin
  inherited;
  FMemTest := TMemoryTest.Create(TTestMemoryTestWithCheckError.Suite);
end;

procedure TMemoryTestNoLeakReportingWithCheckError.testNoLeakWithCheckError;
begin
  check(FTestStub.getEnabled, 'not enabled');
  FMemTest.Run(FTestResult);
  check(FTestResult.FailureCount + FTestResult.ErrorCount = 1,
    'Memory test added 1 error or failure');
end;

{ TTestMemoryTestWithCheckError }

procedure TTestMemoryTestWithCheckError.testEmptyTest;
begin
  inherited;

  // Deliberate error
  Check(false);
end;
{$ENDIF}

{ TTestSetupExeptionInSetup }

procedure TTestSetupStubExceptionInSetup.SetUp;
begin
  inherited;

  raise EUnitTestException.Create('Exception in SetUp');
end;

{ TTestSetupExeptionInTearDown }

procedure TTestSetupStubExceptionInTearDown.TearDown;
begin
  inherited;

  raise EUnitTestException.Create('Exception in TearDown');
end;

{ TTestExceptionInSetup }

procedure TTestTestSetupExceptionInSetup.SetUp;
begin
  inherited;
  FSetupTest := TTestSetupStubExceptionInSetup.Create(FTestStub);
  FTest := FSetupTest;
end;

procedure TTestTestSetupExceptionInSetup.TearDown;
begin
  inherited;
  FTest := nil;
  FSetupTest := nil;
end;

procedure TTestTestSetupExceptionInSetup.testNoExecutionOfTests;
begin

  ITestDecorator(FSetupTest).Run(FTestResult);

  check(not FTestResult.WasSuccessful, 'Test was successful');
  CheckEquals(FTestResult.RunCount, 0, 'Tests ran with Exception in SetUp');
  check(FSetupTest.SetUpCalled, 'SetUp not called');
  check(FSetupTest.TearDownCalled, 'TearDown not called');
end;

{ TTestExceptionInTearDown }

procedure TTestTestSetupExceptionInTearDown.SetUp;
begin
  inherited;
  FSetupTest := TTestSetupStubExceptionInTearDown.Create(FTestStub);
  FTest := FSetupTest;
end;

procedure TTestTestSetupExceptionInTearDown.TearDown;
begin
  inherited;
  FTest := nil;
  FSetupTest := nil;
end;

procedure TTestTestSetupExceptionInTearDown.testExecutionOfTests;
begin

  ITestDecorator(FSetupTest).Run(FTestResult);

  check(not FTestResult.WasSuccessful, 'Test was successful');
  CheckEquals(FTestResult.RunCount, 1, 'Tests did not run');
  check(FSetupTest.SetUpCalled, 'SetUp not called');
  check(FSetupTest.TearDownCalled, 'TearDown not called');
end;

initialization
  RegisterTests('TestExtensions Suite',
                     [ TTestRepeatedTest.Suite,
                       TTestSetupTest.Suite,

                       TTestTestSetupExceptionInSetup.Suite,
                       TTestTestSetupExceptionInTearDown.Suite,

                       TRepeatedTest.Create(TCountCase.Create('CountTest'), COUNT_MAX)
                       ]);

  {$IFNDEF CLR}
  {$IFDEF CONSOLE}
  // These memory tests do not work with the GUI

  RegisterTests('TMemoryTest decorator test',
                     [
                       TMemoryTestNoLeak.Suite,
                       TMemoryTestLeak.Suite,
                       TMemoryTestNoLeakReportingWithCheckError.Suite,
                       TMemoryTest.Create(UnitTestFramework.TTestTest.Suite)
                       {$IFNDEF LINUX}
                         {$IFNDEF CONSOLE}
                           {$IFNDEF CLR}
                       , TMemoryTest.Create(UnitTestGUITesting.TGUITestRunnerTests.Suite)
                           {$ENDIF}
                         {$ENDIF}
                       {$ENDIF}
                      ]);
  {$ENDIF}
  {$ENDIF}
end.

