{ Problem: TestRunAndTearDownFails has a leak of some sort. AV occurs outside
  of entire framework at the end of the test. }

{ $Id: UnitTestFramework.pas 23 2008-08-26 04:42:20Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 23 $
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
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
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


{$UNDEF DETECTMEMLEAKS}
{$IFDEF FASTMM}
  {$DEFINE DETECTMEMLEAKS} // Only check for memory leaks if FASTMM is specifically enabled
{$ENDIF}

unit UnitTestFramework;

interface

uses
  {$IFNDEF LINUX}
  Windows,
  {$ENDIF}
  SysUtils,
  TestFramework;

type
  TMonitoredTestCase = class(TTestCase)
  public
    destructor Destroy; override;
    procedure _AddRef;
    procedure _Release;
  end;

  TVerifierTestCase = class(TTestCase)
  protected
    procedure Verify(AResult: TTestResult;
      runCount, failCount, errCount: Integer);
    procedure VerifyError(ATest: ITest; errCount: Integer = 1);
    procedure VerifyFailure(ATest: ITest); virtual;
    procedure VerifySuccess(ATest: ITest); virtual;
    procedure VerifyLackOfSuccess(ATest: ITest);
  end;

  TVerifierTotalTestCase = class(TVerifierTestCase)
  protected
    procedure VerifySuccess(ATest: ITest); override;
    procedure VerifyFailure(ATest: ITest); override;
  end;

  { ported from JUnit tests, then refactored a bit }
  TTestTest = class(TVerifierTestCase)
  private
    fTestCount: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheck;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsBinHexMessage;
{$IFNDEF CLR} // don't expect Check(Not)EqualsMem to work under CLR -> pointers unsafe
    procedure TestCheckEqualsMem;
    procedure TestFirstByteDiff;
{$ENDIF}
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestError;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestFailure;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestRegisterTest;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestRunAndTearDownFails;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSetupException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSuccess;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTearDownAfterError;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTearDownFails;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTearDownSetupFails;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestWasNotSuccessful;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestWasSuccessful;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestWasStopped;
    procedure TestEmptyTestFailure;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBoolToStr;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestElapsedTestTime;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestFailOnDisAllowedUnCalledCheck;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPassOnAllowedUnCalledCheck;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestAlloweLeakListIterator;
  end;

  TTestMemLeakTestSuite = class(TVerifierTotalTestCase)
  private
    fTestCount: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestClassCanDetectFailure;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseDoesNotLeak;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseLeaksTObject;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseLeaksTObjectReportDisabled;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseLeaksTObjectLeakOfSizeAllowed;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseLeaksTObjectLeakOfListSizeAllowed;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseLeaksAllowedTObjectLeakOfEmptyList;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseFailsOnOverSizeAllowedLeakArrayList;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseNoLeaksAtSetupTearDownLevel;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseLeaksAtSetupTearDownLevel;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseIgnoresLeaksAtSetupTearDownLevel;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseDoesNotIgnoreLeaksInTestProc;
  end;

  TTestTestSuite = class(TVerifierTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSuiteSetupTearDown;
  end;

  TTestBasicTestCase4Leaks = class(TTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfTAbstractTestLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfTTestResultLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfTStatusListnerLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfTTestFalureLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfTTestCaseLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfRunTTestCaseLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfEnumeratorLeaks;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIfITestSuiteLeaks;
  end;

  { JUnit has no tests for this class }
  TTestTestResult = class(TTestCase)
  protected
    procedure SetUp; override;

  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestRun;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestElapsedTime;
  end;

  TTestMethodEnumerator = class(TTestCase)
  private
    FMethodEnumerator: TMethodEnumerator;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestMethodEnumerator;
  end;

  TTestExceptionChecks = class(TVerifierTestCase)
  protected
    procedure TestIndividualException(AName :string; FailCnt, ErrCnt :Integer);
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestExpectedException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestChecksAfterException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDifferentFromExpected;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestInheritsFromExpected;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckWrongException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNoExceptionRaised;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckAndNoExceptionRaised;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNotRaised;
  end;

  TTestStatus = class(TVerifierTestCase)
  published
    procedure testStatus;
  end;

  TStringyObject = class(TObject)
  public
    AString: string;
  end;

  {: Create our own exception class so users can choose
    to ignore it in the debugger }
  EUnitTestException = class(EAbort)
  end;

implementation

{$ifdef DETECTMEMLEAKS}
uses
  FastMMMonitorTest;
{$endif}

const
  ARBITRARY_STOPEXCEPT_MSG = 'An arbitrary StopExpectingException message';
  EXPECTED_STOPEXCEPT_MSG = 'Expected exception "EUnitTestException" '
                            + 'but there was none. '
                            + ARBITRARY_STOPEXCEPT_MSG;

var
// Used by code to test memory leak detection.
  AnObject: TObject = nil;

type
// prototype for CheckEqualsBin/Hex
  TCheckEqualsBinHex = procedure(expected,actual:longword; msg:string = '';
                                    digits:integer=0) of object;

  TRunExceptionCase = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
  end;

  TRunExceptionTornDown = class(TRunExceptionCase)
  protected
    procedure TearDown; override;
  public
    TornDown: boolean;
  end;

  TRunAndTearDownException = class(TRunExceptionTornDown)
  protected
    procedure TearDown; override;
  end;

  TRunFalseAssertCase = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
  end;

  TSetupException = class(TMonitoredTestCase)
  public
    procedure SetUp; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
  end;

  TSetupExceptionTornDown = class(TRunExceptionTornDown)
  public
    procedure SetUp; override;
  end;

  TSuccessCase = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
  end;

  TFailsCase = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
  end;

  TTearDownException = class(TSuccessCase)
  protected
    procedure TearDown; override;
  end;

  TTestMethodEnumClass = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Method0;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Method1;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Method2;
  end;

  TStopTest = class(TMonitoredTestCase)
  private
    procedure DoNothing;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testOne;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testTwo;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure doStop;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure notExecutedOne;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure notExecutedTwo;
  end;

  TExpectedTest = class(TMonitoredTestCase)
  protected
    function  RaiseException :Integer;
{$IFDEF CLR}
  public
{$ENDIF}
    procedure RaiseExceptionProc;
    procedure DoNothing;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testRaised;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNotRaised;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testEnd;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure testStart;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestChecksAfterException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDifferentFromExpected;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestInheritsFromExpected;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckWrongException;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNoExceptionRaised;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckAndNoExceptionRaised;
  end;

  TEmptyTest = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure NoChecksTest;
  end;

  TLeakyTest = class(TMonitoredTestCase)
  public
    procedure SetUp; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDoesNotLeak;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestContainsTObjectLeak;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestContainsAllowedTObjectLeak;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestContainsAllowedTObjectLeakByList;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestContainsAllowedTObjectLeakByEmptyList;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestContainsAllowedLeakArrayLongList;
  end;

  TLeakySetupTearDown = class(TMonitoredTestCase)
  public
    FreeInTearDown: boolean;
    procedure SetUp; override;
    procedure TearDown; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCaseDoesNotLeak;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSetupTearDownLeakDetect;
  end;

  TTestWithStatusMsgs = class(TMonitoredTestCase)
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure Test;
  end;

  TFixtureSuite = class(TTestSuite)
  public
    SetUpCalled: boolean;
    TearDownCalled: boolean;

    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TMockedTimeCase = class(TTestCase)
  public
    function ElapsedTestTime: Cardinal; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure SuccessOne;
{$IFDEF CLR}[Test]{$ENDIF}
    procedure SuccessTwo;
  end;

  TMockedTimeSuite = class(TTestSuite)
  public
    function ElapsedTestTime: Cardinal; override;
  end;

{ TMonitoredTestCase }

destructor TMonitoredTestCase.Destroy;
begin
  inherited Destroy;
end;

procedure TMonitoredTestCase._AddRef;
begin
{$IFNDEF CLR}
  inherited _AddRef;
{$ENDIF}
end;

procedure TMonitoredTestCase._Release;
begin
{$IFNDEF CLR}
  inherited _Release;
{$ENDIF}
end;

{ TRunExceptionTornDown }

procedure TRunExceptionTornDown.TearDown;
begin
  TornDown := true;
end;

{ TRunAndTearDownException }

procedure TRunAndTearDownException.TearDown;
begin
  inherited;
  raise EUnitTestException.Create('');
end;

{ TVerifierTestCase }

procedure TVerifierTestCase.Verify(AResult: TTestResult;
  runCount, failCount, errCount: Integer);
begin
  assert(AResult <> nil);
  CheckEquals(runCount,  AResult.runCount,      'wrong RunCount');
  CheckEquals(failCount, AResult.failureCount,  'wrong FailureCount');
  CheckEquals(errCount,  AResult.errorCount,    'wrong ErrorCount');
end;

procedure TVerifierTestCase.VerifyError(ATest: ITest; errCount: Integer);
var
  AResult: TTestResult;
begin
  assert((ATest <> nil) and (errCount >= 1));
  AResult := ATest.run;
  try
    Verify(AResult, 1, 0, errCount);
  finally
    AResult.Free
  end
end;

procedure TVerifierTestCase.VerifyFailure(ATest: ITest);
var
  AResult: TTestResult;
begin
  assert(ATest <> nil);
  // don't let ref counting free the test too early
  AResult := ATest.run;
  try
    Verify(AResult, 1, 1, 0);
  finally
    AResult.Free;
  end
end;

procedure TVerifierTestCase.VerifyLackOfSuccess(ATest: ITest);
var
  AResult: TTestResult;
begin
  assert(ATest <> nil);
  AResult := ATest.run;
  try
    check(AResult.runCount = 1, 'wrong RunCount');
    check((AResult.failureCount + AResult.errorCount) > 0, 'wrong Failures+Errors');
    check(not AResult.wasSuccessful, 'should not have suceeded');
  finally
    AResult.Free;
  end
end;

procedure TVerifierTestCase.VerifySuccess(ATest: ITest);
var
  AResult: TTestResult;
begin
  assert(ATest <> nil);
  AResult := ATest.run;
  try
    Verify(AResult, 1, 0, 0);
    check(AResult.wasSuccessful, 'should have suceeded');
  finally
    AResult.Free;
  end
end;

procedure TVerifierTotalTestCase.VerifySuccess(ATest: ITest);
var
  AResult: TTestResult;
begin
  assert(ATest <> nil);
  AResult := TTestResult.Create;
  AResult.FailsIfNoChecksExecuted := ATest.FailsOnNoChecksExecuted;
  AResult.FailsIfMemoryLeaked := ATest.FailsOnMemoryLeak;
  AResult.IgnoresMemoryLeakInSetUpTearDown :=
    ATest.IgnoreSetUpTearDownLeaks;
  ATest.RunWithFixture(AResult);
  try
    Verify(AResult, 1, 0, 0);
    check(AResult.wasSuccessful, 'should have suceeded');
  finally
    AResult.Free;
  end
end;

procedure TVerifierTotalTestCase.VerifyFailure(ATest: ITest);
var
  AResult: TTestResult;
begin
  assert(ATest <> nil);
  AResult := TTestResult.Create;
  AResult.FailsIfNoChecksExecuted := ATest.FailsOnNoChecksExecuted;
  AResult.FailsIfMemoryLeaked := ATest.FailsOnMemoryLeak;
  ATest.RunWithFixture(AResult);
  try
    Verify(AResult, 1, 1, 0);
  finally
    AResult.Free;
  end
end;


procedure TTestTest.TestBoolToStr;
begin
  CheckEqualsString('True',  BoolToStr(True),  'BoolToStr(True)');
  CheckEqualsString('False', BoolToStr(False), 'BoolToStr(False)');
end;



procedure TTestTest.TestCheck;
var
  s1, s2, s3 :WideString;
begin
  Check(true, 'Check');
  CheckEquals(Integer(1), 1,                   'CheckEquals    Integer');
  CheckNotEquals(Integer(1), 2,                'CheckNotEquals Integer');
  CheckEquals(1.0, 1.1, 0.15,         'CheckEquals    Double');
  CheckNotEquals(1.0, 1.16, 0.15,     'CheckNotEquals Double');
  CheckEqualsString('abc', 'abc',     'CheckEquals    String');
  CheckNotEqualsString('abc', 'abcd', 'CheckNotEquals String');
  CheckEquals(true, true,             'CheckEquals    Boolean');
  CheckNotEquals(true, false,         'CheckNotEquals Boolean');

  CheckEqualsBin(1, 1,                'CheckEqualsBin  Longword');
  CheckNotEqualsBin(1, 2,             'CheckNotEqualsBin  Longword');
  CheckEqualsHex(1, 1,                'CheckEqualsHex  Longword');
  CheckNotEqualsHex(1, 2,             'CheckNotEqualsHex  Longword');

  CheckNull(TObject(nil),        'CheckNull');
  CheckNotNull(TObject(self),    'CheckNotNull object');
  CheckSame(TObject(self), self, 'CheckSame    object');

  // need the TTestCase(self) cast to work around Delphi typing quirks
  CheckNull(TTEstCase(nil) as ITest,        'CheckNull');
  CheckNotNull(TTestCase(self) as ITest,    'CheckNotNull interface');
  CheckSame(TTestCase(self) as ITest, TTestCase(self) as ITest, 'CheckSame    interface');

  CheckIs(self, TObject, 'self not a TObject');

  s1 := 'aaa'#1024;
  s2 := 'aaa';
  s3 := 'bbb';

  {$IFNDEF CLR}
  CheckEqualsWideString(s1, s1, 'CheckEquals WideString');
  CheckNotEqualsWideString(s1, s2, 'CheckNotEquals WideString');
  CheckNotEqualsWideString(s2, s3, 'CheckNotEquals WideString');
  {$ENDIF}

  CheckTrue(true, 'CheckTrue');
  CheckFalse(false, 'CheckFalse');
end;

procedure TTestTest.TestCheckEqualsBinHexMessage;

  function ExpWas(msg, sExp, sAct: string):string;
  begin
    Result:=Format(sExpButWasFmt, [msg, sExp, sAct]);
  end;

  function BothAre(msg, sAct: string):string;
  begin
    Result:=Format(sExpAndActualFmt, [msg, sAct]);
  end;

  procedure CheckFailureMessage(ACheck:TCheckEqualsBinHex; iExp,iAct,dig:longword;
                                msg,ExpectedFailMsg:string);
  begin
    try
      ACheck(iExp,iAct,msg,dig);
      Assert(False,'Expected Failure: '+msg);
    except
      on E:ETestFailure do
      begin
        CheckEqualsString(ExpectedFailMsg,E.Message);
      end;
    end;
  end;

begin

  // Check the binary string output:
  CheckFailureMessage(CheckEqualsBin,0,1,1,'',ExpWas('','0','1')); // empty msg
  CheckFailureMessage(CheckEqualsBin,0,15,4,'A1',ExpWas('A1, ','0000','1111'));
  CheckFailureMessage(CheckEqualsBin,0,$55AA55AA,32,'B1',
    ExpWas('B1, ','00000000000000000000000000000000',
                  '01010101101010100101010110101010'));
  CheckFailureMessage(CheckNotEqualsBin,3,3,2,'C1',BothAre('C1, ','11'));

  // Check the Hex string output:
  CheckFailureMessage(CheckEqualsHex,0,15,1,'A2',ExpWas('A2, ','0',IntToHex(15, 1)));
  CheckFailureMessage(CheckEqualsHex,0,$55AA55AA,8,'B2',ExpWas('B2, ','00000000',IntToHex($55AA55AA, 8)));
  CheckFailureMessage(CheckNotEqualsHex,999,999,4,'C2',BothAre('C2, ',IntToHex($03E7, 4)));

end;

{$IFNDEF CLR} // [KGS] don't expect CheckEquals(Mem) to work under CLR - pointers are unsafe
procedure TTestTest.TestCheckEqualsMem; // Added KGS 06/06/2005
// Tests TAbstractTest.CheckEqualsMem / CheckNotEqualsMem
type TCheckEqualsMem = procedure(expected,actual:pointer; size:longword; msg:string) of object;
var
  a,b: byte;
  x,y,z:Int64;
  b1,b2: array[0..1023] of byte;
  i: integer;

  procedure CheckEqualsMemFailure(ACheck:TCheckEqualsMem; pExp,pAct:pointer;
                                  size:longword; msg:string);
  begin
    try
      ACheck(pExp,pAct,size,msg);
      Assert(False,'Expected Failure: '+msg);
    except
      on E:ETestFailure do
      begin
        // we expected this!
      end;
    end;
  end;

begin

  // 1. Test on a single byte:
  a:=1;
  b:=0;
  CheckNotEqualsMem(@a,@b,1,'Missed Diff a<>b');
  b:=a;
  CheckEqualsMem(@a,@b,1,'False Diff a<>b');

  // 2. Test 8-byte (Int64) fields:
  x:=$123456789ABCDEF0;
  y:=$123456789ABCDEF0;
  z:=$023456789ABCDEF0; // differs in MSByte = last byte, offset 7
  CheckEqualsMem(@x,@y,8,'False Diff x<>y');
  CheckEqualsMem(@x,@z,7,'x<>z first 7');
  CheckNotEqualsMem(@x,@z,8,'Failed to find x<>z');

  // 3. Test on a 1KB byte-field:
  for i:=0 to 1023 do b1[i]:=i and $FF; // test pattern
  Move(b1[0],b2[0],1024); // copy b1 to b2 so should be identical
  CheckEqualsMem(@b1[0],@b2[0],1024,'Diff b1<>b2');
  // Induce an error:
  Inc(b2[777]); // should create a discrepancy at offset 777
  CheckEqualsMem(@b1[0],@b2[0],777,'First 777 still OK');
  CheckNotEqualsMem(@b1[0],@b2[0],1023,'Induced error b1<>b2 not found');

  // 4. Negative tests: These calls should fail! If they don't, it's an error.
  CheckEqualsMemFailure(CheckNotEqualsMem,@a,@b,1,'Unexpected diff a<>b');
  CheckEqualsMemFailure(CheckNotEqualsMem,@x,@y,8,'Unexpected diff x<>y');
  CheckEqualsMemFailure(CheckNotEqualsMem,@x,@y,8,'Unexpected diff x<>y');
  CheckEqualsMemFailure(CheckNotEqualsMem,@x,@z,7,'Unexpected diff x<>z, 7');
  CheckEqualsMemFailure(CheckEqualsMem,@x,@z,8,'Expected diff x<>z, 8');
  CheckEqualsMemFailure(CheckEqualsMem,@b1[0],@b2[0],1023,'Error b1<>b2 not found');

end;

procedure TTestTest.TestFirstByteDiff;
// Tests TestFrameWork.FirstByteDiff function
// (utility for Check(Not)EqualsMem failure reporting):
var
  a,b,val1,val2: byte;
  x,z:Int64;
  b1,b2: array[0..1023] of byte;
  i: integer;
begin
  // 1. Test on a single byte:
  a:=1;
  b:=0;
  CheckEquals(0,FirstByteDiff(@a,@b,1,val1,val2),'FirstDiff(a,b,1)');
  CheckEquals(Integer(a),Val1,'a<>Val1');
  CheckEquals(Integer(b),Val2,'b<>Val2');

  // Try size=0: should yield -1
  CheckEquals(-1,FirstByteDiff(@a,@b,0,val1,val2),'FirstDiff(size=0)');

  b:=a; // Now return should be -1 because a=b
  CheckEquals(-1,FirstByteDiff(@a,@b,1,val1,val2),'FirstDiff(a=b,1)');

  // 2. Test 8-byte (Int64) fields:
  x:=$123456789ABCDEF0;
  z:=$023456789ABCDEF0; // differs in MSByte = last byte, offset 7
  CheckEquals(7,FirstByteDiff(@x,@z,8,val1,val2),'FirstDiff(x,z,8)');
  CheckEquals(Int64($12),val1,'x-val1');
  CheckEquals(Int64($02),val2,'z-val2');

  // 3. Test 1024-byte (1KB) field:
  for i:=0 to 1023 do b1[i]:=i and $FF; // test pattern
  Move(b1[0],b2[0],1024); // copy b1 to b2 so should be identical
  // Induce an error:
  Inc(b2[777]); // should create a discrepancy at offset 777
  CheckEquals(777,FirstByteDiff(@b1[0],@b2[0],1024,val1,val2),'FirstDiff(b1,b2,1024)');
  CheckEquals(Integer($09),val1,'b1-val1');
  CheckEquals(Integer($0A),val2,'b2-val2');

end;

{$ENDIF} // for conditional No CLR

procedure TTestTest.TestElapsedTestTime;
const
  DELAY = 50;
var
  t, min, max: Cardinal;
begin
  Sleep(DELAY);
  min := (DELAY * 5)  div 10;
  max := (DELAY * 15) div 10 + 1;
  t := ElapsedTestTime;
  check((t <= max), Format('Expected elapsed time to be less than or equal to %d but was %d', [max, t]));
  check((t >= min), Format('Expected elapsed time to be greater than or equal to %d but was %d', [min, t]));
end;

procedure TTestTest.TestEmptyTestFailure;
var
  EmptyTest: TEmptyTest;
begin
  EmptyTest := TEmptyTest.Create('Test');
  VerifyFailure(EmptyTest);
end;

procedure TTestTest.TestFailOnDisAllowedUnCalledCheck;
var
  EmptyTest: TEmptyTest;
begin
  EmptyTest := TEmptyTest.Create('NoChecksTest');
  EmptyTest.FailsOnNoChecksExecuted := True;
  VerifyFailure(EmptyTest);
end;

procedure TTestTest.TestPassOnAllowedUnCalledCheck;
var
  EmptyTest: TEmptyTest;
begin
  EmptyTest := TEmptyTest.Create('NoChecksTest');
  EmptyTest.FailsOnNoChecksExecuted := False;
  VerifySuccess(EmptyTest);
end;

procedure TTestTest.TestAlloweLeakListIterator;
var  // Test based on current allowed array length of 4
  ListIteratorValue: TListIterator;
  Value: integer;
begin
  AllowedMemoryLeakSize := 44;
  // Note. Using more values than necessary to show it's handled gracefully
  SetAllowedLeakArray([3, 97, -10]);
  Value := AllowedMemoryLeakSize;
  Check(Value = 44, 'Should return 44 but returned ' +
    IntToStr(Value));

  ListIteratorValue := AllowedLeaksIterator;
  Value := ListIteratorValue;
  Check(Value = 44, 'ListIterator should return 44 but returned ' +
    IntToStr(Value));
  Value := ListIteratorValue;
  Check(Value =  3, 'ListIterator should return 3 but returned ' +
    IntToStr(Value));
  Value := ListIteratorValue;
  Check(Value = 97, 'ListIterator should return 97 but returned ' +
    IntToStr(Value));
  Value := ListIteratorValue;
  Check(Value = -10, 'ListIterator should return -10 but returned ' +
    IntToStr(Value));
  Value := ListIteratorValue;
  Check(Value =  0, 'ListIterator should return 0 but returned ' +
    IntToStr(Value));
end;

procedure TTestTest.TestError;
var
  ErrorTestCase: TRunExceptionCase;
begin
  ErrorTestCase := TRunExceptionCase.Create('Test');
  VerifyError(ErrorTestCase);
end;

procedure TTestTest.TestFailure;
var
  FailureTestCase: TRunFalseAssertCase;
begin
  FailureTestCase := TRunFalseAssertCase.Create('Test');
  VerifyFailure(FailureTestCase);
end;

{$IFDEF VER130}
// It is Delphi 5 so define the Supports function as it was not introduced until Delphi 6
function Supports(const AClass: IUnknown; const IID: TGUID): Boolean;
var
  Intf: IUnknown;
begin
  Result := (AClass.QueryInterface(IID, Intf) = S_OK) and Assigned(Intf);
end;
{$ENDIF}

procedure TTestTest.SetUp;
var
  SuiteOfTests : ITest;
begin
  inherited;
  SuiteOfTests := nil;
  SuiteOfTests := TestFramework.RegisteredTests;
  Assert(SuiteOfTests <> nil);
  Assert(Supports(SuiteOfTests, ITest));
  fTestCount := SuiteOfTests.Tests.Count;
end;

procedure TTestTest.TearDown;
var
  SuiteOfTests : ITest;
begin
  inherited;
  SuiteOfTests := TestFramework.RegisteredTests;
  while ((SuiteOfTests <> nil) and (SuiteOfTests.Tests.Count > fTestCount)) do
  begin
    SuiteOfTests.Tests.Items[SuiteOfTests.Tests.Count-1] := nil;
    SuiteOfTests.Tests.Delete(SuiteOfTests.Tests.Count-1);
  end;
  SuiteOfTests := nil;
end;

procedure TTestTest.TestRegisterTest;
var
  TestsRegistered: integer;
begin
  AllowedMemoryLeakSize := 56;
  { test needs more work - the below passes, but it's not checking everything }
  TestsRegistered := RegisteredTests.Tests.Count;

  RegisterTest('', TSuccessCase.Suite);
  Check((RegisteredTests.Tests[TestsRegistered] as ITest).name = 'TSuccessCase');

  TestsRegistered := RegisteredTests.Tests.Count;
  RegisterTest('Suite', TSuccessCase.Suite);
  Check((RegisteredTests.Tests[TestsRegistered] as ITest).name = 'Suite');

  RegisterTest('Suite.ChildA', TSuccessCase.Suite);
  RegisterTest('Suite.ChildB', TSuccessCase.Suite);
end;

procedure TTestTest.TestRunAndTearDownFails;
var
  ATornDown: TRunAndTearDownException;
begin
  ATornDown := TRunAndTearDownException.Create('Test');
  ATornDown._AddRef;
  try
    VerifyError(ATornDown, 2);
    check(ATornDown.TornDown, 'not torn down');
  finally
    ATornDown._Release;
  end
end;

procedure TTestTest.TestSetupException;
var
  ASetupException: TSetupException;
begin
  ASetupException := TSetupException.Create('Test');
  VerifyError(ASetupException);
end;

procedure TTestTest.TestSuccess;
var
  ASuccessCase: TSuccessCase;
begin
  ASuccessCase := TSuccessCase.Create('Test');
  VerifySuccess(ASuccessCase);
end;

procedure TTestTest.TestTearDownAfterError;
var
  ARunExceptionTornDown: TRunExceptionTornDown;
begin
  ARunExceptionTornDown := TRunExceptionTornDown.Create('Test');

  ARunExceptionTornDown._AddRef;
  try
    VerifyError(ARunExceptionTornDown);
    check(ARunExceptionTornDown.TornDown, 'not torn down');
  finally
    ARunExceptionTornDown._Release;
  end;
end;

procedure TTestTest.TestTearDownFails;
var
  ATearDownException: TTearDownException;
begin
  ATearDownException := TTearDownException.Create('Test');
  VerifyError(ATearDownException);
end;

procedure TTestTest.TestTearDownSetupFails;
var
  ASetupExceptionTornDown: TSetupExceptionTornDown;
begin
  ASetupExceptionTornDown := TSetupExceptionTornDown.Create('Test');

  ASetupExceptionTornDown._AddRef;
  try
    VerifyError(ASetupExceptionTornDown);
    check(ASetupExceptionTornDown.TornDown);
  finally
    ASetupExceptionTornDown._Release
  end;
end;

procedure TTestTest.TestWasNotSuccessful;
var
  ARunExceptionCase: TRunExceptionCase;
begin
  ARunExceptionCase := TRunExceptionCase.Create('Test');
  VerifyLackOfSuccess(ARunExceptionCase);
end;

procedure TTestTest.TestWasStopped;
var
  AStopCase :ITest;
  AResult   :TTestResult;
begin
  AStopCase := TStopTest.Suite;
  AResult := AStopCase.run;
  try
    Verify(AResult, 3, 1, 0);
    check(AResult.WasStopped);
  finally
    AResult.Free
  end
end;

procedure TTestTest.TestWasSuccessful;
var
  ASuccessCase: TSuccessCase;
begin
  ASuccessCase := TSuccessCase.Create('Test');
  VerifySuccess(ASuccessCase);
end;

{ TRunExceptionCase }

procedure TRunExceptionCase.Test;
begin
  raise EUnitTestException.Create('');
end;

{ TRunFalseAssertCase }

procedure TRunFalseAssertCase.Test;
begin
  check(false);
end;

{ TSetupException }

procedure TSetupException.SetUp;
begin
  raise EUnitTestException.Create('');
end;

procedure TSetupException.Test;
begin
  check(true);
end;

{ TSuccessCase }

procedure TSuccessCase.Test;
begin
  check(true);
end;

procedure TFailsCase.Test;
begin
  Check(False);
end;

{ TTearDownException }

procedure TTearDownException.TearDown;
begin
  raise EUnitTestException.Create('');
end;

{ TSetupExceptionTornDown }

procedure TSetupExceptionTornDown.SetUp;
begin
  raise EUnitTestException.Create('');
end;

{ TTestTestResult }

procedure TTestTestResult.SetUp;
begin
  FailsOnNoChecksExecuted := False;
end;

procedure TTestTestResult.TestElapsedTime;
var
  MockTimeSuite: ITest;
  ATestResult: TTestResult;
begin
  MockTimeSuite := TMockedTimeSuite.Create(TMockedTimeCase);
  ATestResult := nil;
  try
    ATestResult := MockTimeSuite.run;
    CheckEquals(3, ATestResult.TotalTime, 'TestResult.TotalTime');
  finally
    ATestResult.Free;
  end;
end;

procedure TTestTestResult.TestRun;
var
  ASuccessCase: ITest;
  ATestResult: TTestResult;
begin
  ASuccessCase := TSuccessCase.Create('Test');
  ATestResult := nil;
  try
    { TTestCase.run calls TTestResult.run. This test checks to ensure an
      AV bug in TTestResult.run is fixed.}
    ATestResult := ASuccessCase.run;
  finally
    ATestResult.Free;
  end;
end;

{ TTestMethodEnumerator }

procedure TTestMethodEnumerator.TestMethodEnumerator;
const
  TotalMethods: integer = 3;
var
  i: integer;
begin
  FMethodEnumerator := TMethodEnumerator.Create(TTestMethodEnumClass);
  try
    check(FMethodEnumerator.MethodCount = TotalMethods);
    for i := 0 to TotalMethods - 1 do
      check(FMethodEnumerator.NameOfMethod[i] = 'Method' + IntToStr(i));
  finally
    FMethodEnumerator.Free;
  end;
end;

{ TTestMethodEnumClass }

procedure TTestMethodEnumClass.Method0;
begin
  // do nothing, just used for TTestMethodEnumerator.TestMethodEnumerator
end;

procedure TTestMethodEnumClass.Method1;
begin
  // do nothing, just used for TTestMethodEnumerator.TestMethodEnumerator
end;

procedure TTestMethodEnumClass.Method2;
begin
  // do nothing, just used for TTestMethodEnumerator.TestMethodEnumerator
end;

{ TStopTest }

procedure TStopTest.DoNothing;
begin
 // Stub so empty tests will not fail
end;

procedure TStopTest.doStop;
begin
  stopTests;
end;

procedure TStopTest.notExecutedOne;
begin
  DoNothing;
end;

procedure TStopTest.notExecutedTwo;
begin
  DoNothing;
end;

procedure TStopTest.testOne;
begin
  DoNothing;
end;

procedure TStopTest.testTwo;
begin
  DoNothing;
end;

{ TExpectedTest }

procedure TExpectedTest.TestNotRaised;
begin
  StartExpectingException(EUnitTestException);
  StopExpectingException(ARBITRARY_STOPEXCEPT_MSG);
end;

procedure TExpectedTest.testRaised;
begin
  StartExpectingException(EUnitTestException);
  raise EUnitTestException.Create('testStartExpectingException');
end;

procedure TExpectedTest.testStart;
begin
  StopExpectingException;
end;

procedure TExpectedTest.testEnd;
begin
  StartExpectingException(EUnitTestException);
end;

{ TEmptyTest }

{$IFOPT O-}
{$DEFINE UNOPTIMIZED}
{$OPTIMIZATION ON}
{$ENDIF}
procedure TEmptyTest.Test;
begin
// Test left intentionally empty
end;
{$IFDEF UNOPTIMIZED}
{$OPTIMIZATION OFF}
{$ENDIF}

{$IFOPT O+}
{$DEFINE OPTIMIZED}
{$OPTIMIZATION OFF}
{$ENDIF}
procedure TEmptyTest.NoChecksTest;
begin
// Test contains no executed calls to Check(...);
end;
{$IFDEF OPTIMIZED}
{$OPTIMIZATION ON}
{$ENDIF}

function TExpectedTest.RaiseException: Integer;
begin
  raise EUnitTestException.Create('testing exception');
  Result := 0;
end;

procedure TExpectedTest.RaiseExceptionProc;
begin
  RaiseException;
end;

procedure TExpectedTest.TestChecksAfterException;
begin
  StartExpectingException(EUnitTestException);
  CheckEquals(0, RaiseException, 'No error should have been reported');
  CheckEquals(0, RaiseException, 'This code is never reached! No error.');
  CheckEquals(Integer(0), 1, 'This code should never be reached!');
end;


procedure TExpectedTest.TestDifferentFromExpected;
begin
  StartExpectingException(ERangeError);
  CheckEquals(0, RaiseException, 'No error should have been reported');
end;

procedure TExpectedTest.TestInheritsFromExpected;
begin
  StartExpectingException(Exception);
  CheckEquals(0, RaiseException, 'No error should have been reported');
end;

procedure TExpectedTest.TestCheckException;
begin
{$IFDEF CLR}
  CheckException('RaiseExceptionProc', EUnitTestException);
{$ELSE}
  CheckException(RaiseExceptionProc, EUnitTestException);
{$ENDIF}
end;

procedure TExpectedTest.TestCheckWrongException;
begin
{$IFDEF CLR}
  CheckException('RaiseExceptionProc', ERangeError);
{$ELSE}
  CheckException(RaiseExceptionProc, ERangeError);
{$ENDIF}
end;

procedure TExpectedTest.TestNoExceptionRaised;
begin
  StartExpectingException(Exception);
end;

procedure TExpectedTest.DoNothing;
begin
  // nothing
end;

procedure TExpectedTest.testCheckAndNoExceptionRaised;
begin
{$IFDEF CLR}
  CheckException('DoNothing', Exception);
{$ELSE}
  CheckException(DoNothing, Exception);
{$ENDIF}
end;

{ TTestExceptionChecks }

procedure TTestExceptionChecks.TestExpectedException;
var
  AExpectedCase :ITest;
  AResult   :TTestResult;
begin
  AExpectedCase := TExpectedTest.Suite;
  AResult := AExpectedCase.run;
  try
    Verify(AResult, AExpectedCase.CountEnabledTestCases, 6, 0);
  finally
    AResult.Free
  end
end;


procedure TTestExceptionChecks.TestIndividualException(AName: string; FailCnt, ErrCnt: Integer);
var
  AExpectedCase: ITest;
  AResult: TTestResult;
begin
  AExpectedCase := TExpectedTest.Create(AName);
  AResult := AExpectedCase.run;
  try
    Verify(AResult, 1, FailCnt, ErrCnt);
  finally
    AResult.Free
  end;
end;

procedure TTestExceptionChecks.TestCheckException;
begin
  TestIndividualException('TestCheckException', 0, 0);
end;

procedure TTestExceptionChecks.TestChecksAfterException;
begin
  TestIndividualException('TestChecksAfterException', 0, 0);
end;

procedure TTestExceptionChecks.TestCheckWrongException;
begin
  TestIndividualException('TestCheckWrongException', 1, 0);
end;

procedure TTestExceptionChecks.TestDifferentFromExpected;
begin
  TestIndividualException('TestDifferentFromExpected', 1, 0);
end;

procedure TTestExceptionChecks.TestInheritsFromExpected;
begin
  TestIndividualException('TestInheritsFromExpected', 0, 0);
end;

procedure TTestExceptionChecks.TestNoExceptionRaised;
begin
  TestIndividualException('TestNoExceptionRaised', 1, 0);
end;

procedure TTestExceptionChecks.TestCheckAndNoExceptionRaised;
begin
  TestIndividualException('TestCheckAndNoExceptionRaised', 1, 0);
end;

procedure TTestExceptionChecks.TestNotRaised;
var
  AExpectedCase :ITest;
  AResult   :TTestResult;
begin
  AExpectedCase := TExpectedTest.Create('TestNotRaised');
  AResult := AExpectedCase.run;
  try
    Verify(AResult, 1, 1, 0);
    CheckEqualsString( EXPECTED_STOPEXCEPT_MSG,
                 AResult.Failures[0].ThrownExceptionMessage,
                 'wrong message thrown from StopExpectingException');
  finally
    AResult.Free
  end;
end;

{ TTestWthStatusMsgs }

procedure TTestWithStatusMsgs.Test;
begin
  Status('Line 1');
  Status('Line 2');
  Status('Line 3');
end;

{ TTestStatus }

procedure TTestStatus.testStatus;
var
  AStatusCase: TTestWithStatusMsgs;
const
{$IFDEF WIN32}
  constLineDelim = #13#10;
{$ENDIF}
{$IFDEF CLR}
  constLineDelim = #13#10;
{$ENDIF}
{$IFDEF LINUX}
  constLineDelim = #10;
{$ENDIF}
  constStatusTestStr =
      'Line 1' + constLineDelim
      + 'Line 2' + constLineDelim
      + 'Line 3' + constLineDelim;
begin
  AStatusCase := TTestWithStatusMsgs.Create('Test');
  try
    AStatusCase._AddRef;
    VerifySuccess(AStatusCase);

    CheckEqualsString(AStatusCase.getStatus, constStatusTestStr,
      'Status not correctly validated in test');
  finally
    AStatusCase._Release;
  end;
end;

{ TFixtureSuite }

procedure TFixtureSuite.SetUp;
begin
  inherited;
  SetUpCalled := true;
end;

procedure TFixtureSuite.TearDown;
begin
  TearDownCalled := true;
  inherited;
end;

{ TTestTestSuite }

procedure TTestTestSuite.TestSuiteSetupTearDown;
var
  AFixtureSuite: TFixtureSuite;
begin
  AFixtureSuite := TFixtureSuite.Create('Fixture Suite');
{$IFNDEF CLR}
  AFixtureSuite._AddRef;
{$ENDIF}
  try
    AFixtureSuite.AddTests(TSuccessCase);
    VerifySuccess(AFixtureSuite);
    CheckEquals(true, AFixtureSuite.SetUpCalled, 'Suite SetUp called');
    CheckEquals(true, AFixtureSuite.TearDownCalled, 'Suite TearDown called');
  finally
{$IFNDEF CLR}
    AFixtureSuite._Release;
{$ENDIF}
  end;
end;

{ TMockedTimeCase }

function TMockedTimeCase.ElapsedTestTime: Cardinal;
begin
  Result := 1;
end;

procedure TMockedTimeCase.SuccessOne;
begin
  Check(true);
end;

procedure TMockedTimeCase.SuccessTwo;
begin
  Check(true);
end;

{ TMockedTimeSuite }

function TMockedTimeSuite.ElapsedTestTime: Cardinal;
begin
  Result := 3;
end;

procedure TTestMemLeakTestSuite.SetUp;
var
  SuiteOfTests : ITest;
begin
  inherited;
  SuiteOfTests := nil;
  SuiteOfTests := TestFramework.RegisteredTests;
  Assert(SuiteOfTests <> nil);
  Assert(Supports(SuiteOfTests, ITest));
  fTestCount := SuiteOfTests.Tests.Count;
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TearDown;
var
  SuiteOfTests : ITest;
begin
  inherited;
  SuiteOfTests := nil;
  SuiteOfTests := TestFramework.RegisteredTests;
  while ((SuiteOfTests <> nil) and (SuiteOfTests.Tests.Count > fTestCount)) do
  begin
    SuiteOfTests.Tests.Items[SuiteOfTests.Tests.Count-1] := nil;
    SuiteOfTests.Tests.Delete(SuiteOfTests.Tests.Count-1);
  end;
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestClassCanDetectFailure;
var
  TestWillFail: TFailsCase;
begin
{$IFDEF CLR}
  Check(True);
{$ELSE}
  Check(IsMemoryManagerSet, 'Memory Manager not loaded');
{$ENDIF}
  TestWillFail := TFailsCase.Create('Test');
  TestWillFail.FailsOnMemoryLeak := True;
  VerifyFailure(TestWillFail);
end;

procedure TTestMemLeakTestSuite.TestCaseDoesNotLeak;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestDoesNotLeak');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifySuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseLeaksTObject;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestContainsTObjectLeak');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifyFailure(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseLeaksTObjectReportDisabled;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestContainsTObjectLeak');
  LeakyTest.FailsOnMemoryLeak := False;
  VerifySuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseLeaksTObjectLeakOfSizeAllowed;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestContainsAllowedTObjectLeak');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifySuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseLeaksTObjectLeakOfListSizeAllowed;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestContainsAllowedTObjectLeakByList');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifySuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseLeaksAllowedTObjectLeakOfEmptyList;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestContainsAllowedTObjectLeakByEmptyList');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifySuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseFailsOnOverSizeAllowedLeakArrayList;
var
  LeakyTest: TLeakyTest;
begin
  LeakyTest := TLeakyTest.Create('TestContainsAllowedLeakArrayLongList');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifyLackOfSuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseNoLeaksAtSetupTearDownLevel;
var
  LeakyTest: TLeakySetupTearDown;
begin
  LeakyTest := TLeakySetupTearDown.Create('TestSetupTearDownLeakDetect');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifyFailure(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseLeaksAtSetupTearDownLevel;
var
  LeakyTest: TLeakySetupTearDown;

begin
  LeakyTest := TLeakySetupTearDown.Create('TestSetupTearDownLeakDetect');
  LeakyTest.FailsOnMemoryLeak := True;
  VerifyFailure(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseIgnoresLeaksAtSetupTearDownLevel;
var
  LeakyTest: TLeakySetupTearDown;

begin
  LeakyTest := TLeakySetupTearDown.Create('TestSetupTearDownLeakDetect');
  LeakyTest.FailsOnMemoryLeak := True;
  LeakyTest.IgnoreSetUpTearDownLeaks:= True;
  VerifySuccess(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TTestMemLeakTestSuite.TestCaseDoesNotIgnoreLeaksInTestProc;
var
  LeakyTest: TLeakyTest;

begin
  LeakyTest := TLeakyTest.Create('TestContainsTObjectLeak');
  LeakyTest.FailsOnMemoryLeak := True;
  LeakyTest.IgnoreSetUpTearDownLeaks:= True;
  VerifyFailure(LeakyTest);
  FreeAndNil(AnObject);
end;

procedure TLeakyTest.Setup;
begin
  FailsOnNoChecksExecuted := False;
end;

procedure TLeakyTest.TestDoesNotLeak;
begin
  Check(True, 'Test does not leak and never fails');
end;

procedure TLeakyTest.TestContainsTObjectLeak;
begin
  AnObject := TObject.Create;
end;

procedure TLeakyTest.TestContainsAllowedTObjectLeak;
begin
  AnObject := TObject.Create;
  AllowedMemoryLeakSize := 16;
end;

procedure TLeakyTest.TestContainsAllowedTObjectLeakByList;
begin
  AnObject := TObject.Create;
  SetAllowedLeakArray([1, 7, 16]);
end;

procedure TLeakyTest.TestContainsAllowedTObjectLeakByEmptyList;
begin
  AnObject := TObject.Create;
  AllowedMemoryLeakSize := 16;
  SetAllowedLeakArray([]);
end;

procedure TLeakyTest.TestContainsAllowedLeakArrayLongList;
begin
  AnObject := TObject.Create;
  SetAllowedLeakArray([1, 7, 16, 55]);  // causes deliberate failure
end;

procedure TLeakySetupTearDown.SetUp;
begin
  AnObject := TObject.Create;
end;

procedure TLeakySetupTearDown.TearDown;
begin
  if FreeInTearDown then
    FreeAndNil(AnObject);
end;

procedure TLeakySetupTearDown.TestCaseDoesNotLeak;
begin
  FreeInTearDown := True;
  Check(Assigned(AnObject), 'Object should be Assigned');
end;

procedure TLeakySetupTearDown.TestSetupTearDownLeakDetect;
begin
  // Dont allow TearDown to free object created in SetUp.
  // Cause mem leak at teardown phase.
  FreeInTearDown := false;
  Check(Assigned(AnObject), 'Object should be Assigned');
end;

{ TTestBasicTestCase4Leaks }

type
  TTiny1 = class(TTestCase)
  private
    FTestRan: boolean;
  public
    constructor Create(MethodName: string); override;
    destructor Destroy; override;
  published
    procedure NonEmptyProc;
  end;

  TTiny2 = class(TTestCase)
  private
    FTestRan: boolean;
  public
    constructor Create(MethodName: string); override;
    destructor Destroy; override;
  published
    procedure NonEmptyProc;
  end;

constructor TTiny1.Create(MethodName: string);
begin
  inherited Create(MethodName);
end;

destructor TTiny1.Destroy;
begin
  inherited;
end;

procedure TTiny1.NonEmptyProc;
begin
  FTestRan := True;
end;

constructor TTiny2.Create(MethodName: string);
begin
  inherited Create(MethodName);
end;

destructor TTiny2.Destroy;
begin
  inherited;
end;

procedure TTiny2.NonEmptyProc;
begin
  FTestRan := True;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfTAbstractTestLeaks;
var
  TrialTest: TAbstractTest;
  MLM: IMemLeakMonitor;
  LeakSize: integer;
begin
  MLM := MemLeakMonitor;
  TrialTest := TAbstractTest.Create('ABCDEFG12345');
  FreeAndNil(TrialTest);
  Check(not MLM.MemLeakDetected(LeakSize),
    'Named TAbstractTest leaks ' + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfTTestCaseLeaks;
var
  TrialTestCase: TTiny1;
  MLM: IMemLeakMonitor;
  LeakSize: integer;
begin
  MLM := MemLeakMonitor;
  TrialTestCase := TTiny1.Create('NonEmptyProc');
  FreeAndNil(TrialTestCase);
  Check(not MLM.MemLeakDetected(LeakSize), 'TTiny1 leaks '
    + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfTTestResultLeaks;
var
  TrialTestResult: TTestResult;
  MLM: IMemLeakMonitor;
  LeakSize: integer;
begin
  MLM := MemLeakMonitor;
  TrialTestResult := TTestResult.Create;
  FreeAndNil(TrialTestResult);
  Check(not MLM.MemLeakDetected(LeakSize), 'TTestResult leaks '
    + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfTStatusListnerLeaks;
var
  TrialStatusListner: IStatusListener;
  TrialTestResult: TTestResult;
  MLM: IMemLeakMonitor;
  LeakSize: integer;
begin
  TrialTestResult := nil;
  try
    TrialTestResult := TTestResult.Create;
    MLM := MemLeakMonitor;
    TrialStatusListner := TStatusToResultAdapter.Create(TrialTestResult);
    Check(Assigned(TrialStatusListner), 'Failed to create TStatusToResultAdapter');
    TrialStatusListner := nil;
    Check(not MLM.MemLeakDetected(LeakSize), 'IStatusListener leaks '
      + IntToStr(LeakSize) + ' Bytes');
    MLM := nil;
  finally
    FreeAndNil(TrialTestResult);
  end;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfTTestFalureLeaks;
var
  TrialTestFailure: TTestFailure;
  MLM: IMemLeakMonitor;
  LeakSize: integer;
  TrialTestCase: TTiny1;
begin
  MLM := MemLeakMonitor;
  TrialTestCase := TTiny1.Create('NonEmptyProc');
  TrialTestFailure := TTestFailure.Create(TrialTestCase, nil, 'A String');
  FreeAndNil(TrialTestFailure); // Note. Seems to free TrialTestCase early.
  Check(not MLM.MemLeakDetected(LeakSize), 'TTestFailure leaks '
    + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfRunTTestCaseLeaks;
var
  TrialTestCase: TTiny1;
  TrialTestResult: TTestResult;
  MLM: IMemLeakMonitor;
  LeakSize: integer;
begin
  MLM := MemLeakMonitor;
  TrialTestCase := TTiny1.Create('NonEmptyProc');
  TrialTestResult := TTestResult.Create;
  TrialTestResult.RunSuite(TrialTestCase);
  FreeAndNil(TrialTestResult);
  Check(not MLM.MemLeakDetected(LeakSize), 'TTiny1 leaks '
    + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfEnumeratorLeaks;
var
  MLM: IMemLeakMonitor;
  LeakSize: integer;
  TrialEnumerator: TMethodEnumerator;
begin
  MLM := MemLeakMonitor;
  TrialEnumerator := TMethodEnumerator.Create(TTiny1);
  FreeAndNil(TrialEnumerator);
  Check(not MLM.MemLeakDetected(LeakSize), 'TrialEnumerator leaks '
    + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

procedure TTestBasicTestCase4Leaks.TestCheckIfITestSuiteLeaks;
var
  MLM: IMemLeakMonitor;
  LeakSize: integer;
  TrialTestSuite: ITestSuite;

// Suite creation put inside local procedure to provide an end statement to
// hit before the memory difference is computed.
// This is believed to be due to the suite ref creation in TestFramework
// <function TestSuite(name: string; const Tests: array of ITest): ITestSuite;>
// Free up of interfaced refs needs a end statement to complete mem release.

  procedure LRunTest;
  begin
    TrialTestSuite := TestSuite('MyTestSuite', [TTiny1.Suite, TTiny2.Suite]);
    TrialTestSuite := nil;
  end;

begin
  MLM := MemLeakMonitor;
  LRunTest;
  Check(not MLM.MemLeakDetected(LeakSize), 'TrialTestSuite leaks '
    + IntToStr(LeakSize) + ' Bytes');
  MLM := nil;
end;

initialization
  RegisterTests('Framework Suites',[TTestTest.Suite,
                                    TTestTestResult.Suite,
                                    TTestStatus.Suite,
                                    TTestMethodEnumerator.Suite,
                                    TTestExceptionChecks.Suite,
{$IFDEF DETECTMEMLEAKS}
                                    TBasicMemMonitor.Suite,
                                    TMemMonitorGetErrorMessage.Suite,
                                    TMemMonitorGetErrorMessageNew.Suite,
                                    TTestMemLeakTestSuite.Suite,
                                    TTestBasicTestCase4Leaks.Suite,
{$ENDIF}
                                    TTestTestSuite.Suite]);
end.
