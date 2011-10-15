{ $Id: UnitTestLeak.pas 7 2008-04-24 11:59:47Z judc $ }
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
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
 
unit UnitTestLeak;

// Sample code to demonstrate testcase by testcase memory leak detection.
// Project must be compiled with "FASTMM" global defines to
// include FastMM4 code.
// See the documentation in FastMMMemLeakMonitor.pas for details on setting
// the FASTMM compile time options.
//
// After compiling the accomanying DUnit source and executing this demo project
// there will be a GUI menu option "Fail TestCase if memory leaked".
//
// The GUI option selection "Fail Test Case if Memory Leaked" is applied to
// every testcase unless overriden. The GUI setting can be overridden on a
// testsuite basis by setting the test property
// FailsOnMemLeakDetection := True/False in the SetUp procedure.
//
// Similarly, individual tests can override the GUI and SetUp setting in the
// Test procedure. The property can be set anywhere in either the SetUp or Test
// procedure as shown below in "procedure TestObjectLeakErrorSurpressed".
// It can even be set after all checks are performed in a Test procedure because
// a test failure would override any leak detection anyway. The property should
// not be set in the TeardDown procedure.
//
// Memory leak errors do not override testcase failures.
//
// In the examples, Non Leaking tests are placed between successive leaks to
// demonstrate that memory leak detection is performed on a case by case basis.
// The non leaky code also acts to clean up lost memory from the previous test.
//
// By default, Memory "recovered" from a preceeding leaky test also flags as an
// error. Setting the Test property IgnoreMemoryRecovery := True in either the
// SetUp or Test procedure prevents an error report.
// The properties FailsOnMemLeakDetection := False and
//                IgnoreMemoryRecovery := True should be used sparingly because
// they are easily burried in code and forgotten.
// It is suggested they should only be used use for deliberate leaks or leaks
// over which the coder has no control.
//
// A better way to handle unavoidable known leaks of a fixed size is as follows.
// In the SetUp or Test procedures call AddKnownMemoryLeak(leaksize, Qty)
// once or multiple times.
// Alternatively simply set the property KnownMemoryLeakSize := value;
// The advantage of setting KnownMemoryLeakSize over completely masking the leak
// is that new leaks wont go undetected.
// So, if a test is known to reclaim previously leaked memory it is preferable
// to set a negative leak value, so future errors are not masked, rather than
// setting IgnoreMemoryRecovery. The dissadvantage is that the previous test
// must also be run else the test will flag an error.
//
// AddKnownMemoryLeak(leaksize, Qty) is probably only ever going to
// have 1 as the qty, and setting KnownMemoryLeakSize has the same effect. The
// call is included in case a substitute for FastMM is used and makes use of
// individual leak tracking. We just use FastMM in a bulk leak detector mode.
//
// Known leaks caused by differing compilers (D5..D2005).
// There are a number of Delphi "components" which are known to leak memory and
// their presence changes between compiler versions. Because DUnit leak
// detection is performed on a differential basis, unless that leaky code is
// invoked and destroyed within the Test procedure then it will have no impact.
// If necessary {$ifdef verXXX} {$endif} type constructs can be used to set
// AllowedLeakSize() sizes according to the compiler in operation.
//
// FastMM was designed as a highly efficient memory manager and the fact we can
// utilise it for "on-the-fly" memory leak detection is a bonus. So the
// following is not a criticism of it's design, just our work-around use of it.
// A limitation imposed by fixed blocks is that it may not be possible to
// pre-calculate the memory allowance required with absolute certainty.
// So the technique for setting the value is to allow the leak size to be
// reported, then register the size with AllowedLeakSize() in the individual
// test code.
//
// Other circumstances where Leaksizes vary if string lengths change naturally.
// For instance if the current date is reported and just a one byte difference
// occurs it may force a string to be allocated to a smaller or larger block.
// It is preferable to write test code so that code under test does't dont leak
// in the first place, which is why this new code exists for DUnit.
// Enjoy. (Peter McNab)

interface
uses
  TestFramework,
  SysUtils;

type

  TTestLeak = class(TTestCase)
  published
    procedure TestCaseFailsNoLeaks;
    procedure TestCaseNoLeaks1;
    procedure TestCaseLeaksObject;
    procedure TestCaseNoLeaks2;
    procedure TestCaseFailsAndLeaks;
    procedure TestCaseNoLeaks3;
    procedure TestCaseLeaksObjectErrorSurpressed;
    procedure TestCaseNoLeaks4;
    procedure TestUnRaisedException;
    procedure TestCaseNoLeaks5;
    procedure TestCaseLeaksArray;
    procedure TestCaseNoLeaks6;
    procedure TestCaseLeaksMemory;
    procedure TestCaseNoLeaks7;
    procedure TestCaseLeaksString0;
    procedure TestCaseNoLeaks8;
    procedure TestCaseLeaksString1;
    procedure TestCaseNoLeaks9;
    procedure TestCaseNoLeaks10;
    procedure TestEmptyReportSuppressed;
    procedure TestEmptyAndReportSo;
  end;

  TTestSetUpLeaks = class(TTestCase)
  public
    procedure SetUp; override;
  published
    procedure TestCaseFreesSetUp1;
    procedure TestCaseLeaksObject;
    procedure TestCaseFreesSetUp2;
  end;

  TTestTearDownLeaks = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaseNoLeaks1;
    procedure TestCaseTearDownLeaks;
    procedure TestCaseNoLeaks2;
  end;

  TTestLeakOfSizeAllowed = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaseNoLeaks1;
    procedure TestCaseLeakOfSizeAllowed;
    procedure TestCaseNoLeaks2;
  end;
  TTestLeakOfDifferentSizeAllowed = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaseNoLeaks1;
    procedure TestCaseLeakOfDifferentSizeAllowed;
    procedure TestCaseNoLeaks2;
  end;
var
  LeakedObject: TObject = nil;
  Excpt: EAbort;
  LeakyArray : array of Byte;
  LeakyString : string;
  LeakyMemory : PChar;

  procedure ClearVars;

implementation

procedure ClearVars;
begin
  SetLength(LeakyArray,0);
  LeakyArray := nil;
  LeakyString := '';
  SetLength(LeakyString, 0);
  FreeAndNil(LeakedObject);
  if (LeakyMemory <> nil) then
  try
    FreeMem(LeakyMemory);
    LeakyMemory := nil;
  except
    LeakyMemory := nil;
  end;

  try
    if Assigned(Excpt) then
      raise excpt;
  except
    Excpt := nil;
  end;
end;

{ TTestLeak }

procedure TTestLeak.TestCaseFailsNoLeaks;
begin
  Check(False, 'Deliberate fail no leaks');
end;

procedure TTestLeak.TestCaseFailsAndLeaks;
begin
  LeakedObject := TObject.Create;
  Check(Assigned(LeakedObject), 'Failed to Create Object');
  Check(False, 'Deliberate fail shows failures take precendence over leak report');
end;

procedure TTestLeak.TestCaseNoLeaks1;
begin
  try
    LeakedObject := TObject.Create;
    Check(Assigned(LeakedObject), 'Golly, Failed to Create Object');
  finally
    FreeAndNil(LeakedObject);
  end;
end;

procedure TTestLeak.TestCaseNoLeaks2;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks3;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks4;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks5;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks6;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks7;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks8;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseNoLeaks9;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestCaseLeaksObjectErrorSurpressed;
begin
  LeakedObject := TObject.Create;
  Check(Assigned(LeakedObject), 'Failed to Create Object');
// Override the GUI setting to prevent Memory leak from failing testcase.
  FailsOnMemoryLeak := False;
end;

procedure TTestLeak.TestCaseLeaksObject;
begin
  LeakedObject := TObject.Create;
  Check(Assigned(LeakedObject), 'Failed to Create Object');
end;

procedure TTestLeak.TestUnRaisedException;
begin
  Excpt := EAbort.Create('Testing');
  Check(Excpt.Message = 'Testing', 'Exception Failed to Create');
end;

procedure TTestLeak.TestCaseLeaksArray;
begin
  SetLength(LeakyArray, 4);
  Check(Sizeof(LeakyArray) = 4,'Leaky Array size error = ' + IntToStr(Sizeof(LeakyArray)));
end;

procedure TTestLeak.TestCaseLeaksMemory;
begin
  GetMem(LeakyMemory, 4);
  Check(Sizeof(LeakyMemory) = 4,'Leaky Memory size error = ' + IntToStr(Sizeof(LeakyMemory)));
end;

procedure TTestLeak.TestCaseLeaksString0;
begin
  SetLength(LeakyString, 0);
  Check(Sizeof(LeakyString) = 4,' 0 Leaky String size error = ' + IntToStr(Sizeof(LeakyString)));
end;

procedure TTestLeak.TestCaseLeaksString1;
begin
  SetLength(LeakyString, 1);
  Check(Sizeof(LeakyString) = 4,' 1 Leaky String size error = ' + IntToStr(Sizeof(LeakyString)));
end;

procedure TTestLeak.TestCaseNoLeaks10;
begin
  TestCaseNoLeaks1;
end;

procedure TTestLeak.TestEmptyReportSuppressed;
begin
  FailsOnNoChecksExecuted := False;
end;

procedure TTestLeak.TestEmptyAndReportSo;
begin
  FailsOnNoChecksExecuted := True;
end;

{ TTestSetUpLeaks }

procedure TTestSetUpLeaks.SetUp;
begin
  inherited;
  LeakedObject := TObject.Create;
end;

procedure TTestSetUpLeaks.TestCaseFreesSetUp1;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
  FreeAndNil(LeakedObject);
end;

procedure TTestSetUpLeaks.TestCaseFreesSetUp2;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
  FreeAndNil(LeakedObject);
end;

procedure TTestSetUpLeaks.TestCaseLeaksObject;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
end;

{ TTestTearDownLeaks }

procedure TTestTearDownLeaks.SetUp;
begin
  inherited;
  LeakedObject := TObject.Create;
end;

procedure TTestTearDownLeaks.TearDown;
begin
  inherited;
end;

procedure TTestTearDownLeaks.TestCaseNoLeaks1;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
  FreeAndNil(LeakedObject);
end;

procedure TTestTearDownLeaks.TestCaseNoLeaks2;
begin
  TestCaseNoLeaks1;
end;

procedure TTestTearDownLeaks.TestCaseTearDownLeaks;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
end;

{ TTestLeakOfSizeAllowed }

procedure TTestLeakOfSizeAllowed.SetUp;
begin
  inherited;
  LeakedObject := TObject.Create;
end;

procedure TTestLeakOfSizeAllowed.TearDown;
begin
  inherited;
end;

procedure TTestLeakOfSizeAllowed.TestCaseLeakOfSizeAllowed;
begin
  AllowedMemoryLeakSize := 16;
  Check(Assigned(LeakedObject), 'Failed to Create Object');
end;

procedure TTestLeakOfSizeAllowed.TestCaseNoLeaks1;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
  FreeAndNil(LeakedObject);
end;

procedure TTestLeakOfSizeAllowed.TestCaseNoLeaks2;
begin
  TestCaseNoLeaks1;
end;

{ TTestLeakOfDifferentSizeAllowed }

procedure TTestLeakOfDifferentSizeAllowed.SetUp;
begin
  inherited;
  LeakedObject := TObject.Create;
end;

procedure TTestLeakOfDifferentSizeAllowed.TearDown;
begin
  inherited;

end;

procedure TTestLeakOfDifferentSizeAllowed.TestCaseLeakOfDifferentSizeAllowed;
begin
  AllowedMemoryLeakSize := 1;  // Will always fail because min size is 12 bytes
  Check(Assigned(LeakedObject), 'Failed to Create Object');
end;

procedure TTestLeakOfDifferentSizeAllowed.TestCaseNoLeaks1;
begin
  Check(Assigned(LeakedObject), 'Failed to Create Object');
  FreeAndNil(LeakedObject);
end;

procedure TTestLeakOfDifferentSizeAllowed.TestCaseNoLeaks2;
begin
  TestCaseNoLeaks1;
end;

initialization
  Excpt := nil;
  LeakyMemory := nil;
  SetLength(LeakyString,0);


end.
