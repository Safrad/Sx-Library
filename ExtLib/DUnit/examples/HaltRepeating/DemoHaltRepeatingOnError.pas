{ $Id: DemoHaltRepeatingOnError.pas 7 2008-04-24 11:59:47Z judc $ }
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
 
unit DemoHaltRepeatingOnError;

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

  TTestStubTest = class(TTestCase)
  private
    FTestResult: TTestResult;
    FTestStub: ITestStub;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestRepeatedTest = class(TTestStubTest)
  private
    FIterations: integer;
    FRepTest: ITest;
  public
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
  public
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
  public
    procedure SetUp; override;
  published
{$IFDEF CLR}[Test]{$ENDIF}
    procedure CountTestFails; virtual;
  end;

implementation

uses
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
  CountCase.HaltOnError := True;    {******** Example use of property *********}
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
   Check(False, 'Forced Fail to halt repetition');  {******* Test fails *********}
end;

procedure TCountCaseFails.SetUp;
begin
  FCounter := 0;
end;

initialization
  RegisterTests('TestExtensions Suite',[ TTestRepeatedTest.Suite]);

end.

