unit Unit1Test;

interface

uses
  SysUtils, Classes, TestFramework, Unit1;

type
  ExceptionClass = class of Exception;
  TTestMethod = procedure of object;

  { by chrismo -- test cases that trap for specific exception classes have
    a lot of duplication. This test case attempts to refactor the common code
    into a CheckException method. But it's awkward to use. }
  // refactoring out calls to trap specific exception classes
  TTestMyObject = class(TTestCase)
  private
    FMyObject: TMyObject;
    FTestResult: boolean;
    FTestString: string;
  protected
    procedure CallStrToIntIsZero;
    procedure CheckException(AMethod: TTestMethod;
      AExceptionClass: ExceptionClass);
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure testMyObject;
    procedure TestEMyObject;
    procedure TestStrToIntIsZero;
  end;

  { majohnson replies with a TTestCase that overrides RunTest ... essentially
    putting the functionality of chrismo's CheckException method into
    RunTest. Simpler, cleaner, easy to use. }
  TTestMyObjectOverrideRunTest = class(TTestCase)
  private
    FExpectedException: ExceptionClass;
    FMyObject: TMyObject;
  public
    procedure SetExpectedException(Value :ExceptionClass);
    procedure RunTest(testResult :TTestResult); override;
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestStrToIntIsZero;
  end;

  function Suite: ITestSuite;

implementation

function Suite: ITestSuite;
begin
  Result := TestSuite('Test MyObject',
                  [TTestMyObject.Suite,
                   TTestMyObjectOverrideRunTest.Suite]);
end;

{ TTestMyObject }

procedure TTestMyObject.CallStrToIntIsZero;
begin
  FTestResult := FMyObject.StrToIntIsZero(FTestString);
end;

procedure TTestMyObject.CheckException(AMethod: TTestMethod;
  AExceptionClass: ExceptionClass);
begin
  try
    AMethod;
    fail('Expected exception not raised'); 
  except
    on E: Exception do
    begin
      if E.ClassType <> AExceptionClass then
        raise;
    end
  end;
end;

procedure TTestMyObject.Setup;
begin
  FMyObject := TMyObject.Create;
end;

procedure TTestMyObject.TearDown;
begin
  FMyObject.Free;
end;

procedure TTestMyObject.TestEMyObject;
begin
  CheckException(FMyObject.RandomException, EMyObject);
end;

procedure TTestMyObject.testMyObject;
begin
  try
    FMyObject.DoSomething;
  except
    assert(false);
  end;
end;

procedure TTestMyObject.TestStrToIntIsZero;
begin
  FTestString := 'blah';
  CheckException(CallStrToIntIsZero, EConvertError);
end;

{ TTestMyObjectOverrideRunTest }

procedure TTestMyObjectOverrideRunTest.RunTest(testResult :TTestResult);
begin
  try
    inherited runTest(testResult);
    if FExpectedException <> nil then
      fail('Excepted Exception did not occur');
  except
     on E: Exception do
     begin
       if FExpectedException = nil then
         raise
       else
         if E.ClassType <> FExpectedException then
           raise;
     end;
  end;
  { clear the exception until the next test registers an Exception }
  FExpectedException := nil;
end;

procedure TTestMyObjectOverrideRunTest.SetExpectedException(
  Value: ExceptionClass);
begin
  FExpectedException := Value
end;

procedure TTestMyObjectOverrideRunTest.Setup;
begin
  FMyObject := TMyObject.Create;
end;

procedure TTestMyObjectOverrideRunTest.TearDown;
begin
  FMyObject.Free;
end;

procedure TTestMyObjectOverrideRunTest.TestStrToIntIsZero;
begin
  SetExpectedException(EConvertError);
  FMyObject.StrToIntIsZero('blah');
end;

end.

