unit uArgumentsTest;

interface

uses
  TestFrameWork,
  uArgumentsForTest;

type
  TArgumentsTest = class(TTestCase)
  private
    Arguments: TArgumentsForTest;
    procedure CheckResult;
  published
    procedure Test;
    procedure TestShowRequired;
    procedure TestShowUnused;
  end;

implementation

uses
  SysUtils,
  uStrings,
  uEParseError;

{ TArgumentsTest }

procedure TArgumentsTest.CheckResult;
begin
  CheckEquals(True, Arguments.Replace.Exists);
  CheckEquals(True, Arguments.D1.Exists);
  CheckEquals(10, Arguments.NumericArgument.Value);

  CheckEquals('a', Arguments.SA.Values[0]);
  CheckEquals('b', Arguments.SA.Values[1]);
  CheckEquals('c', Arguments.SA.Values[2]);

  CheckEquals(1, Arguments.NA.Values[0]);
  CheckEquals(2, Arguments.NA.Values[1]);
  CheckEquals(3, Arguments.NA.Values[2]);

  CheckEquals(False, Arguments.D2.Exists);
  CheckEquals(False, Arguments.Source.Exists);
  CheckEquals(False, Arguments.Dir.Exists);
end;

procedure TArgumentsTest.Test;
begin
  Arguments := TArgumentsForTest.Create;
  try
    Arguments.Parse('');
    // OK
    Arguments.Clear;
    Arguments.Parse('"*.exe" -r -d1 -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;

    // check "  " (double space) in command line
    Arguments.Clear;
    Arguments.Parse('"*.exe" -r  -d1   -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;

    // check " " in start command line
    Arguments.Clear;
    Arguments.Parse('"*.exe"  -r  -d1   -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;

    Arguments.Clear;
    Arguments.Parse('"*.exe" -r -d1 -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;
    CheckEquals('Argument "number" requires argument "d"' + LineSep, Arguments.Check);
    CheckEquals(10, Arguments.NumericArgument.Value);

    Arguments.Clear;
    Arguments.Parse('"*.exe" -r -d1 -number:10 -sa a;b;c -na 1;2;3');
    CheckResult;
    CheckEquals(10, Arguments.NumericArgument.Value);

    Arguments.Clear;
    Arguments.Parse('"*.exe" -r -d1 -number:10 -d C:\Test -na 1;2;3');
    CheckEquals('C:\Test\', Arguments.Dir.Value);

    Arguments.Clear;
    Arguments.Parse('"*.exe" -r -d1 -number:10 -d  C:\Test -na 1;2;3');
    CheckEquals('C:\Test\', Arguments.Dir.Value);

    Arguments.Clear;
    Arguments.Parse('"*.exe" -d "1" -r  -d1   -number 10 -sa a;b;c -na 1;2;3');
    CheckEquals(CorrectDirF(GetCurrentDir) + '1\', Arguments.Dir.Value);
    CheckEquals('', Arguments.Check);

    // Swith parameter with false value
    Arguments.Clear;
    Arguments.Parse('"*.exe" -r 0 -d1 -number 10 -sa a;b;c -na 1;2;3');
    CheckEquals(False, Arguments.Replace.Value);
    CheckResult;

    // Swith parameter with true value
    Arguments.Clear;
    Arguments.Parse('"*.exe" -r 1 -d1 -number 10 -sa a;b;c -na 1;2;3');
    CheckEquals(True, Arguments.Replace.Value);
    CheckResult;

    // Swith parameter with invalid value
    Arguments.Clear;
    StartExpectingException(EParseError);
    Arguments.Parse('"*.exe" -r invalid -d1 -number 10 -sa a;b;c -na 1;2;3');
    StopExpectingException;

    // Combo Argument
    CheckEquals(-1, Arguments.Combo.Value);
    Arguments.Clear;
    CheckEquals(-1, Arguments.Combo.Value);
    Arguments.Parse('"*.exe" -combo second');
    CheckEquals(True, Arguments.Combo.Exists);
    CheckEquals(1, Arguments.Combo.Value);
    Arguments.Parse('"*.exe" -combo first');
    CheckEquals(0, Arguments.Combo.Value);

    // Swith argument with invalid value
    Arguments.Clear;
    StartExpectingException(EParseError);
    Arguments.Parse('"*.exe" -r invalid -d1 -number 10 -sa a;b;c -na 1;2;3');
    StopExpectingException;

    // Time argument
    Arguments.Clear;
    Arguments.Parse('"*.exe" -time 0:02:03.536');
    CheckEquals(123536, Arguments.Time.Value.Milliseconds);
  finally
    Arguments.Free;
  end;
end;

procedure TArgumentsTest.TestShowRequired;
var
  Required: string;
begin
  Arguments := TArgumentsForTest.Create;
  try
    Arguments.Parse('');
    Required := Arguments.ShowRequired;
    CheckTrue(Required <> '');
  finally
    Arguments.Free;
  end;
end;

procedure TArgumentsTest.TestShowUnused;
var
  Unused: string;
begin
  Arguments := TArgumentsForTest.Create;
  try
    Arguments.Parse('');
    Unused := Arguments.ShowUnused;
    CheckTrue(Unused <> '');
  finally
    Arguments.Free;
  end;
end;

initialization
	RegisterTest('Arguments Test', TArgumentsTest.Suite);
end.
