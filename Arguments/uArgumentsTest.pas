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
  uStrings;

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
    Arguments.Parse('-r -d1 -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;

    // check "  " (double space) in command line
    Arguments.Parse('-r  -d1   -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;

    // check " " in start command line
    Arguments.Parse(' -r  -d1   -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;

    Arguments.Parse('-r -d1 -number 10 -sa a;b;c -na 1;2;3');
    CheckResult;
    CheckEquals('Argument "number" requires argument "d"' + LineSep, Arguments.Check);

    Arguments.Parse('-d "1" -r  -d1   -number 10 -sa a;b;c -na 1;2;3');
    CheckEquals('"1"', Arguments.Dir.Value);
    CheckEquals('', Arguments.Check);
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
