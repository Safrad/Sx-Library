unit uFibonacciBenchmark;

interface

uses
  uProjectVersion,
  uBenchmark;

type
  TFibonacciBenchmark = class(TBenchmark)
  protected
    function GetVersion: TProjectVersion; override;
  public
    procedure Execute; override;
  end;

implementation

uses
  uTypes;

{ TODO :
function Fibonacci(aNumber: Integer): Integer;
begin
  if aNumber < 0 then
    raise Exception.Create('The Fibonacci sequence is not defined for negative integers.');

  case aNumber of
  0: Result:= 0;
  1: Result:= 1;
  else
    Result:= Fibonacci(aNumber - 1) + Fibonacci(aNumber - 2);
  end;
end;
}

function Fibonacci(const aNumber: Integer): Integer;
begin
  if aNumber <= 1 then
    Result := aNumber
  else
    Result := Fibonacci(aNumber - 1) + Fibonacci(aNumber - 2);
end;

{ TFibonacciBenchmark }

procedure TFibonacciBenchmark.Execute;
var
  i: SG;
begin
  inherited;

  i := 0;
  while not Terminated do
  begin
    Fibonacci(21);
    Inc(i);
  end;
  CalculatedItems := i;
end;

function TFibonacciBenchmark.GetVersion: TProjectVersion;
begin
  Result.Major := 1;
  Result.Minor := 1;
  Result.Release := 0;
  Result.Build := 0;
end;

end.
