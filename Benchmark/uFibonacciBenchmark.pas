unit uFibonacciBenchmark;

interface

uses
  uTypes,
  uProjectVersion,
  uBenchmark;

type
  TFibonacciBenchmark = class(TBenchmark)
  private
    FNumber: SG;
    procedure SetNumber(const Value: SG);
  protected
    function GetName: string; override;
    function GetVersion: TProjectVersion; override;
  public
    procedure Execute; override;

    property Number: SG read FNumber write SetNumber;
  end;

implementation

uses
  SysUtils;

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
    Fibonacci(FNumber);
    Inc(i);
  end;
  CalculatedItems := i;
end;

function TFibonacciBenchmark.GetName: string;
begin
  Result := 'Fibonnacci ' + IntToStr(FNumber);
end;

function TFibonacciBenchmark.GetVersion: TProjectVersion;
begin
  Result.Major := 1;
  Result.Minor := 1;
  Result.Release := 0;
  Result.Build := 0;
end;

procedure TFibonacciBenchmark.SetNumber(const Value: SG);
begin
  FNumber := Value;
end;

end.
