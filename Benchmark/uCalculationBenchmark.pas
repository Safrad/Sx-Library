unit uCalculationBenchmark;

interface

uses
  uProjectVersion,
  uBenchmark;

type
  TCalculationBenchmark = class(TBenchmark)
  protected
    function GetName: string; override;
    function GetVersion: TProjectVersion; override;
  public
    constructor Create;
    procedure Execute; override;
  end;

implementation

uses
  uTypes,
  uInputFormat,
  uDParser,
  uLapStopwatch;

{ TCalculationBenchmark }

constructor TCalculationBenchmark.Create;
begin
  inherited Create;
end;

procedure TCalculationBenchmark.Execute;
const
  Input = 'Sin(2) + Cos(4) + 4/7 * Sqrt(7)';
//  Input = '';
var
  LapStopwatch: TLapStopwatch;
  i: SG;
begin
  inherited;

  LapStopwatch := TLapStopwatch.Create;
  i := 0;
  while not Terminated do
  begin
    StrToVector(Input, False, nil, LapStopwatch);
    LapStopwatch.Reset;
    Inc(i);
  end;
  CalculatedItems := i;
  LapStopwatch.Free;
end;

function TCalculationBenchmark.GetName: string;
begin
  Result := 'Calculation';
end;

function TCalculationBenchmark.GetVersion: TProjectVersion;
begin
  Result.Major := 1;
  Result.Minor := 1;
  Result.Release := 0;
  Result.Build := 0;
end;

end.
