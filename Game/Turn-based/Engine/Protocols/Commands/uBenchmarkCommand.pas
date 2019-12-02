unit uBenchmarkCommand;

interface

uses
  uMoveTimeLevel,
  uEngineCommand;

type
  TBenchmarkCommand = class(TEngineCommand)
  private
    FTimeLevel: TMoveTimeLevel;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,

  uTypes,
  uMath,
  uEParseError,
  uTimeSpan,

  uCustomLevel,
  uOutputFormat;

{ TBenchmarkCommand }

constructor TBenchmarkCommand.Create;
begin
  inherited;

  Description := 'Benchmark engine speed on current computer.';
  FTimeLevel := TMoveTimeLevel.Create;
  FTimeLevel.ValueType := lvtEqual;
end;

destructor TBenchmarkCommand.Destroy;
begin
  try
    FTimeLevel.Free;
  finally
    inherited;
  end;
end;

procedure TBenchmarkCommand.Execute(const AParameters: string);
const
  TotalTime = 15; // seconds

  TestPositionCount = 5;
  TestPositions: array[0..TestPositionCount - 1] of string = (
    'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1', // Start position
    'rnbqkbnr/qqqqqqqq/8/8/8/8/QQQQQQQQ/RNBQKBNR w KQkq - 0 1', // Open position
    'rnbqkbnr/8/1p1p1p1p/p1p1p1p1/P1P1P1P1/1P1P1P1P/8/RNBQKBNR w KQkq - 0 1', // Closed position
    '5rk1/2bpp3/b2n1p2/8/8/3B4/PPPPP1Q1/2KRR3 b - - 0 1', // Attack king position
    '4k3/3pp3/8/8/8/8/3PP3/4K3 w - - 0 1' // Endgame position
  );
var
  i: SG;
  TotalNodes: U8;
  RealTotalTime: TTimeSpan;
begin
  inherited;

  if LowerCase(AParameters) = 'basic' then
  begin
    InternalEngine.Output.TellGUIInfo('Testing, please wait ' + IntToStr(TotalTime) + ' seconds…');
    FTimeLevel.Value.SecondsAsF := TotalTime / TestPositionCount;
    FTimeLevel.AnalysisInfo := InternalEngine.AnalysisInfo;
    InternalEngine.LevelManager.MyLevel := FTimeLevel;
    InternalEngine.LevelManager.InfiniteAnalysis := False;
    TotalNodes := 0;
    RealTotalTime.Ticks := 0;
    for i := Low(TestPositions) to High(TestPositions) do
    begin
      InternalEngine.SetPositionFromString(TestPositions[i]);
      InternalEngine.Start;
      InternalEngine.WaitForCalculationDone(FTimeLevel.Value.Milliseconds + 1000);
      InternalEngine.Output.TellGUIInfo('Bechmark score ' + IntToStr(i + 1) + ': ' + NToS(Round(InternalEngine.AnalysisInfo.Nodes / InternalEngine.AnalysisInfo.ElapsedTime.Elapsed.SecondsAsF)));

      Inc(TotalNodes, InternalEngine.AnalysisInfo.Nodes);
      RealTotalTime.Ticks := RealTotalTime.Ticks + InternalEngine.AnalysisInfo.ElapsedTime.Elapsed.Ticks;
    end;
    InternalEngine.Output.TellGUIInfo('Total bechmark score: ' + NToS(Round(TotalNodes / RealTotalTime.SecondsAsF)));
  end
  else
    raise EParseError.Create(['basic'], AParameters);
end;

function TBenchmarkCommand.GetSyntax: string;
begin
  Result := '[basic]';
end;

end.
