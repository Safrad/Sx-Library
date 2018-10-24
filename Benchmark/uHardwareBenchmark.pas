unit uHardwareBenchmark;

interface

uses
  uTypes,
  uSxThread,
  uOutputInfo,
  uBenchmark;

const
  TestCount = 9;

type
  THardwareBenchmark = class(TSxThread)
  private
    FEmptyOutputInfo: TOutputInfo;
    FScores: array[0..TestCount - 1] of FG;
    FResultAsString: string;
    FOutputInfo: TOutputInfo;
    function GetBenchmark(const ATestType: SG): TBenchmark;
    function DoTest(const ATestType: SG; const AThreadCount: SG): FG;
    procedure SetOutputInfo(const Value: TOutputInfo);
    function GetOverallScore: FG;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ResultAsString: string read FResultAsString;
    property OverallScore: FG read GetOverallScore;
    property OutputInfo: TOutputInfo read FOutputInfo write SetOutputInfo;
  end;

implementation

uses
  // TODO : Mathematical operations, compression, encryption, physics
  uFibonacciBenchmark,
  uCalculationBenchmark,
  uMemoryBenchmark,
  uDiskBenchmark,

  Math,
  Windows,
  SysUtils,
  uCPU,
  uUnitFormatter,
  uOperatingSystem,
  uMath,
  uStrings,
  uStopwatch,
  uOutputFormat,
  uFiles;

{ THardwareBenchmark }

function THardwareBenchmark.DoTest(const ATestType: SG; const AThreadCount: SG): FG;
var
  Benchmarks: array of TBenchmark;
  i: SG;
  Performance: FG;
begin
  SetLength(Benchmarks, AThreadCount);
  for i := 0 to AThreadCount - 1 do
  begin
    Benchmarks[i] := GetBenchmark(ATestType);
    case ATestType of
    5..8:
      TDiskBenchmark(Benchmarks[i]).FileName := OperatingSystem.TemporaryDirectory.ProcessTempDir + 'Test' + IntToStr(i) + '.tmp';
    end;
    if ATestType = 5 then
      TDiskBenchmark(Benchmarks[i]).CreateFile;
    if AThreadCount = 1 then
      FResultAsString := FResultAsString + Benchmarks[i].Description + ':' + CharSpace;

    Benchmarks[i].OutputInfo := FEmptyOutputInfo;
    OutputInfo.AddCaption('Running test ' + Benchmarks[i].Description + ' for ' + NToS(AThreadCount) + ' thread(s)…');
    Benchmarks[i].Start;
  end;
  if IsRelease then
    Sleep(700)
  else
    Sleep(350);
  for i := 0 to AThreadCount - 1 do
  begin
    Benchmarks[i].Terminate;
  end;
  Performance := 0;
  for i := 0 to AThreadCount - 1 do
  begin
    Benchmarks[i].WaitFor;
    Increment(Performance, Benchmarks[i].Performace);
    Benchmarks[i].Free;
  end;
  Result := Performance;
end;

function THardwareBenchmark.GetBenchmark(const ATestType: SG): TBenchmark;
begin
  case ATestType of
  0:
  begin
    Result := TFibonacciBenchmark.Create;
    TFibonacciBenchmark(Result).Number := 21;
  end;
  1: Result := TCalculationBenchmark.Create;
  2:
  begin
    Result := TMemoryBenchmark.Create;
    TMemoryBenchmark(Result).BlockSize := 64;
  end;
  3:
  begin
    Result := TMemoryBenchmark.Create;
    TMemoryBenchmark(Result).BlockSize := 64 * KB;
  end;
  4:
  begin
    Result := TMemoryBenchmark.Create;
    TMemoryBenchmark(Result).BlockSize := 64 * MB;
  end;
  5:
  begin
    Result := TDiskBenchmark.Create;
    TDiskBenchmark(Result).Access := daRead;
    TDiskBenchmark(Result).UseBuffer := False; // Required
  end;
  6:
  begin
    Result := TDiskBenchmark.Create;
    TDiskBenchmark(Result).Access := daRead;
    TDiskBenchmark(Result).RandomMode := True;
    TDiskBenchmark(Result).UseBuffer := False; // Required
  end;
  7:
  begin
    Result := TDiskBenchmark.Create;
    TDiskBenchmark(Result).Access := daWrite;
    TDiskBenchmark(Result).UseBuffer := True;
    TDiskBenchmark(Result).WriteThrough := True; // Required
  end;
  8:
  begin
    Result := TDiskBenchmark.Create;
    TDiskBenchmark(Result).Access := daWrite;
    TDiskBenchmark(Result).RandomMode := True;
    TDiskBenchmark(Result).UseBuffer := True;
    TDiskBenchmark(Result).WriteThrough := True; // Required
  end;
  else
    Result := nil;
  end;
//    Benchmarks[i].Priority := tpTimeCritical;
end;

function THardwareBenchmark.GetOverallScore: FG;
var
  Score: FG;
  Count: UG;
  i: SG;
begin
	Score := 1;
	Count := 0;
  for i := 0 to Length(FScores) - 1 do
  begin
    if FScores[i] > 0 then
    begin
      Score := Score * FScores[i];
      Inc(Count);
    end;
  end;
  if Count = 0 then
    Result := 0
  else
    Result := Power(Score, 1 / Count);
end;

procedure THardwareBenchmark.Execute;
var
  TestType, Core: SG;
  Performance, Performance1Core, LastPerformance: FG;
  UnitFormatter: TUnitFormatter;
begin
  Name := 'Hardware Benchmarks';

  inherited;

  UnitFormatter := TUnitFormatter.Create;
  try
    UnitFormatter.UnitName := '';
    UnitFormatter.PrefixType := ptMetric;
    FResultAsString := '';
  //  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
    OutputInfo.ProgressMaximum := TestCount;
    Sleep(100);
    for TestType := 0 to TestCount - 1 do
    begin
      OutputInfo.ProgressValue := TestType + 1;
      LastPerformance := 0;
      Performance1Core := 0;
      Performance := 0;
      for Core := 1 to GCPU.LogicalProcessorCount + 2 do
      begin
        Performance := DoTest(TestType, Core);
        if Core = 1 then
          Performance1Core := Performance;
        if Performance < LastPerformance * 1.05 then
        begin
          if Performance1Core > 0 then
            FResultAsString := FResultAsString + 'Speedup ratio: ' + FloatToStrF(Max(Performance, LastPerformance) / Performance1Core, ffGeneral, 4, 5) + CharTimes;
          Break;
        end;
        FResultAsString := FResultAsString + NToS(Core) + ' thread(s): ' + UnitFormatter.Format(Performance) + ' | ';
  {      if Elapsed.Seconds > 0 then
        begin
          OutputInfo.AddCaption(Translate('Success:') + CharSpace + NToS(CalculatedItems, ofIO) + ' B (' + BToStr(CalculatedItems) + ')');

          OutputInfo.AddCaption(Translate('Time:') + CharSpace + MsToStr(Round(Elapsed.Milliseconds), diSD, 3, False) + CharSpace + Sec);

          BS := Round(CalculatedItems / Elapsed.Seconds);
          s := NToS(BS, ofIO) + ' B/' + Sec + ' (' + BToStr(BS) + '/' + Sec + ')';
          OutputInfo.AddCaption(Translate('Speed:') + CharSpace + s);
        end
        else
          OutputInfo.AddCaption(Translate('Too fast, can not measure.') + CharSpace + s);}

        LastPerformance := Performance;

        if Terminated then
          Break;
      end;
      FScores[TestType] := Performance;
      FResultAsString := FResultAsString + LineSep;
      if Terminated then
      begin
        FResultAsString := FResultAsString + 'Aborted by user!' + LineSep;
        Break;
      end;
    end;
    FResultAsString := FResultAsString + 'Overall mark: ' + UnitFormatter.Format(OverallScore) + LineSep;
  finally
    UnitFormatter.Free;
  end;
//     SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
end;

procedure THardwareBenchmark.SetOutputInfo(const Value: TOutputInfo);
begin
  FOutputInfo := Value;
end;

constructor THardwareBenchmark.Create;
begin
  inherited Create;

  FEmptyOutputInfo := TOutputInfo.Create;
end;

destructor THardwareBenchmark.Destroy;
begin
  FreeAndNil(FEmptyOutputInfo);

  inherited;
end;

end.
