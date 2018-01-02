unit uHardwareBenchmark;

interface

uses
  Classes,
  uTypes,
  uOutputInfo,
  uBenchmark;

type
  THardwareBenchmark = class
  private
    FResultAsString: string;
    FOutputInfo: TOutputInfo;
    function DoTest(const ATestType: SG; const AThreadCount: SG): FG;
    procedure SetOutputInfo(const Value: TOutputInfo);

  public
    procedure Run;

    property ResultAsString: string read FResultAsString;
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
  uUnitFormatter,
  uMath,
  uStrings,
  uNewThread,
  uStopwatch,
  uOutputFormat;

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
    case ATestType of
    0: Benchmarks[i] := TFibonacciBenchmark.Create;
    1: Benchmarks[i] := TCalculationBenchmark.Create;
    2:
    begin
      Benchmarks[i] := TMemoryBenchmark.Create;
      TMemoryBenchmark(Benchmarks[i]).BlockSize := 64;
    end;
    3:
    begin
      Benchmarks[i] := TMemoryBenchmark.Create;
      TMemoryBenchmark(Benchmarks[i]).BlockSize := 64 * KB;
    end;
    4:
    begin
      Benchmarks[i] := TMemoryBenchmark.Create;
      TMemoryBenchmark(Benchmarks[i]).BlockSize := 64 * MB;
    end;
    5: Benchmarks[i] := TDiskBenchmark.Create;
    end;
//    Benchmarks[i].Priority := tpTimeCritical;
    Benchmarks[i].OutputInfo := OutputInfo;
    Benchmarks[i].Start;
  end;
  Sleep(500);
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

procedure THardwareBenchmark.Run;
var
  TestType, Core: SG;
  Performance, Performance1Core, LastPerformance: FG;
  UnitFormatter: TUnitFormatter;
begin
  UnitFormatter := TUnitFormatter.Create;
  UnitFormatter.UnitName := '';
  UnitFormatter.PrefixType := ptMetric;
  FResultAsString := '';
//  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  for TestType := 0 to 5 do
  begin
    FResultAsString := FResultAsString + 'Test' + CharSpace + IntToStr(TestType) + ':' + CharSpace;
    LastPerformance := 0;
    Performance1Core := 0;
    for Core := 1 to 64 do //64 do // GSysInfo.LogicalProcessorCount  do
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
      FResultAsString := FResultAsString + NToS(Core) + ' thread(s): ' + UnitFormatter.Format(Performance) + '; ';
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
    end;
    FResultAsString := FResultAsString + LineSep;
  end;
  UnitFormatter.Free;
//     SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
end;

procedure THardwareBenchmark.SetOutputInfo(const Value: TOutputInfo);
begin
  FOutputInfo := Value;
end;

end.
