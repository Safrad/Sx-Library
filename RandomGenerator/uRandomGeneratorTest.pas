unit uRandomGeneratorTest;

interface

uses
  uTypes,
  uRandomGenerator,
  TestFrameWork;

type
  TRandomGeneratorTest = class(TTestCase)
  private
    const
      LoopCount = 20000000; // Depends on CPU speed
    procedure TestSpecific(const ARandomGenerator: TRandomGenerator);
    procedure TestImbalancedEvenOdd(const ARandomGenerator: TRandomGenerator);
    procedure TestImbalancedLowHigh(const ARandomGenerator: TRandomGenerator);
    procedure TestImbalancedLowHighU8(const ARandomGenerator: TRandomGenerator);
    procedure TestImbalancedLowHighU82(const ARandomGenerator: TRandomGenerator);
    procedure TestRandomZeroDistanceValue(const AValue: SG; const AMinimum, AMaximum: SG);
  published
    procedure TestDelphiRandomGeneratorCompatibility;
    procedure TestDelphiGenerator;
    procedure TestBorlandCRandomGenerator;
    procedure TestGNUCRandomGenerator;
    procedure TestNumericalRecepiesRandomGenerator;
    procedure TestRandom0Generator;
    procedure TestKnuthRandomGenerator;
    procedure TestSxRandomGenerator;
    procedure TestRandomZeroDistance;
  end;

implementation

uses
  Threading,
  uMath,
  uOutputFormat,
  uDelphiRandomGenerator,
  uBorlandCRandomGenerator,
  uGNUCRandomGenerator,
  uNumericalRecepiesRandomGenerator,
  uRandom0Generator,
  uKnuthRandomGenerator,
  uSxRandomGenerator;

{ TRandomGeneratorTest }

procedure TRandomGeneratorTest.TestDelphiRandomGeneratorCompatibility;
const
  MaxValue = 999;
var
  i: SG;
  DelphiRandomGenerator: TDelphiRandomGenerator;
  Expected, Actual: U4;
begin
  Randomize;
  DelphiRandomGenerator := TDelphiRandomGenerator.Create;
  try
    DelphiRandomGenerator.Seed := U8(RandSeed);
    for i := 0 to LoopCount - 1 do
    begin
      Expected := Random(MaxValue + 1);
      Actual := DelphiRandomGenerator.RangeU4(MaxValue);

      CheckEquals(Expected, Actual);
    end;
  finally
    DelphiRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestBorlandCRandomGenerator;
var
  BorlandCRandomGenerator: TBorlandCRandomGenerator;
begin
  BorlandCRandomGenerator := TBorlandCRandomGenerator.Create;
  try
    TestSpecific(BorlandCRandomGenerator);
  finally
    BorlandCRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestGNUCRandomGenerator;
var
  GNUCRandomGenerator: TGNUCRandomGenerator;
begin
  GNUCRandomGenerator := TGNUCRandomGenerator.Create;
  try
    TestSpecific(GNUCRandomGenerator);
  finally
    GNUCRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestDelphiGenerator;
var
  DelphiRandomGenerator: TDelphiRandomGenerator;
begin
  DelphiRandomGenerator := TDelphiRandomGenerator.Create;
  try
    TestSpecific(DelphiRandomGenerator);
  finally
    DelphiRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestRandom0Generator;
var
  Random0Generator: TRandom0Generator;
begin
  Random0Generator := TRandom0Generator.Create;
  try
    TestSpecific(Random0Generator);
  finally
    Random0Generator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestRandomZeroDistance;
begin
  TestRandomZeroDistanceValue(0, 0, 0);
  TestRandomZeroDistanceValue(1, -1, 1);
  TestRandomZeroDistanceValue(2, -2, 2);
  TestRandomZeroDistanceValue(1000, -1000, 1000);
end;

procedure TRandomGeneratorTest.TestRandomZeroDistanceValue(const AValue, AMinimum, AMaximum: SG);
const
  Count = 10000;
var
  Value: SG;
  i: SG;
  Minimum, Maximum, Suma: SG;
  SxRandomGenerator: TSxRandomGenerator;
begin
  SxRandomGenerator := TSxRandomGenerator.Create;
  try
    Minimum := High(Minimum);
    Maximum := Low(Minimum);
    Suma := 0;
    for i := 0 to Count - 1 do
    begin
      Value := SxRandomGenerator.RandomZeroDistance(AValue);
      Inc(Suma, Value);
      if Value < Minimum then
        Minimum := Value;
      if Value > Maximum then
        Maximum := Value;
    end;
    CheckTrue(Abs(Suma) <= AValue * Count div 20, 'RandomZeroDistance is imbalanced');
    CheckEquals(AMinimum, Minimum);
    CheckEquals(AMaximum, Maximum);
  finally
    SxRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestImbalancedEvenOdd(const ARandomGenerator: TRandomGenerator);
const
  cValueRange = 10000;
var
  i: SG;
  Value: U4;
  EvenCount, OddCount: U8;
  RelativeError: FG;
begin
  EvenCount := 0;
  OddCount := 0;
  for i := 0 to LoopCount - 1 do
  begin
    Value := ARandomGenerator.RangeU4(cValueRange - 1);
    if Value and 1 = 0 then
      Inc(EvenCount)
    else
      Inc(OddCount);
  end;
  RelativeError := GetRelativeError(EvenCount, LoopCount div 2);
  CheckTrue(RelativeError <= 0.001 {0.1%},
    'Imbalanced even/odd (' + NToS(EvenCount) + '/' + NToS(OddCount) + ') numbers.');
end;

procedure TRandomGeneratorTest.TestImbalancedLowHigh(const ARandomGenerator: TRandomGenerator);
const
  cValueRange = 10000;
var
  i: SG;
  Value: U4;
  LowCount, HighCount: U8;
  RelativeError: FG;
  Precision: FG;
begin
  LowCount := 0;
  HighCount := 0;
  for i := 0 to LoopCount - 1 do
  begin
    Value := ARandomGenerator.RangeU4(cValueRange - 1);
    if Value < cValueRange div 2 then
      Inc(LowCount)
    else
      Inc(HighCount);
  end;

  RelativeError := GetRelativeError(LowCount, LoopCount div 2);
  if ARandomGenerator is TRandom0Generator then
    Precision := 0.04 {4%}
  else
    Precision := 0.001; {0.1%}

  CheckTrue(RelativeError <= Precision,
    'Imbalanced low/high (' + NToS(LowCount) + '/' + NToS(HighCount) + ') numbers.');
end;

procedure TRandomGeneratorTest.TestImbalancedLowHighU8(const ARandomGenerator: TRandomGenerator);
var
  i: SG;
  Value: U8;
  LowCount, HighCount: U8;
  RelativeError: FG;
  Precision: FG;
begin
  LowCount := 0;
  HighCount := 0;
  for i := 0 to LoopCount - 1 do
  begin
    Value := ARandomGenerator.RandomU8;
    if Value < 9223372036854775808 then
      Inc(LowCount)
    else
      Inc(HighCount);
  end;

  RelativeError := GetRelativeError(LowCount, LoopCount div 2);
  if ARandomGenerator is TRandom0Generator then
    Precision := 0.04 {4%}
  else
    Precision := 0.001; {0.1%}

  CheckTrue(RelativeError <= Precision,
    'Imbalanced low/high (' + NToS(LowCount) + '/' + NToS(HighCount) + ') numbers.');
end;

procedure TRandomGeneratorTest.TestImbalancedLowHighU82(const ARandomGenerator: TRandomGenerator);
const
  ValueCount = 1000000000000000000;
var
  i: SG;
  Value: U8;
  LowCount, HighCount: U8;
  RelativeError: FG;
  Precision: FG;
begin
  LowCount := 0;
  HighCount := 0;
  for i := 0 to LoopCount - 1 do
  begin
    Value := ARandomGenerator.RangeU8(ValueCount - 1);
    if Value < ValueCount div 2 then
      Inc(LowCount)
    else
      Inc(HighCount);
  end;

  RelativeError := GetRelativeError(LowCount, LoopCount div 2);
  if ARandomGenerator is TRandom0Generator then
    Precision := 0.04 {4%}
  else
    Precision := 0.001; {0.1%}

  CheckTrue(RelativeError <= Precision,
    'Imbalanced low/high (' + NToS(LowCount) + '/' + NToS(HighCount) + ') numbers.');
end;

procedure TRandomGeneratorTest.TestKnuthRandomGenerator;
var
  KnuthRandomGenerator: TKnutRandomGenerator;
begin
  KnuthRandomGenerator := TKnutRandomGenerator.Create;
  try
    TestSpecific(KnuthRandomGenerator);
  finally
    KnuthRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestNumericalRecepiesRandomGenerator;
var
  NumericalRecepiesRandomGenerator: TNumericalRecepiesRandomGenerator;
begin
  NumericalRecepiesRandomGenerator := TNumericalRecepiesRandomGenerator.Create;
  try
    TestSpecific(NumericalRecepiesRandomGenerator);
  finally
    NumericalRecepiesRandomGenerator.Free;
  end;
end;

procedure TRandomGeneratorTest.TestSpecific(const ARandomGenerator: TRandomGenerator);
begin
  TestImbalancedEvenOdd(ARandomGenerator);
  TestImbalancedLowHigh(ARandomGenerator);
  TestImbalancedLowHighU8(ARandomGenerator);
  TestImbalancedLowHighU82(ARandomGenerator);
end;

procedure TRandomGeneratorTest.TestSxRandomGenerator;
var
  SxRandomGenerator: TSxRandomGenerator;
begin
  SxRandomGenerator := TSxRandomGenerator.Create;
  try
    TestSpecific(SxRandomGenerator);
  finally
    SxRandomGenerator.Free;
  end;
end;

initialization
	RegisterTest('Random Generator Test', TRandomGeneratorTest.Suite);
end.
