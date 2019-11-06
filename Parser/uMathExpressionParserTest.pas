unit uMathExpressionParserTest;

interface

uses
  uTypes,
	SysUtils,
  TestFrameWork;

type
  TMathExpressionParserTest = class(TTestCase)
  private
    FFailCount: UG;
    FTestCount: UG;
    function SameResult(AExpected, AActual: string): BG;
    procedure TestExpression(const AExpression: string; const AExpectedResult: string; const AExpectedErrorCount: SG);
    procedure TestFile(const AFileName: TFileName);
  published
    procedure TestEmptyInput;
    procedure TestErrors;
    procedure TestNumbers;
    procedure TestConstants;
    procedure TestComparison;
    procedure TestRounding;
    procedure TestOperators;
    procedure TestLogic;
    procedure TestBasicMath;
    procedure TestAdvancedMath;
    procedure TestGoniometric;
    procedure TestHyperbolic;
    procedure TestPhysics;
    procedure TestELO;
  end;

implementation

uses
  uStrings,
  uMathExpressionParser, uParserMsg,
  uExpressionTreeEvaluator,
  uSxStringParser,
	uFiles, uCSVFile, uInputFormat, uOutputFormat, uVector;

{ TMathExpressionParserTest }

function TMathExpressionParserTest.SameResult(AExpected, AActual: string): BG;
begin
  if LastChar(AExpected) = '…' then
    Result := StartStr(DelLastChar(AExpected), AActual)
  else
    Result := AExpected = AActual;
end;

procedure TMathExpressionParserTest.TestFile(const AFileName: TFileName);
var
	CSVFile: TCSVFile;
	Line: TArrayOfString;
  ExpectedErrorCount: UG;
begin
	FFailCount := 0;
	FTestCount := 0;

	CSVFile := TCSVFile.Create;
	try
    CSVFile.SetColumnNames(['Expression', 'Result', 'Errors']);
		if CSVFile.Open(DataDir + 'Parser\' + AFileName) then
		begin
			while not CSVFile.EOF do
			begin
				Line := CSVFile.ReadLine;
        if Length(Line) < 2 then
          raise Exception.Create('Invalid CSV line length.');
        if Length(Line) <= 2 then
          ExpectedErrorCount := 0
        else
          ExpectedErrorCount := StrToValI(Line[2], False, 0, 0, MaxInt, 1);
        TestExpression(Line[0], Line[1], ExpectedErrorCount);
        Inc(FTestCount);
			end;
		end;
		CSVFile.Close;
	finally
		FreeAndNil(CSVFile);
	end;
	if FFailCount <> 0 then
		Check(FFailCount = 0, Format('Fail %s calc tests.', [NToS(FFailCount) + ' / ' + NToS(FTestCount)]));
end;

procedure TMathExpressionParserTest.TestAdvancedMath;
begin
  TestFile('AdvancedMath.csv');
end;

procedure TMathExpressionParserTest.TestBasicMath;
begin
  TestFile('BasicMath.csv');
end;

procedure TMathExpressionParserTest.TestComparison;
begin
  TestFile('Comparison.csv');
end;

procedure TMathExpressionParserTest.TestConstants;
begin
  TestFile('Constants.csv');
end;

procedure TMathExpressionParserTest.TestELO;
begin
  TestFile('ELO.csv');
end;

procedure TMathExpressionParserTest.TestEmptyInput;
begin
  TestFile('EmptyInput.csv');
end;

procedure TMathExpressionParserTest.TestErrors;
begin
  TestFile('Errors.csv');
end;

procedure TMathExpressionParserTest.TestGoniometric;
begin
  TestFile('Goniometric.csv');
end;

procedure TMathExpressionParserTest.TestHyperbolic;
begin
  TestFile('Hyperbolic.csv');
end;

procedure TMathExpressionParserTest.TestLogic;
begin
  TestFile('Logic.csv');
end;

procedure TMathExpressionParserTest.TestNumbers;
begin
  TestFile('Numbers.csv');
end;

procedure TMathExpressionParserTest.TestOperators;
begin
  TestFile('Operators.csv');
end;

procedure TMathExpressionParserTest.TestPhysics;
begin
  TestFile('Physics.csv');
end;

procedure TMathExpressionParserTest.TestRounding;
begin
  TestFile('Rounding.csv');
end;

procedure TMathExpressionParserTest.TestExpression(const AExpression: string; const AExpectedResult: string; const AExpectedErrorCount: SG);
var
  Parser: TMathExpressionParser;
	ParserMessages: TParserMessages;
  ExpressionTreeEvaluator: TExpressionTreeEvaluator;
  Vector: TVector;
  VectorAsString: string;
  CalcErrorCount: SG;
begin
  ParserMessages := TParserMessages.Create;
  try
    Parser := TMathExpressionParser.Create;
    try
      Parser.SxParser := TSxStringParser.Create;
      try
        TSxStringParser(Parser.SxParser).Text := AExpression;

        Parser.Messages := ParserMessages;
        Parser.DecimalSep := '.';
        Parser.ThousandSep := ',';

        Parser.ReadInput;
        ExpressionTreeEvaluator := Parser.CreateExpressionTreeEvaluator;
        CalcErrorCount := ParserMessages.Count;
        try
          try
            if CalcErrorCount = 0 then
              Vector := ExpressionTreeEvaluator.EvaluateRoot;
            if Parser.InputType <> itEOI then
              CalcErrorCount := 1;
          except
            SetLength(Vector, 0);
            CalcErrorCount := 1;
          end;
        finally
          ExpressionTreeEvaluator.Free;
        end;
      finally
        Parser.SxParser.Free;
      end;
    finally
      FreeAndNil(Parser);
    end;
    VectorAsString := VectorToStr(Vector, ofIO);
    if AExpectedErrorCount <> CalcErrorCount then
    begin
      Inc(FFailCount);
    end
    else if not SameResult(AExpectedResult, VectorAsString) then
    begin
//      Warning('Failed expression %1=%2 (should be %3).', [AExpression, VectorAsString, AResult]);
      Inc(FFailCount);
    end;
  finally
    FreeAndNil(ParserMessages);
  end;
end;

initialization
	RegisterTest('Math Expression Parser Test', TMathExpressionParserTest.Suite);
end.
