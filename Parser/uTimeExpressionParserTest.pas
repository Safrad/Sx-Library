unit uTimeExpressionParserTest;

interface

uses
  uTypes,
	SysUtils,
  TestFrameWork;

type
  TTimeExpressionParserTest = class(TTestCase)
  private
    procedure TestExpression(const AExpression: string; const AExpectedResultInMs: U8; const AExpectedErrorCount: SG);
  published
    procedure TestEmptyInput;
    procedure TestErrors;
    procedure TestOk;
  end;

implementation

uses
  uStrings,
  uTimeExpressionParser, uParserMsg,
  uSxStringParser,
  uExpressionTreeEvaluator;

{ TTimeExpressionParserTest }

procedure TTimeExpressionParserTest.TestEmptyInput;
begin
  TestExpression('', 0, 0);
end;

procedure TTimeExpressionParserTest.TestErrors;
begin
  TestExpression('pes', 0, 1);
end;

procedure TTimeExpressionParserTest.TestExpression(const AExpression: string; const AExpectedResultInMs: U8; const AExpectedErrorCount: SG);
var
  Parser: TTimeExpressionParser;
	ParserMessages: TParserMessages;
  Value: U8;
  CalcErrorCount: SG;
begin
  ParserMessages := TParserMessages.Create;
  try
    Parser := TTimeExpressionParser.Create;
    try
      Parser.SxParser := TSxStringParser.Create;
      TSxStringParser(Parser.SxParser).Text := AExpression;

      Parser.Messages := ParserMessages;
      Parser.DecimalSep := '.';
      Parser.ThousandSep := ',';

      Parser.Parse;
      CalcErrorCount := ParserMessages.Count;
      Value := Trunc(Parser.Value.Milliseconds);
    finally
      Parser.SxParser.Free;
      FreeAndNil(Parser);
    end;
  finally
    FreeAndNil(ParserMessages);
  end;
  CheckEquals(AExpectedErrorCount, CalcErrorCount);
  CheckEquals(AExpectedResultInMs, Value);
end;

procedure TTimeExpressionParserTest.TestOk;
begin
  TestExpression('37', 37000, 0);
  TestExpression('1:56', 116000, 0);
  TestExpression('2:30:56', 9056000, 0);
  TestExpression('2:30:56.758', 9056758, 0);
end;

initialization
	RegisterTest('Time Expression Parser Test', TTimeExpressionParserTest.Suite);
end.
