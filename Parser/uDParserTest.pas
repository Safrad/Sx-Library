unit uDParserTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TDParserTest = class(TTestCase)
  private
    FailCount: UG;
    procedure TestExpression(const AExpression: string; const AResult: string; const AErrorCount: SG);
  published
    procedure Test;
  end;

implementation

uses
	SysUtils,
  uDParser, uParserMsg,
	uFiles, uCSVFile, uInputFormat, uOutputFormat, uVector;

{ TDParserTest }

procedure TDParserTest.Test;
var
	CSVFile: TCSVFile;
	Line: TArrayOfString;
begin
	FailCount := 0;
//	PassCount := 0;

	CSVFile := TCSVFile.Create;
  CSVFile.SetColumnNames(['Input', 'Result', 'Errors']);
	try
		if CSVFile.Open(DataDir + 'DParser.csv') then
		begin
			while not CSVFile.EOF do
			begin
				Line := CSVFile.ReadLine;
        TestExpression(Line[0], Line[1], StrToValI(Line[2], False, 0, 0, MaxInt, 1));
			end;
		end;
		CSVFile.Close;
	finally
		FreeAndNil(CSVFile);
	end;
	if FailCount <> 0 then
		Check(FailCount = 0, Format('Fail %s calc tests.', [NToS(FailCount)]));
end;

procedure TDParserTest.TestExpression(const AExpression: string; const AResult: string; const AErrorCount: SG);
var
  Parser: TDParser;
	ParserMessages: TParserMessages;
  Root: PNode;
  Vector: TVector;
  VectorAsString: string;
begin
  ParserMessages := TParserMessages.Create;
  try
    Parser := TDParser.Create(AExpression);
    try
      Parser.Messages := ParserMessages;
      Parser.DecimalSep := '.';
      Parser.ThousandSep := ',';

      Parser.ReadInput;
      Root := Parser.NodeE(nil);
      if Root <> nil then
      begin
        Vector := Calc(Root);
        FreeTree(Root);
      end
      else
        Vector := nil;
    finally
      FreeAndNil(Parser);
    end;
    VectorAsString := VectorToStr(Vector, ofIO);
    if AResult <> VectorAsString then
    begin
//      Warning('Failed expression %1=%2 (should be %3).', [AExpression, VectorAsString, AResult]);
      Inc(FailCount);
    end
    else if AErrorCount <> ParserMessages.Count then
    begin
      Inc(FailCount);
    end;
  finally
    FreeAndNil(ParserMessages);
  end;
end;

initialization
	RegisterTest('DParser Test', TDParserTest.Suite);
end.
