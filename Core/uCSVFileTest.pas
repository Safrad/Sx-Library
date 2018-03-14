unit uCSVFileTest;

interface

uses
  uTypes,
  SysUtils,
  TestFrameWork;

type
  TCSVFileTest = class(TTestCase)
  private
    function ReadCSV(const AFileName: TFileName): SG;
  published
    procedure CorrectDataTest;
    procedure IncorrectDataTest;
  end;

implementation

uses
  uFiles,
  uCSVFile;

procedure TCSVFileTest.CorrectDataTest;
var
  FileName: TFileName;
  CSVFile: TCSVFile;
  Line: TArrayOfString;
begin
  FileName := DataDir + 'CSV' + PathDelim + 'Correct.csv';
  CSVFile := TCSVFile.Create;
  CSVFile.SetColumnNames(['Length', 'Text']);
  try
    if CSVFile.Open(FileName) then
    begin
      while not CSVFile.EOF do
      begin
        Line := CSVFile.ReadLine;

        CheckEquals(StrToInt(Line[0]), Length(Line[1]), 'Text: ''' + Line[1] + '''');
      end;
      CheckTrue(CSVFile.Errors = '');
      CSVFile.Close;
    end;
  finally
    CSVFile.Free;
  end;
end;

function TCSVFileTest.ReadCSV(const AFileName: TFileName): SG;
var
  CSVFile: TCSVFile;
  Line: TArrayOfString;
begin
  Result := 0;
  CSVFile := TCSVFile.Create;
  try
    if CSVFile.Open(AFileName) then
    begin
      while not CSVFile.EOF do
      begin
        Line := CSVFile.ReadLine;
        Inc(Result);
      end;
      CheckTrue(CSVFile.Errors <> '');
      CSVFile.Close;
    end;
  finally
    CSVFile.Free;
  end;
end;

procedure TCSVFileTest.IncorrectDataTest;
var
  FileName: TFileName;
  i: SG;
  LineCount: SG;
begin
  for i := 0 to 99 do
  begin
    FileName := DataDir + 'CSV' + PathDelim + 'Incorrect' + IntToStr(i) + '.csv';
    if not FileExistsEx(FileName) then
      Break;
    LineCount := ReadCSV(FileName);
    CheckEquals(i, LineCount);
  end;
end;

initialization
	RegisterTest('CSV File Test', TCSVFileTest.Suite);
end.