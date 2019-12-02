unit uCSVFileTest;

interface

uses
  uTypes,
  SysUtils,
  TestFrameWork;

type
  TCSVFileTest = class(TTestCase)
  private
    function ReadIncorrectCSV(const AFileName: TFileName): SG;
  published
    procedure EmptyDataTest;
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
    CSVFile.Open(FileName);
    while not CSVFile.EOF do
    begin
      Line := CSVFile.ReadLine;

      CheckEquals(StrToInt(Line[0]), Length(Line[1]), 'Text: ''' + Line[1] + '''');
    end;
    CheckTrue(CSVFile.Errors = '');
    CSVFile.Close;
  finally
    CSVFile.Free;
  end;
end;

function TCSVFileTest.ReadIncorrectCSV(const AFileName: TFileName): SG;
var
  CSVFile: TCSVFile;
  Line: TArrayOfString;
begin
  Result := 0;
  CSVFile := TCSVFile.Create;
  try
    CSVFile.Open(AFileName);
    while not CSVFile.EOF do
    begin
      Line := CSVFile.ReadLine;
      Inc(Result);
    end;
    CheckTrue(CSVFile.Errors <> '');
    CSVFile.Close;
  finally
    CSVFile.Free;
  end;
end;

procedure TCSVFileTest.EmptyDataTest;
var
  FileName: TFileName;
  CSVFile: TCSVFile;
  Line: TArrayOfString;
begin
  FileName := DataDir + 'CSV' + PathDelim + 'Empty.csv';
  CSVFile := TCSVFile.Create;
  CSVFile.SetColumnNames(['Length', 'Text']);
  try
    CSVFile.Open(FileName);
    CheckTrue(CSVFile.EOF);
    CheckTrue(CSVFile.Errors = '');
    CSVFile.Close;
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
    LineCount := ReadIncorrectCSV(FileName);
    CheckEquals(i, LineCount);
  end;
end;

initialization
	RegisterTest('CSV File Test', TCSVFileTest.Suite);
end.