unit uCSVFileTest;

interface

uses
  SysUtils,
  TestFrameWork;

type
  TCSVFileTest = class(TTestCase)
  private
    procedure ReadCSV(const AFileName: TFileName);
  published
    procedure OkTest;
    procedure WrongTest;
  end;

implementation

uses
  uTypes,
  uFiles,
  uCSVFile;

procedure TCSVFileTest.OkTest;
var
  FileName: TFileName;
  CSVFile: TCSVFile;
  Line: TArrayOfString;
begin
  FileName := DataDir + 'CSV' + PathDelim + 'Ok.csv';
  CSVFile := TCSVFile.Create(2);
  try
    if CSVFile.Open(FileName) then
    begin
      while not CSVFile.EOF do
      begin
        Line := CSVFile.ReadLine;

        CheckEquals(StrToInt(Line[0]), Length(Line[1]), 'Text: ''' + Line[1] + '''');
      end;
      CSVFile.Close;
    end;
  finally
    CSVFile.Free;
  end;
end;

procedure TCSVFileTest.ReadCSV(const AFileName: TFileName);
var
  CSVFile: TCSVFile;
  Line: TArrayOfString;
begin
  CSVFile := TCSVFile.Create(2);
  try
    if CSVFile.Open(AFileName) then
    begin
      while not CSVFile.EOF do
      begin
        Line := CSVFile.ReadLine;
      end;
      CSVFile.Close;
    end;
  finally
    CSVFile.Free;
  end;
end;

procedure TCSVFileTest.WrongTest;
var
  FileName: TFileName;
  i: SG;
begin
  for i := 1 to 99 do
  begin
    FileName := DataDir + 'CSV' + PathDelim + 'Wrong' + IntToStr(i) + '.csv';
    if not FileExistsEx(FileName) then
      Break;
    try
      ReadCSV(FileName);
    except
      // Must be called
    end;
  end;
end;

initialization
	RegisterTest('CSV File Test', TCSVFileTest.Suite);
end.