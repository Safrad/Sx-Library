unit uDBFTest;

interface

uses
  SysUtils,
  TestFrameWork;

type
  TDBFTest = class(TTestCase)
  private
    procedure TestFile(const AFileName: TFileName);
  published
    procedure Test;
  end;

implementation

uses
  uSystemPaths,
  uDBF,
  uDBFReader;

{ TDBFTest }

procedure TDBFTest.Test;
begin
  TestFile(SystemPaths.DataDir + 'DBF\Cz-1250.dbf');
  TestFile(SystemPaths.DataDir + 'DBF\Cz-852.dbf');
end;

procedure TDBFTest.TestFile(const AFileName: TFileName);
var
  DBF: TDBF;
  Column: TDBFColumn;
begin
  DBF := TDBF.Create;
  try
    TDBFReader.ReadDBFFromFile(DBF, AFileName);
    Column := DBF.FindColumn('TEXT');
    CheckEquals('P¯Ìliö ûluùouËk˝ k˘Ú ˙pÏl Ô·belskÈ Ûdy', Column.Items[0]);
    Column := DBF.FindColumn('NUMBER');
    CheckEquals(777, Column.Items[0]);
  finally
    DBF.Free;
  end;
end;

initialization
	RegisterTest('DBF Test', TDBFTest.Suite);
end.
