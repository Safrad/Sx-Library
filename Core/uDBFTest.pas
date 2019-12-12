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
  uFiles,
  uDBF;

{ TDBFTest }

procedure TDBFTest.Test;
begin
  TestFile(DataDir + 'DBF\Cz-1250.dbf');
  TestFile(DataDir + 'DBF\Cz-852.dbf');
end;

procedure TDBFTest.TestFile(const AFileName: TFileName);
var
  DBF: TDBF;
  Column: TDBFColumn;
begin
  DBF := TDBF.Create;
  try
    DBF.LoadFromFile(AFileName);
    Column := DBF.FindColumn('TEXT');
    CheckEquals('Pøíliš luouèkı kùò úpìl ïábelské ódy', Column.Items[0]);
    Column := DBF.FindColumn('NUMBER');
    CheckEquals(777, Column.Items[0]);
  finally
    DBF.Free;
  end;
end;

initialization
	RegisterTest('DBF Test', TDBFTest.Suite);
end.
