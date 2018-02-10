unit uWebUpdateTest;

interface

uses
  TestFramework;

type
  TWebUpdateTest = class(TTestCase)
  published
    procedure Test;
    procedure TestFail;
  end;

implementation

uses
  uTypes, SysUtils, IdHTTP, IdException, uFiles, uWebUpdate, uDownloadEx;

{ TWebUpdateTest }

procedure TWebUpdateTest.Test;
var
  FileName: TFileName;
  i: SG;
begin
  FileName := TempDir + 'data.txt';
  for i := 0 to 9 do
  begin
    DownloadFile('http://sx.rosada.cz/test/test.txt', FileName);
    CheckEquals(16, GetFileSizeU(FileName));
  end;
end;

procedure TWebUpdateTest.TestFail;
var
  FileName: TFileName;
  Success: BG;
begin
  FileName := TempDir + 'data.txt';
  Success := False;
  try
    DownloadFileEx('http://software.rosada.cz/nonexisting.zip', FileName, '');
    Success := True;
  except
    on E: Exception do
    begin
      Check(E is Exception, 'Exception expected.');
    end;
  end;
  if Success then
    Fail('Exception expected.');
end;

initialization
  RegisterTest('Web Update Test', TWebUpdateTest.Suite);

end.

