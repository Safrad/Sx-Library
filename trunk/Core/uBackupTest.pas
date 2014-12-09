unit uBackupTest;

interface

uses TestFrameWork;

type
  TBackupTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uTypes,
  SysUtils,
  uFiles,
  uBackup;

procedure TBackupTest.Test;
var
  FileName: TFileName;
  i: SG;
begin
  FileName := TempDir + 'data.txt';
  for i := 0 to 199 do
  begin
	  WriteStringToFile(FileName, 'text' + IntToStr(i), False);
	  BackupFile(FileName, bfSame);
  end;
end;

initialization
	RegisterTest('Backup Test', TBackupTest.Suite);
end.
