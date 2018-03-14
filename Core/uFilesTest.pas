unit uFilesTest;

interface

uses TestFrameWork;

type
  TFilesTest = class(TTestCase)
  published
    procedure TestSetFileDateTime;
    procedure TestSetFileModified;
  end;

implementation

uses
  SysUtils,
  Windows,
  uFiles,
  uOperatingSystem;

{ TFilesTest }

procedure TFilesTest.TestSetFileModified;

var
  FileName: TFileName;
  Expected, Actual: TFileTime;
begin
  FileName := OperatingSystem.TemporaryDirectory.ProcessTempDir + 'Test.txt';

  WriteStringToFile(FileName, 'data', False);

  Expected := DateTimeToFileTime(1);
  SetFileModified(FileName, Expected);
  CheckTrue(GetFileModified(FileName, Actual));
  CheckEquals(Expected.dwLowDateTime, Actual.dwLowDateTime);
  CheckEquals(Expected.dwHighDateTime, Actual.dwHighDateTime);
end;

procedure TFilesTest.TestSetFileDateTime;
var
  FileName: TFileName;
	ExpectedCreationTime, ExpectedLastAccessTime, ExpectedLastWriteTime: TFileTime;
  ActualCreationTime, ActualLastAccessTime, ActualLastWriteTime: TFileTime;
begin
  FileName := OperatingSystem.TemporaryDirectory.ProcessTempDir + 'Test.txt';

  WriteStringToFile(FileName, 'data', False);

  ExpectedCreationTime := DateTimeToFileTime(1);
  ExpectedLastAccessTime := DateTimeToFileTime(2);
  ExpectedLastWriteTime := DateTimeToFileTime(3);

	SetFileDateTime(FileName, ExpectedCreationTime, ExpectedLastAccessTime, ExpectedLastWriteTime);

	CheckTrue(GetFileDateTime(FileName, ActualCreationTime, ActualLastAccessTime, ActualLastWriteTime));

  CheckEquals(ExpectedCreationTime.dwLowDateTime, ActualCreationTime.dwLowDateTime);
  CheckEquals(ExpectedCreationTime.dwHighDateTime, ActualCreationTime.dwHighDateTime);

  CheckEquals(ExpectedLastAccessTime.dwLowDateTime, ActualLastAccessTime.dwLowDateTime);
  CheckEquals(ExpectedLastAccessTime.dwHighDateTime, ActualLastAccessTime.dwHighDateTime);

  CheckEquals(ExpectedLastWriteTime.dwLowDateTime, ActualLastWriteTime.dwLowDateTime);
  CheckEquals(ExpectedLastWriteTime.dwHighDateTime, ActualLastWriteTime.dwHighDateTime);
end;

initialization
	RegisterTest('Files Test', TFilesTest.Suite);
end.
