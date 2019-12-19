unit uFilesTest;

interface

uses TestFrameWork;

type
  TFilesTest = class(TTestCase)
  published
    procedure TestSetFileDateTime;
    procedure TestSetFileModified;
    procedure TestReadWriteString;
  end;

implementation

uses
  SysUtils,
  uTypes,
  uFileCharset,
  uFiles,
  uChar,
  uTemporaryDirectory;

{ TFilesTest }

procedure TFilesTest.TestSetFileModified;

var
  FileName: TFileName;
  Expected, Actual: TFileTime;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'Test.txt';

  WriteStringToFile(FileName, 'data', False);

  Expected := DateTimeToFileTime(1);
  SetFileModified(FileName, Expected);
  Actual := GetFileModified(FileName);
  CheckEquals(Expected.dwLowDateTime, Actual.dwLowDateTime);
  CheckEquals(Expected.dwHighDateTime, Actual.dwHighDateTime);
end;

procedure TFilesTest.TestReadWriteString;
var
	Text: string;
	FileName: TFileName;
	Lines: TArrayOfString;
  Count: SG;
	fc: TFileCharset;
  i: SG;
begin
	Text := 'line1' + CharCR + 'line2' + CharLF + 'line3' + CharCR + CharLF + 'line4';
	for fc := Low(fc) to High(fc) do
	begin
		if fc in [fcAnsi, fcUTF8{$if CompilerVersion >= 21} , fcUTF16BE, fcUTF16LE{$ifend}] then
		begin
      FileName := TemporaryDirectory.ProcessTempDir + 'TestLine' + IntToStr(SG(fc)) + '.txt';
      WriteStringToFile(FileName, Text, False, fc);
      Count := 0;
      ReadStringsFromFile(FileName, Lines, Count);
      Check(Count = 4, 'count');
      for i := 0 to 3 do
	      Check(Lines[i] = 'line' + IntToStr(i + 1), 'line ' + IntToStr(i + 1));
      DeleteFileEx(FileName);
    end;
  end;
end;

procedure TFilesTest.TestSetFileDateTime;
var
  FileName: TFileName;
	ExpectedCreationTime, ExpectedLastAccessTime, ExpectedLastWriteTime: TFileTime;
  ActualCreationTime, ActualLastAccessTime, ActualLastWriteTime: TFileTime;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'Test.txt';

  WriteStringToFile(FileName, 'data', False);

  ExpectedCreationTime := DateTimeToFileTime(1);
  ExpectedLastAccessTime := DateTimeToFileTime(2);
  ExpectedLastWriteTime := DateTimeToFileTime(3);

	SetFileDateTime(FileName, ExpectedCreationTime, ExpectedLastAccessTime, ExpectedLastWriteTime);
	GetFileDateTime(FileName, ActualCreationTime, ActualLastAccessTime, ActualLastWriteTime);

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
