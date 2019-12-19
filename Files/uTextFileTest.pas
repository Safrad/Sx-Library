unit uTextFileTest;

interface

uses
  SysUtils,
  TestFrameWork,
  uFileCharset;

type
  TTextFileTest = class(TTestCase)
  private
    procedure TestReadWriteLineCustom(const AFileCharset: TFileCharset);
    procedure CreateSampleFile(const FileName: TFileName);
  published
    procedure TestReadWriteLine;
    procedure TestReadWriteLine16BE;
    procedure TestReadWriteLine16LE;
    procedure TestRewriteFile;
    procedure TestWriteThroughput;
    procedure TestReadThroughput;
  end;

implementation

uses
  uRawFile,
	uTypes, uTextFile, uFiles, uStrings, uChar, uOperatingSystem,
  uTemporaryDirectory;

{ TTextFileTest }

procedure TTextFileTest.TestReadThroughput;
var
  Line: string;
  F: TTextFile;
  FileName: TFileName;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'Test.txt';
  CreateSampleFile(FileName);

  F := TTextFile.Create;
  try
    F.DeleteAfterClose := True;
    F.FileName := FileName;
    F.FileMode := fmReadOnly;
    F.DefaultCharset := fcAnsi;
    F.Open;
    while not F.Eof do
    begin
      F.ReadLine(Line);
    end;
    F.Close;
  finally
    F.Free;
  end;
end;

procedure TTextFileTest.CreateSampleFile(const FileName: TFileName);
var
  s: string;
begin
  s := RandomString(10 * MB, 32, 127);
  WriteStringToFile(FileName, s, False);
end;

procedure TTextFileTest.TestReadWriteLine;
var
	fc: TFileCharset;
begin
	// Tests
	for fc := Low(fc) to High(fc) do
	begin
    TestReadWriteLineCustom(fc);
	end;
end;

procedure TTextFileTest.TestReadWriteLine16BE;
begin
  TestReadWriteLineCustom(fcUTF16BE);
end;

procedure TTextFileTest.TestReadWriteLine16LE;
begin
  TestReadWriteLineCustom(fcUTF16LE);
end;

procedure TTextFileTest.TestReadWriteLineCustom(const AFileCharset: TFileCharset);
const
	Line1 = 'a Ë P¯Ìliö ûluùouËk˝ k˘Ú ˙pÏl Ô·belskÈ Ûdy';
	Line2 = 'a' {$IFDEF UNICODE} + #$03A9 {$ENDIF};
	Line2a = 'a' {$IFDEF UNICODE} + '?' {$ENDIF};
var
	F: TTextFile;
	FileName: TFileName;
	Line: UnicodeString;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'Test' + IntToStr(SG(AFileCharset)) + '.txt';
  F := TTextFile.Create;
  if not (AFileCharset in [fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE]) then
    F.DeleteAfterClose := True;
  try
    F.FileName := FileName;
    F.FileMode := fmRewrite;
    F.DefaultCharset := AFileCharset;
    F.Open;
    if AFileCharset in [fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE] then
    begin
      F.WriteLine(UnicodeString(Line1));
      F.WriteLine(UnicodeString(Line2));
    end
    else
      F.Truncate;
    F.Close;
  finally
    F.Free;
  end;
  if AFileCharset in [fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE] then
  begin
    F := TTextFile.Create;
    try
      F.FileName := FileName;
      F.FileMode := fmReadOnly;
      F.DeleteAfterClose := True;
      F.DefaultCharset := AFileCharset;
      F.Open;
      F.ReadLine(Line);
      Check(Line = Line1);
      F.ReadLine(Line);
      if AFileCharset <> fcAnsi then
        Check(Line = Line2)
      else
        Check(Line = Line2a);
      F.Close;
    finally
      F.Free;
    end;
  end;
  DeleteFile(FileName);
end;

procedure TTextFileTest.TestRewriteFile;
var
	F: TTextFile;
	FileName: TFileName;
  i: SG;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'FileTest.txt';
  for i := 0 to 1 do
  begin
    F := TTextFile.Create;
    try
      F.FileName := FileName;
      F.FileMode := fmRewrite;
      F.DefaultCharset := fcUTF8;
      F.Open;
      F.WriteLine('text');
      F.Close;
    finally
      F.Free;
    end;
  end;
  DeleteFile(fileName);
end;

procedure TTextFileTest.TestWriteThroughput;
var
	F: TTextFile;
  i: SG;
begin
  F := TTextFile.Create;
  try
    F.DeleteAfterClose := True;
    F.FileName := TemporaryDirectory.ProcessTempDir + 'Test.txt';
    F.FileMode := fmRewrite;
    F.DefaultCharset := fcUTF8;
    F.Open;
    for i := 0 to 999999 do
    begin
      F.WriteLine('[Test line]');
    end;
    F.Close;
  finally
    F.Free;
  end;
end;

initialization
	RegisterTest('Text File Test', TTextFileTest.Suite);
end.
