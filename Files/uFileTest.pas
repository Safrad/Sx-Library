unit uFileTest;

interface

uses
  SysUtils,
  TestFrameWork;

type
  TFileTest = class(TTestCase)
  private
    procedure CreateSampleFile(const FileName: TFileName);
  published
    procedure TestReadWriteLine;
    procedure TestRewriteFile;
    procedure TestWriteThroughput;
    procedure TestReadThroughput;
  end;

implementation

uses
	uTypes, uFileCharset, uFile, uFiles, uStrings, uChar, uOperatingSystem,
  uTemporaryDirectory;

{ TFileTest }

procedure TFileTest.TestReadWriteLine;
const
	Line1 = 'a Ë P¯Ìliö ûluùouËk˝ k˘Ú ˙pÏl Ô·belskÈ Ûdy';
	Line2 = 'a' {$IFDEF UNICODE} + #$03A9 {$ENDIF};
	Line2a = 'a' {$IFDEF UNICODE} + '?' {$ENDIF};
var
	fc: TFileCharset;
	F: TFile;
	FileName: TFileName;
	Line: UnicodeString;
begin
	// Tests
	for fc := Low(fc) to High(fc) do
	begin
		FileName := TemporaryDirectory.ProcessTempDir + 'Test' + IntToStr(SG(fc)) + '.txt';
		F := TFile.Create;
		if not (fc in [fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE]) then
			F.DeleteAfterClose := True;
		try
			F.Charset := fc;
			if F.Open(FileName, fmRewrite) then
			begin
				if fc in [fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE] then
				begin
					F.Writeln(UnicodeString(Line1));
					F.Writeln(UnicodeString(Line2));
				end
				else
					F.Truncate;
				F.Close;
			end;
		finally
			F.Free;
		end;
		if fc in [fcAnsi, fcUTF8, fcUTF16BE, fcUTF16LE] then
		begin
			F := TFile.Create;
			F.DeleteAfterClose := True;
			try
				F.Charset := fc;
				if F.Open(FileName, fmReadOnly) then
				begin
					F.Readln(Line);
					Check(Line = Line1);
					F.Readln(Line);
					if fc <> fcAnsi then
						Check(Line = Line2)
					else
						Check(Line = Line2a);
					F.Close;
				end;
			finally
				F.Free;
			end;
		end;
    SysUtils.DeleteFile(FileName);
	end;
end;

procedure TFileTest.TestRewriteFile;
var
	F: TFile;
	FileName: TFileName;
  i: SG;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'FileTest.txt';
  for i := 0 to 1 do
  begin
    F := TFile.Create;
    try
      if F.Open(FileName, fmRewrite) then
      begin
        F.Writeln('text');
        F.Close;
      end;
    finally
      F.Free;
    end;
  end;
  SysUtils.DeleteFile(fileName);
end;

procedure TFileTest.TestWriteThroughput;
var
	F: TFile;
  i: SG;
begin
  F := TFile.Create;
  try
    F.DeleteAfterClose := True;
    F.DefaultCharset := fcUTF8;
    F.Open(TemporaryDirectory.ProcessTempDir + 'Test.txt', fmRewrite);
    for i := 0 to 999999 do
    begin
      F.Writeln('[Test line]');
    end;
    F.Close;
  finally
    F.Free;
  end;
end;

procedure TFileTest.TestReadThroughput;
var
  Line: string;
  F: TFile;
  FileName: TFileName;
begin
  FileName := TemporaryDirectory.ProcessTempDir + 'Test.txt';
  CreateSampleFile(FileName);

  F := TFile.Create;
  try
    F.DeleteAfterClose := True;
    F.DefaultCharset := fcAnsi;
    F.Open(FileName, fmReadOnly);
    while not F.Eof do
    begin
      F.Readln(Line);
    end;
    F.Close;
  finally
    F.Free;
  end;
end;

procedure TFileTest.CreateSampleFile(const FileName: TFileName);
var
  s: string;
begin
  s := RandomString(10 * MB);
  WriteStringToFile(FileName, s, False);
end;

initialization
	RegisterTest('File Test', TFileTest.Suite);
end.
