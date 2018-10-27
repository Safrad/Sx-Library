unit uFileTest;

interface

uses TestFrameWork;

type
  TFileTest = class(TTestCase)
  published
    procedure TestReadWriteLine;
    procedure TestRewriteFile;
  end;

implementation

uses
  Windows, SysUtils,
	uTypes, uFile, uFiles, uStrings, uChar, uOperatingSystem,
  uTemporaryDirectory;

{ TFileTest }

procedure TFileTest.TestReadWriteLine;
const
	Line1 = 'a Ë P¯Ìliö ûluùouËk˝ k˘Ú ˙pÏl Ô·belskÈ Ûdy';
	Line2 = 'a' {$IFDEF UNICODE} + #$03A9 {$ENDIF};
var
	fc: TFileCharset;
	F: TFile;
	FileName: TFileName;
	Line: UnicodeString;
begin
	// Tests
	for fc := Low(fc) to High(fc) do
	begin
		FileName := OperatingSystem.TemporaryDirectory.ProcessTempDir + 'Test' + IntToStr(SG(fc)) + '.txt';
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
						Check(Line = AnsiString(Line2));
					F.Close;
				end;
			finally
				F.Free;
			end;
		end;
    DeleteFile(FileName);
	end;
end;

procedure TFileTest.TestRewriteFile;
var
	F: TFile;
	FileName: TFileName;
  i: SG;
begin
  FileName := OperatingSystem.TemporaryDirectory.ProcessTempDir + 'FileTest.txt';
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
  DeleteFile(fileName);
end;

initialization
	RegisterTest('File Test', TFileTest.Suite);
end.
