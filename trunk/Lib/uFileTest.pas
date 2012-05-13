unit uFileTest;

interface

uses TestFrameWork;

type
  TFileTest = class(TTestCase)
  published
    procedure Test1;
    procedure Test2;
  end;

implementation

uses
	SysUtils,
	uTypes, uFile, uFiles, uStrings;

{ TFileTest }

procedure TFileTest.Test1;
const
	Line1 = 'a è Šafránek David';
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
		FileName := CommonTempDir + 'Test' + IntToStr(SG(fc)) + '.txt';
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

procedure TFileTest.Test2;
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
      FileName := CommonTempDir + 'TestLine' + IntToStr(SG(fc)) + '.txt';
      WriteStringToFile(FileName, Text, False, fc);
      Count := 0;
      ReadStringsFromFile(FileName, Lines, Count);
      Check(Count = 4, 'count');
      for i := 0 to 3 do
	      Check(Lines[i] = 'line' + IntToStr(i + 1), 'line ' + IntToStr(i + 1));
      DeleteFile(FileName);
    end;
  end;
end;

initialization
	RegisterTest('File Test', TFileTest.Suite);
end.
