unit uFileTest;

interface

implementation

uses
	SysUtils,
	uTypes, uFile, uFiles, uStrings;

procedure Test1;
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
					Assert(Line = Line1);
					F.Readln(Line);
					if fc <> fcAnsi then
						Assert(Line = Line2)
					else
						Assert(Line = AnsiString(Line2));
					F.Close;
				end;
			finally
				F.Free;
			end;
		end;
    DeleteFile(FileName);
	end;
end;

procedure Test2;
var
	Text: string;
	FileName: TFileName;
	Lines: TArrayOfString;
  Count: SG;
	fc: TFileCharset;
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
      Assert(Count = 4);
      Assert(Lines[0] = 'line1');
      Assert(Lines[1] = 'line2');
      Assert(Lines[2] = 'line3');
      Assert(Lines[3] = 'line4');
      DeleteFile(FileName);
    end;
  end;
end;


initialization

Test1;
Test2;

end.
