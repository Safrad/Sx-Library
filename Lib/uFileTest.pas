// * File:     Lib\uFileTest.pas
// * Created:  2009-09-11
// * Modified: 2009-11-15
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uFileTest;

interface

implementation

uses
	SysUtils,
	uTypes, uFile, uFiles;

procedure Test;
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
		if not (fc in [fcAnsi, fcUTF8, fcUTF16LE]) then
			F.DeleteAfterClose := True;
		try
			F.Charset := fc;
			if F.Open(FileName, fmRewrite) then
			begin
				if fc in [fcAnsi, fcUTF8, fcUTF16LE] then
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
		if fc in [fcAnsi, fcUTF8, fcUTF16LE] then
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
	end;
end;

initialization

Test;

end.
