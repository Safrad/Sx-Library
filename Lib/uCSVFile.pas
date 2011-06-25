//* File:     Lib\uCSVFile.pas
//* Created:  2007-03-10
//* Modified: 2008-02-04
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uCSVFile;

interface

uses
	SysUtils,
	uTypes, uFiles;

const
	CSVSep = ','; // ;
	CSVRemark = '#';

type
	TCSVFile = class
	private
		{ Private declarations }
		CSVFile: TFile;
		CSVFileName: TFileName;
		FColumnCount: SG;
		FLineIndex: SG;
	public
		{ Public declarations }
		constructor Create(const ColumnCount: SG);
		destructor Destroy; override;

		function ReadLine: TArrayOfString;
		function Close: BG;
		function Open(const FileName: TFileName): BG;
		function EOF: BG;
	property
		LineIndex: SG read FLineIndex;
	end;

implementation

uses
	Windows,
	uStrings;

{ TCSV }

constructor TCSVFile.Create(const ColumnCount: SG);
begin
	Assert(ColumnCount >= 0);
	FColumnCount := ColumnCount;
	CSVFile := TFile.Create;
	inherited Create;
end;

destructor TCSVFile.Destroy;
begin
	Close;
	inherited;
end;

function TCSVFile.Open(const FileName: TFileName): BG;
begin
	Assert(FileName <> '');
	FLineIndex := 0;
	Result := CSVFile.Open(FileName, fmReadOnly);
	CSVFileName := FileName;
end;

function TCSVFile.ReadLine: TArrayOfString;
var
	Line: string;
	InLineIndex: SG;
	ColumnIndex: SG;
	LastIndex: SG;
	Quoted: BG;
begin
	Result := nil;
	if Assigned(CSVFile) and (CSVFile.Opened) then
	begin
		while CSVFile.Readln(Line) do
		begin
			if Line = '' then Continue; // Empty line
			if Line[1] = CSVRemark then Continue; // Remark line
			SetLength(Result, FColumnCount);
			LastIndex := 1;
			InLineIndex := 1;
			ColumnIndex := 0;
			Quoted := False;
			while InLineIndex <= Length(Line) do
			begin
				case Line[InLineIndex] of
				'"':
				begin
					if (Quoted = False) {and (LastIndex = InLineIndex)} {FirstColumnChar} then
					begin
						Quoted := True;
						if LastIndex = InLineIndex then
							Inc(LastIndex) // Skip First Quote
						else
						begin
							Delete(Line, InLineIndex, 1); // Delete quote
							Dec(InLineIndex);
						end;
					end
					else
					begin
						if Quoted then
						begin
							if CharAt(Line, InLineIndex + 1) in ['"'] then
							begin // Convert two quotes to one.
								Delete(Line, InLineIndex, 1);
							end
							else
							begin
//								Assert(CharAt(Line, InLineIndex + 1) in [',', ';', CharTab];
								Quoted := False;
								Delete(Line, InLineIndex, 1); // Delete last quote
								Dec(InLineIndex);
							end;
						end;


{						if CharAt(Line, InLineIndex - 1) in ['"', '\'] then
						begin
							// Normal Char
							Delete(Line, InLineIndex - 1, 1);
							Dec(InLineIndex);
						end
						else
						begin
							if LastIndex = InLineIndex then
								Inc(LastIndex) // Skip First Quote
							else
							begin
								Delete(Line, InLineIndex, 1);
								Dec(InLineIndex);
							end;
							Quoted := not Quoted;
						end;}
					end;
				end;
				',', ';', CharTab: // Column separators
				begin
					if Quoted = False then
					begin
						if FColumnCount = 0 then
						begin
							SetLength(Result, ColumnIndex + 1);
							Result[ColumnIndex] := Copy(Line, LastIndex, InLineIndex - LastIndex);
						end
						else if ColumnIndex < FColumnCount then
						begin
							Result[ColumnIndex] := Copy(Line, LastIndex, InLineIndex - LastIndex);
						end;
						Inc(ColumnIndex);
						LastIndex := InLineIndex + 1;
					end;
					// else char is part of value
				end;
				end;

				Inc(InLineIndex);
			end;
			if FColumnCount = 0 then
			begin
				SetLength(Result, ColumnIndex + 1);
				Result[ColumnIndex] := Copy(Line, LastIndex, InLineIndex - LastIndex);
			end
			else if ColumnIndex < FColumnCount then
			begin
				Result[ColumnIndex] := Copy(Line, LastIndex, InLineIndex - LastIndex);
			end;

			Inc(FLineIndex);
			Break;
		end;
	end;
end;

function TCSVFile.Close: BG;
begin
	Result := True;
	if Assigned(CSVFile) then
	begin
		if CSVFile.Opened then
			Result := CSVFile.Close;
		FreeAndNil(CSVFile);
	end;
	CSVFileName := '';
end;

function TCSVFile.EOF: BG;
begin
	Result := True;
	if Assigned(CSVFile) and (CSVFile.Opened) then
		Result := CSVFile.Eof;
end;

end.
