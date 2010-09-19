//* File:     Lib\uCSV.pas
//* Created:  2007-03-10
//* Modified: 2007-03-10
//* Version:  X.X.37.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uCSV;

interface

uses
	SysUtils,
	uTypes, uFiles;

const
	CSVSep = ',';

type
	TStringArray = array of string;

	TCSVFile = class
	private
		{ Private declarations }
		CSVFile: TFile;
		CSVFileName: TFileName;
//		FLine: TStringArray;
	public
		{ Public declarations }
//		property Line: TStringArray read FLine;
		constructor Create(var FileName: TFileName); overload;
		constructor Create; overload;
		destructor Destroy; override;

		function ReadLine: TStringArray;
		function Close: BG;
		function Open(var FileName: TFileName): BG;
		function EOF: BG;
	end;

implementation

uses
	Windows,
	uStrings;

{ TCSV }

constructor TCSVFile.Create(var FileName: TFileName);
begin
	inherited Create;
	CSVFile := TFile.Create;
	Open(FileName);
end;

constructor TCSVFile.Create;
begin
	CSVFile := TFile.Create;
	inherited Create;
end;

destructor TCSVFile.Destroy;
begin
	Close;
	inherited;
end;

function TCSVFile.Open(var FileName: TFileName): BG;
begin
	Result := CSVFile.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False);
	CSVFileName := FileName;
end;

function TCSVFile.ReadLine: TStringArray;
label LRetry;

	procedure AddItem(var Result: TStringArray; NewItem: string);
	begin
		SetLength(Result, Length(Result) + 1);
		Result[Length(Result) - 1] := NewItem;
	end;

var
	Line: string;
	InLineIndex: SG;
	LastIndex: SG;
	Quoted: BG;
begin
	Result := nil;
	if Assigned(CSVFile) and (CSVFile.Opened) then
	begin
		while CSVFile.Readln(Line) do
		begin
			if Line = '' then Continue; // Empty line
			if Line[1] = '#' then Continue; // Remark line
			LastIndex := 1;
			InLineIndex := 1;
			Quoted := False;
			while InLineIndex <= Length(Line) do
			begin
				case Line[InLineIndex] of
				'"':
				begin
					if CharAt(Line, InLineIndex - 1) in ['"', '\'] then
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
					end;
				end;
				',', ';', CharTab:
				begin
					if Quoted = False then
					begin
						AddItem(Result, Copy(Line, LastIndex, InLineIndex - LastIndex));
						LastIndex := InLineIndex + 1;
					end;
					// else char is part of value
				end;
				end;

				Inc(InLineIndex);
			end;
			AddItem(Result, Copy(Line, LastIndex, InLineIndex - LastIndex));

			Break;
		end;
	end;
end;

function TCSVFile.Close: BG;
begin
	if Assigned(CSVFile) then
	begin
		Result := CSVFile.Close;
		FreeAndNil(CSVFile);
	end
	else
		Result := True;
	CSVFileName := '';
end;

function TCSVFile.EOF: BG;
begin
	Result := True;
	if Assigned(CSVFile) and (CSVFile.Opened) then
		Result := CSVFile.Eof;
end;

end.
