unit uCSVFile;

interface

uses
	SysUtils,
	uTypes, uFile;

const
	CSVSep = ','; // ;
	CSVRemark = '#';

type
	TCSVFile = class
	private
		{ Private declarations }
		FFile: TFile;
		FColumnCount: SG;
		FLineIndex: SG;
		FColumnIndexes: TArrayOfSG;
	public
		{ Public declarations }
		AcceptRemark: BG;
		constructor Create(const ColumnCount: SG);
		destructor Destroy; override;

		function ReadLine: TArrayOfString;
		procedure RemapColumns(const ColumnNames: array of string);
		function Open(const FileName: TFileName): BG;
		function Close: BG;
		function EOF: BG;
	property
		LineIndex: SG read FLineIndex;
	end;

procedure CreateCSVHead(const FileName: TFileName; const Head: array of string);

implementation

uses
	Windows, Math,
	uStrings, uMath, uMsg,
	uFiles;

procedure CreateCSVHead(const FileName: TFileName; const Head: array of string);
var
	i: SG;
	f, s: string;
	LineIndex: SG;
	l: string;
begin
	s := CSVRemark;
	for i := 0 to Length(Head) - 1 do
	begin
		if i <> 0 then
			s := s + CSVSep;
		s := s + '"' + Head[i] + '"';
	end;
	s := s + FileSep;
	if FileExists(FileName) = False then
	begin
		WriteStringToFile(FileName, s, False);
	end
	else
	begin
		f := ReadStringFromFile(FileName);
		LineIndex := 1;
		l := ReadToNewLine(f, LineIndex) + FileSep;
		if l <> s then
		begin
			Delete(f, 1, LineIndex - 1);
			Insert(s, f, 1);
			WriteStringToFile(FileName, f, False);
		end;
	end;
end;

const
  MaxColumn = 255;

{ TCSVFile }

constructor TCSVFile.Create(const ColumnCount: SG);
begin
	SetLength(FColumnIndexes, MaxColumn);
	FillOrderUG(FColumnIndexes[0], MaxColumn);

	inherited Create;

	Assert(ColumnCount >= 0);
	FColumnCount := ColumnCount;
	FFile := TFile.Create;
end;

destructor TCSVFile.Destroy;
begin
	Close;
	FreeAndNil(FFile);
	inherited;
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
	if Assigned(FFile) and (FFile.Opened) then
	begin
		SetLength(Result, FColumnCount);
		while FFile.Readln(Line) do
		begin
			if Line = '' then Continue; // Empty line
			if (Line[1] = CSVRemark) and (AcceptRemark = False) then Continue; // Remark line
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
							if CharAt(Line, InLineIndex + 1) = '"' then
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
							if FColumnIndexes[ColumnIndex] >= 0 then
							begin
								SetLength(Result, Max(Length(Result), FColumnIndexes[ColumnIndex] + 1));
								Result[FColumnIndexes[ColumnIndex]] := Copy(Line, LastIndex, InLineIndex - LastIndex);
							end;
						end
						else if ColumnIndex < FColumnCount then
						begin
							if FColumnIndexes[ColumnIndex] >= 0 then
								Result[FColumnIndexes[ColumnIndex]] := Copy(Line, LastIndex, InLineIndex - LastIndex);
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
				if FColumnIndexes[ColumnIndex] >= 0 then
				begin
					SetLength(Result, Max(Length(Result), FColumnIndexes[ColumnIndex] + 1));
					Result[FColumnIndexes[ColumnIndex]] := Copy(Line, LastIndex, InLineIndex - LastIndex);
				end;
			end
			else if ColumnIndex < FColumnCount then
			begin
				if FColumnIndexes[ColumnIndex] >= 0 then
					Result[FColumnIndexes[ColumnIndex]] := Copy(Line, LastIndex, InLineIndex - LastIndex);
			end;

			Inc(FLineIndex);
			Break;
		end;
	end;
end;

function TCSVFile.Open(const FileName: TFileName): BG;
begin
	Result := FFile.Open(FileName, fmReadOnly);
end;

function TCSVFile.Close: BG;
begin
	Result := True;
	if Assigned(FFile) then
	begin
		if FFile.Opened then
			Result := FFile.Close;
	end;
	SetLength(FColumnIndexes, 0);
end;

function TCSVFile.EOF: BG;
begin
	Result := True;
	if Assigned(FFile) and (FFile.Opened) then
		Result := FFile.Eof;
end;

procedure TCSVFile.RemapColumns(const ColumnNames: array of string);
var
	Row: TArrayOfString;
	i, j: SG;
begin
  FColumnCount := Length(ColumnNames);
	SetLength(FColumnIndexes, MaxColumn);
	FillOrderUG(FColumnIndexes[0], MaxColumn);

	AcceptRemark := True;
	Row := ReadLine;
	AcceptRemark := False;

	if FirstChar(Row[0]) = CSVRemark then
		Delete(Row[0], 1, 1);

	SetLength(FColumnIndexes, Length(Row));
	for i := 0 to Length(Row) - 1 do
	begin
		FColumnIndexes[i] := -1;
		for j := 0 to Length(ColumnNames) - 1 do
		begin
			if ColumnNames[j] = Row[i] then
			begin
				FColumnIndexes[i] := j;
				Break;
			end;
		end;
		if (FColumnIndexes[i] = -1) and (Row[i] <> '') then
			Warning('Column %1 not used.', [Row[i]]);
	end;
end;

end.
