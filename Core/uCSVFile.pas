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
		FRowIndex: SG;
		FColumnIndexes: TArrayOfSG;
    FRow: TArrayOfString;
    FColumnNames: array of string;
    FAcceptRemark: BG;
    FErrors: string;

		function ReadNextDataRow: BG;
    function ReadNextHeaderOrDataRow(const AAcceptRemark: BG): BG;
    procedure ClearRow;
    procedure AppendRow(const AColumnIndex: SG; const AText: string);
    procedure SetAcceptRemark(const Value: BG);
		procedure RemapColumns;
    procedure AddError(const AMessage: string);
	public
		{ Public declarations }
		constructor Create;
		destructor Destroy; override;

		function ReadLine: TArrayOfString;
		function Open(const FileName: TFileName): BG;
		function Close: BG;
		function EOF: BG;

    procedure WriteCSVData(const AFileName: TFileName; const AData: string; const ADelimeter: string = CSVSep);
    procedure SetColumnNames(const Value: array of string);

		property AcceptRemark: BG read FAcceptRemark write SetAcceptRemark;
    property Errors: string read FErrors;
	end;

implementation

uses
	Math,
	uStrings, uMath,
	uFiles;

{ TCSVFile }

constructor TCSVFile.Create;
begin
	inherited;

	FFile := TFile.Create;
end;

destructor TCSVFile.Destroy;
begin
	Close;
	FreeAndNil(FFile);

	inherited;
end;

function TCSVFile.ReadNextDataRow: BG;
begin
  Result := ReadNextHeaderOrDataRow((FRowIndex = 0) or FAcceptRemark);
  if Result and (FRowIndex = 0) then
  begin // Header
    RemapColumns;
    Result := ReadNextHeaderOrDataRow(FAcceptRemark); // First data line
  end;
  Inc(FRowIndex);
end;

function TCSVFile.ReadNextHeaderOrDataRow(const AAcceptRemark: BG): BG;
var
	InLineIndex: SG;
	ColumnIndex: SG;
	LastIndex: SG;
  FirstOrAfterSeparator: BG;
	Line: string;
	Quoted: BG;
begin
  Result := False;
  ClearRow;
  Quoted := False;
  ColumnIndex := 0;
  while FFile.Readln(Line) do
  begin
    if not Quoted then
    begin
      if Line = '' then Continue; // Empty line

      if (Line[1] = CSVRemark) then
      begin // Remark line
        if (AAcceptRemark = False) then
          Continue;
        Delete(Line, 1, 1); // Remove remark
      end;

      ColumnIndex := 0;
    end;

    Result := True;
    LastIndex := 1;
    InLineIndex := 1;
    FirstOrAfterSeparator := True;
    while InLineIndex <= Length(Line) do
    begin
      case Line[InLineIndex] of
      '"':
      begin
        if (Quoted = False) {and (LastIndex = InLineIndex)} {FirstColumnChar} then
        begin
          if FirstOrAfterSeparator then
          begin
            Quoted := True;
            if LastIndex = InLineIndex then
              Inc(LastIndex) // Skip First Quote
            else
            begin
              Delete(Line, InLineIndex, 1); // Delete quote
              Dec(InLineIndex);
            end;
          end;
        end
        else if Quoted then
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
      end;
      ',', ';', CharTab: // Column separators
      begin
        if Quoted = False then
        begin
          FirstOrAfterSeparator := True;
          AppendRow(ColumnIndex, Copy(Line, LastIndex, InLineIndex - LastIndex));
          Inc(ColumnIndex);
          LastIndex := InLineIndex + 1;
        end;
        // else char is part of value
      end;
      ' ':
      begin

      end
      else
      begin
        FirstOrAfterSeparator := False;
      end;
      end;

      Inc(InLineIndex);
    end;
    AppendRow(ColumnIndex, Copy(Line, LastIndex, InLineIndex - LastIndex));

    if not Quoted then
      Break
    else
    begin
      Result := True;
      FRow[FColumnIndexes[ColumnIndex]] := FRow[FColumnIndexes[ColumnIndex]] + FullSep;
    end;
  end;
end;

function TCSVFile.Open(const FileName: TFileName): BG;
begin
  FRowIndex := 0;
  FErrors := '';
  ClearRow;
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
end;

function TCSVFile.EOF: BG;
begin
	Result := True;
	if Assigned(FFile) and (FFile.Opened) then
  begin
		Result := FFile.Eof;
    if Result = False then
      Result := not ReadNextDataRow;
  end;
end;

function TCSVFile.ReadLine: TArrayOfString;
begin
//  Assert(Length(FRow) > 0);
  Result := FRow;
  SetLength(FRow, 0);
end;

procedure TCSVFile.RemapColumns;
var
	i, j: SG;
begin
  FColumnCount := Length(FColumnNames);
	SetLength(FColumnIndexes, Length(FRow));
  if Length(FRow) > 0 then
  	FillOrderUG(FColumnIndexes[0], Length(FRow));

	for i := 0 to Length(FRow) - 1 do
	begin
		FColumnIndexes[i] := -1;
		for j := 0 to Length(FColumnNames) - 1 do
		begin
			if FColumnNames[j] = FRow[i] then
			begin
				FColumnIndexes[i] := j;
				Break;
			end;
		end;
		if (FColumnIndexes[i] = -1) and (FRow[i] <> '') then
			AddError('Column ' + FRow[i] + ' not used.');
	end;
end;

procedure TCSVFile.AppendRow(const AColumnIndex: SG; const AText: string);
begin
  if AColumnIndex >= Length(FColumnIndexes) then
  begin
    AddError('Skip column index ' +  IntToStr(AColumnIndex) + ' in row ' + IntToStr(FRowIndex + 1));
  end
  else if FColumnIndexes[AColumnIndex] >= 0 then
  begin
    SetLength(FRow, Max(Length(FRow), FColumnIndexes[AColumnIndex] + 1));
    FRow[FColumnIndexes[AColumnIndex]] := FRow[FColumnIndexes[AColumnIndex]] + AText;
  end;
end;

procedure TCSVFile.WriteCSVData(const AFileName: TFileName; const AData: string; const ADelimeter: string = CSVSep);
var
  i: SG;
  Head: string;
begin
	if FileExists(AFileName) = False then
	begin
    Head := CSVRemark;
    for i := 0 to Length(FColumnNames) - 1 do
    begin
      if i <> 0 then
        Head := Head + ADelimeter;
      Head := Head + '"' + FColumnNames[i] + '"';
    end;
    Head := Head + FileSep;
		WriteStringToFile(AFileName, Head + AData, False);
	end
	else
	begin
		WriteStringToFile(AFileName, AData, True);

    // TODO : Remap data
{		f := ReadStringFromFile(FFile.FileName);
		LineIndex := 1;
		l := ReadToNewLine(f, LineIndex) + FileSep;
		if l <> s then
		begin
			Delete(f, 1, LineIndex - 1);
			Insert(s, f, 1);
			WriteStringToFile(FileName, f, False);
		end;}
	end;

end;

procedure TCSVFile.SetColumnNames(const Value: array of string);
var
  Count, i: SG;
begin
  Count := Length(Value);
  SetLength(FColumnNames, Count);
  for i := 0 to Count - 1 do
    FColumnNames[i] := Value[i];

	SetLength(FColumnIndexes, Count);
	FillOrderUG(FColumnIndexes[0], Count);
end;

procedure TCSVFile.SetAcceptRemark(const Value: BG);
begin
  FAcceptRemark := Value;
end;

procedure TCSVFile.ClearRow;
var
  i: SG;
begin
  for i := 0 to Length(FRow) - 1 do
    FRow[i] := '';
end;

procedure TCSVFile.AddError(const AMessage: string);
begin
  FErrors := FErrors + AMessage + LineSep;
end;

end.
