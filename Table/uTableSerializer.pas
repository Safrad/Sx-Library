unit uTableSerializer;

interface

uses
  uTypes,
  uDIniFile,
  uTable;

type
  TTableSerializer = class
  private
    FIniFile: TDIniFile;
    FSection: string;
    FTable: TTable;
    procedure SetIniFile(const Value: TDIniFile);
    procedure SetSection(const Value: string);
    procedure SetTable(const Value: TTable);
  public
    property Table: TTable read FTable write SetTable;
    property IniFile: TDIniFile read FIniFile write SetIniFile;
    property Section: string read FSection write SetSection;
    procedure Serialize(const Save: BG);
  end;

implementation

uses
  SysUtils,

  uEscape,
  uCell,
  uRow;

{ TTableSerializer }

procedure TTableSerializer.Serialize(const Save: BG);
var
	SectionIndex, ValueIndex: SG;
	RowIndex, ColumnIndex: SG;
  Cell: TCell;
  Row: TRow;
  C: SG;
begin
	if Save = False then
	begin
		C := FIniFile.ReadNum(FSection, 'Count', 0);
		SectionIndex := FIniFile.GetSectionIndex(Section);
		if SectionIndex >= 0 then
		begin
			for RowIndex := 0 to C - 1 do
			begin
        Row := TRow.Create(FTable.ColumnCount);
				for ColumnIndex := 0 to FTable.ColumnCount - 1 do
				begin
					ValueIndex := IniFile.GetValueIndex(SectionIndex, FTable.GetColumnName(ColumnIndex) + IntToStr(RowIndex));
					if ValueIndex >= 0 then
					begin
            Cell := TCell.Create;
            Cell.Text := RemoveEscape(FIniFile.GetKeyValue(SectionIndex, ValueIndex));
            Row.SetCell(ColumnIndex, Cell);
					end;
				end;
        FTable.AddRow(Row);
			end;
		end;
	end
	else
	begin
		FIniFile.WriteNum(FSection, 'Count', FTable.RowCount);
		for RowIndex := 0 to FTable.RowCount - 1 do
		begin
			for ColumnIndex := 0 to FTable.ColumnCount - 1 do
			begin
				FIniFile.WriteString(
          FSection,
          FTable.GetColumnName(ColumnIndex) + IntToStr(RowIndex),
          AddEscape(FTable.GetCell(ColumnIndex, RowIndex).GetData,
          True));
			end;
		end;
{		if FIniFile.ValueExists(Section, 'Name' + IntToStr(Favorites.Count)) then
			FIniFile.WriteString(Section, 'Name' + IntToStr(Favorites.Count), '');}
	end;
end;

procedure TTableSerializer.SetIniFile(const Value: TDIniFile);
begin
  FIniFile := Value;
end;

procedure TTableSerializer.SetSection(const Value: string);
begin
  FSection := Value;
end;

procedure TTableSerializer.SetTable(const Value: TTable);
begin
  FTable := Value;
end;

end.
