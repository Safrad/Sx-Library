unit uTable;

interface

uses
  Generics.Collections,

  uDIniFile,
  uTypes,
  uRow,
  uCell,
  uColumn,

  uITable,
  uICell,
  uIRow;

type
  TRows = TList<IRow>;

//  TTable = class(ITable)
  TTable = class(TInterfacedObject, ITable)
  private
    FCaption: string;
    FColumns: TColumns;
    FData: TRows;
//    function GetColumnCount: SG; override;
//    function GetRowCount: SG; override;
    function GetColumnCount: SG;
    function GetRowCount: SG;
    function GetColumns: TColumns;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Free; reintroduce;

    // Input
    procedure SetCaption(const AValue: string);
    property Data: TRows read FData;
    procedure AddHeaderRow(const ARow: IRow);

    procedure Clear;
    procedure AddRow(const ARow: IRow);
    procedure RemoveRow(const ARowIndex: SG);
    procedure SetCell(const AColumnIndex: SG; const ARowIndex: SG; const ACell: ICell);

    // Output
    function GetCaption: string;
    function GetColumnName(const AColumnIndex: SG): string;
    function GetColumn(const AColumnIndex: SG): TColumn;
    function GetRow(const ARowIndex: SG): IRow;
    function GetCell(const AColumnIndex: SG; const ARowIndex: SG): ICell;
    property ColumnCount: SG read GetColumnCount;
    property RowCount: SG read GetRowCount;
    property Columns: TColumns read GetColumns;

    // Input / Output
    procedure Serialize(const IniFile: TDIniFile; const Section: string; const Save: BG);
    property Caption: string read GetCaption write SetCaption;
  end;

implementation

uses
  SysUtils,
  Math,

  uEscape;

{ TTable }

procedure TTable.AddHeaderRow(const ARow: IRow);
var
  i: SG;
  Column: TColumn;
begin
  for i := 0 to ARow.GetColumnCount - 1 do
  begin
    Column := TColumn.Create;
    try
      Column.Caption := ARow.GetCell(i).GetData;
      FColumns.Add(Column);
    except
      Column.Free;
    end;
  end;
end;

procedure TTable.AddRow(const ARow: IRow);
begin
  FData.Add(ARow as TRow);
end;

procedure TTable.Clear;
begin
  FData.Clear;
end;

constructor TTable.Create;
begin
  inherited Create;

  FColumns := TColumns.Create;
  FData := TRows.Create;
end;

destructor TTable.Destroy;
begin
  try
    FData.Free;
    FColumns.Free;
  finally
    inherited;
  end;
end;

procedure TTable.Free;
begin
  // No code, interface
end;

function TTable.GetCaption: string;
begin
  Result := FCaption;
end;

function TTable.GetCell(const AColumnIndex, ARowIndex: SG): ICell;
begin
  Result := FData[ARowIndex].GetCell(AColumnIndex);
end;

function TTable.GetColumn(const AColumnIndex: SG): TColumn;
begin
  Result := FColumns[AColumnIndex];
end;

function TTable.GetColumnCount: SG;
begin
  Result := FColumns.Count;
end;

function TTable.GetColumnName(const AColumnIndex: SG): string;
begin
  Result := FColumns[AColumnIndex].Caption;
end;

function TTable.GetColumns: TColumns;
begin
  Result := FColumns;
end;

function TTable.GetRow(const ARowIndex: SG): IRow;
begin
  Result := FData[ARowIndex];
end;

function TTable.GetRowCount: SG;
begin
  Result := FData.Count;
end;

procedure TTable.RemoveRow(const ARowIndex: SG);
begin
  FData.Delete(ARowIndex);
end;

procedure TTable.Serialize(const IniFile: TDIniFile; const Section: string; const Save: BG);
var
	SectionIndex, ValueIndex: SG;
	RowIndex, ColumnIndex: SG;
  Cell: TCell;
  Row: TRow;
  C: SG;
begin
	if Save = False then
	begin
		C := IniFile.ReadNum(Section, 'Count', 0);
		SectionIndex := IniFile.GetSectionIndex(Section);
		if SectionIndex >= 0 then
		begin
			for RowIndex := 0 to C - 1 do
			begin
        Row := TRow.Create(ColumnCount);
				for ColumnIndex := 0 to ColumnCount - 1 do
				begin
					ValueIndex := IniFile.GetValueIndex(SectionIndex, GetColumnName(ColumnIndex) + IntToStr(RowIndex));
					if ValueIndex >= 0 then
					begin
            Cell := TCell.Create;
            Cell.Text := RemoveEscape(IniFile.GetKeyValue(SectionIndex, ValueIndex));
            Row.SetCell(ColumnIndex, Cell);
					end;
				end;
        AddRow(Row);
			end;
		end;
	end
	else
	begin
		IniFile.WriteNum(Section, 'Count', RowCount);
		for RowIndex := 0 to RowCount - 1 do
		begin
			for ColumnIndex := 0 to ColumnCount - 1 do
			begin
				IniFile.WriteString(Section, GetColumnName(ColumnIndex) + IntToStr(RowIndex), AddEscape(GetCell(ColumnIndex, RowIndex).GetData, True));
			end;
		end;
{		if IniFile.ValueExists(Section, 'Name' + IntToStr(Favorites.Count)) then
			IniFile.WriteString(Section, 'Name' + IntToStr(Favorites.Count), '');}
	end;
end;

procedure TTable.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

procedure TTable.SetCell(const AColumnIndex, ARowIndex: SG; const ACell: ICell);
begin
//  FData[ARowIndex].Columns[AColumnIndex] := ACell as TCell;
  FData[ARowIndex].SetCell(AColumnIndex, ACell);

//  Cell := FData[ARowIndex].Columns[AColumnIndex];
//  Cell.TextLines.Text := ACell.GetData;
//  Cell.ForegroundColor := GetColor(ACell.ForegroundColor); TODO
end;

end.
