unit uTable;

interface

uses
  uDIniFile,
  uTypes,
  uRow,
  uCell,

  Generics.Collections,

  uITable,
  uICell,
  uIRow;

type
  TRows = TList<IRow>;

//  TTable = class(ITable)
  TTable = class(TInterfacedObject, ITable)
  private
    FHeader: IRow;
    FData: TRows;
//    function GetColumnCount: SG; override;
//    function GetRowCount: SG; override;
    function GetColumnCount: SG;
    function GetRowCount: SG;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Free; reintroduce;

    // Process
    property Data: TRows read FData;
    procedure AddHeaderRow(const ARow: IRow);

{    procedure Clear; override;
    procedure AddRow(const ARow: IRow); override;
    procedure RemoveRow(const ARowIndex: SG); override;
    function GetCell(const AColumnIndex: SG; const ARowIndex: SG): ICell; override;
    procedure SetCell(const AColumnIndex: SG; const ARowIndex: SG; const ACell: ICell); override;
    function GetColumnName(const AColumnIndex: SG): string; override;}
    procedure Clear;
    procedure AddRow(const ARow: IRow);
    function GetRow(const ARowIndex: SG): IRow;
    procedure RemoveRow(const ARowIndex: SG);
    function GetCell(const AColumnIndex: SG; const ARowIndex: SG): ICell;
    procedure SetCell(const AColumnIndex: SG; const ARowIndex: SG; const ACell: ICell);
    function GetColumnName(const AColumnIndex: SG): string;
    procedure Serialize(const IniFile: TDIniFile; const Section: string; const Save: BG);

    // Output
    property ColumnCount: SG read GetColumnCount;
    property RowCount: SG read GetRowCount;
  end;

implementation

uses
  SysUtils,
  Math,

  uEscape;

{ TTable }

procedure TTable.AddHeaderRow(const ARow: IRow);
begin
  FHeader := ARow as TRow;
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

  FData := TRows.Create;
//  FData.OwnsObjects := True;
end;

destructor TTable.Destroy;
begin
  try
    FData.Free;
    FHeader :=nil;
  finally
    inherited;
  end;
end;

procedure TTable.Free;
begin
  // No code, interface
end;

function TTable.GetCell(const AColumnIndex, ARowIndex: SG): ICell;
begin
  Result := FData[ARowIndex].GetCell(AColumnIndex);
{  Result.ForegroundColor := GetColor(Cell.TextColor);
  Result.BackgroundColor := 0;}
end;

function TTable.GetColumnCount: SG;
begin
  Result := FHeader.GetColumnCount;
{  if Length(Data) > 0 then
  begin
    if Data[0] = nil then
      Result := 0
    else
      Result := Length(Data[0].Columns);
  end
  else
  begin
    Result := 0;
  end;}
end;

function TTable.GetColumnName(const AColumnIndex: SG): string;
begin
  Result := FHeader.GetCell(AColumnIndex).GetData;
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
  FillChar(Cell, SizeOf(Cell), 0);
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

procedure TTable.SetCell(const AColumnIndex, ARowIndex: SG; const ACell: ICell);
begin
//  FData[ARowIndex].Columns[AColumnIndex] := ACell as TCell;
  FData[ARowIndex].SetCell(AColumnIndex, ACell);

//  Cell := FData[ARowIndex].Columns[AColumnIndex];
//  Cell.TextLines.Text := ACell.GetData;
//  Cell.ForegroundColor := GetColor(ACell.ForegroundColor); TODO
end;

end.
