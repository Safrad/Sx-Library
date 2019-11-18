unit uConsoleTable;

interface

uses
  uTypes,
  uTable,
  uRow,
  uTableBorderSet,
  uConsoleColor;

type
  TConsoleTable = class
  private
    FTableBorderSet: TTableBorderSet;
    FTable: TTable;
    function TopHorizontalLine(const ASizes: TArrayOfSG): string;
    function MiddleHorizontalLine(const ASizes: TArrayOfSG): string;
    function BottomHorizontalLine(const ASizes: TArrayOfSG): string;
    function GetHorizontalLineString(const ASizes: TArrayOfSG; ALeftTop: string; AMiddleTop: string; ARightTop: string):
      string;
    procedure DataLine(const ASizes: TArrayOfSG; const ARowHeight: SG; const AColumns: TColumns; const ABackgroundColor:
      TConsoleColor);
    function CalculateOptimalWidthOfRows: TArrayOfSG;
    function WordWrapAndCalculateOptimalWidthOfRows: TArrayOfSG;
    function CalculateDataWidthOfRows: TArrayOfSG;
    class procedure CalculateMaximalWidthOfRows(const AResult: TArrayOfSG);
    procedure WordWrapData(const AResult: TArrayOfSG);
    procedure SetTableBorderSet(const Value: TTableBorderSet);
    procedure SetTable(const Value: TTable);
  public
    constructor Create;
    destructor Destroy; override;

    property Table: TTable read FTable write SetTable;
    property TableBorderSet: TTableBorderSet read FTableBorderSet write SetTableBorderSet;

    procedure WriteToConsole;
  end;

implementation

uses
  Math,
  StrUtils,

  uMath,
  uCell,
  uTableBorderTextSet,
  uTableBorderDoubleLineSet,
  uItemType,
  uConsole,
  uCodePage;

const
  BorderColor = ccWhite;

{ TConsoleTable }

function TConsoleTable.BottomHorizontalLine(const ASizes: TArrayOfSG): string;
begin
  Result := GetHorizontalLineString(ASizes,
    TableBorderSet.Get(itUpAndRight),
    TableBorderSet.Get(itUpAndHorizontal),
    TableBorderSet.Get(itUpAndLeft));
end;

function TConsoleTable.CalculateDataWidthOfRows: TArrayOfSG;
var
  rowIndex, columnIndex: SG;
begin
  SetLength(Result, FTable.ColumnCount);
  for rowIndex := 0 to FTable.RowCount - 1 do
  begin
    for columnIndex := 0 to FTable.ColumnCount - 1 do
    begin
      if (FTable.Data[rowIndex] <> nil) and (columnIndex < FTable.Data[rowIndex].GetColumnCount) then
        Result[columnIndex] := Max(Result[columnIndex], (FTable.Data[rowIndex].GetCell(columnIndex) as TCell).TextLines.GetMaximalLineLength);
    end;
  end;
end;


class procedure TConsoleTable.CalculateMaximalWidthOfRows(const AResult: TArrayOfSG);
const
  RightSpacing = 1; // Minimum 1 is reguired to disable table row overflow to new line
  BorderSize = 1;
var
  maxIndex: SG;
begin
  while Suma(AResult) + BorderSize + Length(AResult) > TConsole.GetSize.X - RightSpacing do
  begin
    maxIndex := MaxValueIndex(AResult);
    Dec(AResult[maxIndex]);
  end;
end;

function TConsoleTable.CalculateOptimalWidthOfRows: TArrayOfSG;
begin
  Result := CalculateDataWidthOfRows;
  CalculateMaximalWidthOfRows(Result);
end;

constructor TConsoleTable.Create;
begin
  inherited Create;

  if TConsole.CodePage >= cpUTF7 then
    FTableBorderSet := TTableBorderDoubleLineSet.Create
  else
    FTableBorderSet := TTableBorderTextSet.Create;
end;

procedure TConsoleTable.WordWrapData(const AResult: TArrayOfSG);
var
  rowIndex, columnIndex: SG;
begin
  for rowIndex := 0 to FTable.RowCount - 1 do
  begin
    for columnIndex := 0 to FTable.ColumnCount - 1 do
    begin
      if (FTable.Data[rowIndex] <> nil) and (columnIndex < FTable.Data[rowIndex].GetColumnCount) then
        (FTable.Data[rowIndex].GetCell(columnIndex) as TCell).TextLines.WordWrap(AResult[columnIndex]);
    end;
  end;
end;

function TConsoleTable.WordWrapAndCalculateOptimalWidthOfRows: TArrayOfSG;
begin
  if FTable.RowCount = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Result := CalculateOptimalWidthOfRows;

  WordWrapData(Result);

  // Repeat with word wrapped data
  Result := CalculateOptimalWidthOfRows;
end;

procedure TConsoleTable.DataLine(const ASizes: TArrayOfSG; const ARowHeight: SG; const AColumns: TColumns; const
  ABackgroundColor: TConsoleColor);
var
  columnIndex: SG;
  lineIndex: SG;
  cell: TCell;
begin
  for lineIndex := 0 to ARowHeight - 1 do
  begin
    TConsole.Write(TableBorderSet.Get(itVertical), BorderColor, ABackgroundColor);
    for columnIndex := 0 to FTable.ColumnCount - 1 do
    begin
      cell := AColumns[columnIndex];
      TConsole.WriteAligned(cell.TextLines.GetLineString(cell.TextAlignment.Vertical, lineIndex, ARowHeight), ASizes[columnIndex], cell.TextAlignment.Horizontal,
        cell.TextColor, ABackgroundColor);
      if columnIndex < FTable.ColumnCount - 1 then
        TConsole.Write(TableBorderSet.Get(itVertical), BorderColor, ABackgroundColor);
    end;
    TConsole.WriteLine(TableBorderSet.Get(itVertical), BorderColor, ABackgroundColor);
  end;
end;

destructor TConsoleTable.Destroy;
begin
  try
   FTableBorderSet.Free;
  finally
    inherited;
  end;
end;

function TConsoleTable.GetHorizontalLineString(const ASizes: TArrayOfSG; ALeftTop, AMiddleTop, ARightTop: string): string;
var
  i: SG;
begin
  Result := ALeftTop;
  for i := 0 to Length(ASizes) - 1 do
  begin
    Result := Result + DupeString(TableBorderSet.Get(itHorizontal), ASizes[i]);
    if i < Length(ASizes) - 1 then
      Result := Result + AMiddleTop;
  end;
  Result := Result + ARightTop;
end;

function TConsoleTable.MiddleHorizontalLine(const ASizes: TArrayOfSG): string;
begin
  Result := GetHorizontalLineString(ASizes,
    TableBorderSet.Get(itVerticalAndRight),
    TableBorderSet.Get(itVerticalAndHorizontal),
    TableBorderSet.Get(itVerticalAndLeft));
end;

procedure TConsoleTable.SetTable(const Value: TTable);
begin
  FTable := Value;
end;

procedure TConsoleTable.SetTableBorderSet(const Value: TTableBorderSet);
begin
  FTableBorderSet := Value;
end;

function TConsoleTable.TopHorizontalLine(const ASizes: TArrayOfSG): string;
begin
  Result := GetHorizontalLineString(ASizes,
    TableBorderSet.Get(itDownAndRight),
    TableBorderSet.Get(itDownAndHorizontal),
    TableBorderSet.Get(itDownAndLeft));
end;

procedure TConsoleTable.WriteToConsole;
var
  sizes: TArrayOfSG;
  row: SG;
  rowColor: TConsoleColor;
  HeaderRow: TRow;
  column: SG;
  cell: TCell;
begin
  HeaderRow := TRow.Create(FTable.ColumnCount);
  for column := 0 to FTable.ColumnCount - 1 do
  begin
    cell := TCell.Create;
    cell.TextColor := ccBlack;
    cell.SetData(FTable.Columns[column].Caption);
    HeaderRow.SetCell(column, cell);
  end;

  FTable.Data.Insert(0, HeaderRow);
  try
    sizes := WordWrapAndCalculateOptimalWidthOfRows;
    rowColor := ccGray;
    TConsole.WriteLine(TopHorizontalLine(sizes), BorderColor, rowColor);
    for row := 0 to FTable.RowCount - 1 do
    begin
      if row > 0 then
        rowColor := ccBlack;

      if FTable.Data[row] <> nil then
        DataLine(sizes, FTable.Data[row].GetHeight, (FTable.Data[row] as TRow).Columns, rowColor);

      if row < FTable.RowCount - 1 then
        TConsole.WriteLine(MiddleHorizontalLine(sizes), BorderColor, rowColor);
    end;
    TConsole.WriteLine(BottomHorizontalLine(sizes), BorderColor, rowColor);
  finally
    FTable.Data.Delete(0);
  end;
end;

end.
