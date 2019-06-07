unit uTable;

interface

uses
  uConsoleColor,
  uTypes, uRow, uCell, uTableBorderSet;

const
  BorderColor = ccWhite;

type
  TRows = array of TRow;

  TTable = class
  private
    FData: TRows;
    FTableBorderSet: TTableBorderSet;
    procedure DataLine(const ASizes: TArrayOfSG; const ARowHeight: SG; const AColumns: TColumns; const ABackgroundColor:
      TConsoleColor);
    function TopHorizontalLine(const ASizes: TArrayOfSG): string;
    function MiddleHorizontalLine(const ASizes: TArrayOfSG): string;
    function BottomHorizontalLine(const ASizes: TArrayOfSG): string;
    function GetHorizontalLineString(const ASizes: TArrayOfSG; ALeftTop: string; AMiddleTop: string; ARightTop: string):
      string;
    function CalculateOptimalWidthOfRows: TArrayOfSG;
    function WordWrapAndCalculateOptimalWidthOfRows: TArrayOfSG;
    function CalculateDataWidthOfRows: TArrayOfSG;
    class procedure CalculateMaximalWidthOfRows(const AResult: TArrayOfSG);
    procedure WordWrapData(const AResult: TArrayOfSG);
    function GetColumnCount: SG;
    procedure SetTableBorderSet(const Value: TTableBorderSet);
  public
    constructor Create(const ARowCount: SG);
    destructor Destroy; override;

    procedure Clear;

    procedure WriteToConsole;
    property Data: TRows read FData;
    property ColumnCount: SG read GetColumnCount;
    property TableBorderSet: TTableBorderSet read FTableBorderSet write SetTableBorderSet;
  end;

implementation

uses
  Math, StrUtils,
  uStrings,
  uCodePage,
  uConsole,
  uTableBorderTextSet,
  uTableBorderDoubleLineSet,
  uItemType;

{ TTable }

function TTable.BottomHorizontalLine(const ASizes: TArrayOfSG): string;
begin
  Result := GetHorizontalLineString(ASizes,
    TableBorderSet.Get(itUpAndRight),
    TableBorderSet.Get(itUpAndHorizontal),
    TableBorderSet.Get(itUpAndLeft));
end;

function Suma(const AValues: TArrayOfSG): SG;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to Length(AValues) - 1 do
  begin
    Inc(Result, AValues[i]);
  end;
end;

function MaxValueIndex(const AValues: TArrayOfSG): SG;
var
  i: SG;
  MaxValue: SG;
begin
  Result := -1;
  MaxValue := MinInt;
  for i := Length(AValues) - 1 downto 0 do
  begin
    if AValues[i] >= MaxValue then
    begin
      MaxValue := AValues[i];
      Result := i;
    end;
  end;
end;

function TTable.CalculateDataWidthOfRows: TArrayOfSG;
var
  rowIndex, columnIndex: SG;
begin
  SetLength(Result, ColumnCount);
  for rowIndex := 0 to Length(Data) - 1 do
  begin
    for columnIndex := 0 to ColumnCount - 1 do
    begin
      if (Data[rowIndex] <> nil) and (columnIndex < Length(Data[rowIndex].Columns)) then
        Result[columnIndex] := Max(Result[columnIndex], Data[rowIndex].Columns[columnIndex].GetMaximalLineLength);
    end;
  end;
end;

class procedure TTable.CalculateMaximalWidthOfRows(const AResult: TArrayOfSG);
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

function TTable.CalculateOptimalWidthOfRows: TArrayOfSG;
begin
  Result := CalculateDataWidthOfRows;
  CalculateMaximalWidthOfRows(Result);
end;

function TTable.WordWrapAndCalculateOptimalWidthOfRows: TArrayOfSG;
begin
  if Length(Data) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Result := CalculateOptimalWidthOfRows;

  WordWrapData(Result);

  // Repeat with word wrapped data
  Result := CalculateOptimalWidthOfRows;
end;

procedure TTable.Clear;
var
  row: Integer;
begin
  for row := 0 to Length(FData) - 1 do
  begin
    FData[row].Free;
  end;
  SetLength(FData, 0);
end;

constructor TTable.Create(const ARowCount: SG);
begin
  SetLength(FData, ARowCount);
  if TConsole.CodePage >= cpUTF7 then
    FTableBorderSet := TTableBorderDoubleLineSet.Create
  else
    FTableBorderSet := TTableBorderTextSet.Create;
end;

procedure TTable.DataLine(const ASizes: TArrayOfSG; const ARowHeight: SG; const AColumns: TColumns; const
  ABackgroundColor: TConsoleColor);
var
  columnIndex: SG;
  lineIndex: SG;
  cell: TCell;
begin
  for lineIndex := 0 to ARowHeight - 1 do
  begin
    TConsole.Write(TableBorderSet.Get(itVertical), BorderColor, ABackgroundColor);
    for columnIndex := 0 to ColumnCount - 1 do
    begin
      cell := AColumns[columnIndex];
      TConsole.WriteAligned(cell.GetLineString(cell.TextAlignment.Vertical, lineIndex, ARowHeight), ASizes[columnIndex], cell.TextAlignment.Horizontal,
        cell.TextColor, ABackgroundColor);
      if columnIndex < ColumnCount - 1 then
        TConsole.Write(TableBorderSet.Get(itVertical), BorderColor, ABackgroundColor);
    end;
    TConsole.WriteLine(TableBorderSet.Get(itVertical), BorderColor, ABackgroundColor);
  end;
end;

destructor TTable.Destroy;
begin
  Clear;
  FTableBorderSet.Free;

  inherited;
end;

function TTable.GetColumnCount: SG;
begin
  if Length(Data) > 0 then
  begin
    if Data[0] = nil then
      Result := 0
    else
      Result := Length(Data[0].Columns);
  end
  else
  begin
    Result := 0;
  end;
end;

function TTable.GetHorizontalLineString(const ASizes: TArrayOfSG; ALeftTop, AMiddleTop, ARightTop: string): string;
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

function TTable.MiddleHorizontalLine(const ASizes: TArrayOfSG): string;
begin
  Result := GetHorizontalLineString(ASizes,
    TableBorderSet.Get(itVerticalAndRight),
    TableBorderSet.Get(itVerticalAndHorizontal),
    TableBorderSet.Get(itVerticalAndLeft));
end;

procedure TTable.SetTableBorderSet(const Value: TTableBorderSet);
begin
  FTableBorderSet := Value;
end;

function TTable.TopHorizontalLine(const ASizes: TArrayOfSG): string;
begin
  Result := GetHorizontalLineString(ASizes,
    TableBorderSet.Get(itDownAndRight),
    TableBorderSet.Get(itDownAndHorizontal),
    TableBorderSet.Get(itDownAndLeft));
end;

procedure TTable.WordWrapData(const AResult: TArrayOfSG);
var
  rowIndex, columnIndex: SG;
begin
  for rowIndex := 0 to Length(Data) - 1 do
  begin
    for columnIndex := 0 to ColumnCount - 1 do
    begin
      if (Data[rowIndex] <> nil) and (columnIndex < Length(Data[rowIndex].Columns)) then
        Data[rowIndex].Columns[columnIndex].WordWrap(AResult[columnIndex]);
    end;
  end;
end;

procedure TTable.WriteToConsole;
var
  sizes: TArrayOfSG;
  row: SG;
  rowColor: TConsoleColor;
begin
  sizes := WordWrapAndCalculateOptimalWidthOfRows;
  rowColor := ccGray;
  TConsole.WriteLine(TopHorizontalLine(sizes), BorderColor, rowColor);
  for row := 0 to Length(FData) - 1 do
  begin
    if row > 0 then
      rowColor := ccBlack;

    if FData[row] <> nil then
      DataLine(sizes, FData[row].GetHeight, FData[row].Columns, rowColor);
      
    if row < Length(FData) - 1 then
      TConsole.WriteLine(MiddleHorizontalLine(sizes), BorderColor, rowColor);
  end;
  TConsole.WriteLine(BottomHorizontalLine(sizes), BorderColor, rowColor);
end;

end.
