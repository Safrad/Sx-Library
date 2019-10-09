unit uRow;

interface

uses
  Generics.Collections,

  uTypes,
  uCell,
  uIRow,
  uICell;

type
  TColumns = TObjectList<TCell>;

//  TRow = class({TInterfacedObject, }IRow)
  TRow = class(TInterfacedObject, IRow)
  private
    FColumns: TColumns;
  public
    constructor Create(const AColumnCount: SG);
    destructor Destroy; override;

{      function GetCell(const AColumnIndex: SG): ICell; override;
    procedure SetCell(const AColumnIndex: SG; const ACell: ICell); override;
    function GetColumnCount: SG; override;}
    function GetCell(const AColumnIndex: SG): ICell;
    procedure SetCell(const AColumnIndex: SG; const ACell: ICell);
    function GetColumnCount: SG;
    function GetHeight: SG;

    property Columns: TColumns read FColumns;
  end;

  THeaderRow = TRow;

implementation

uses
  Math;

{ TRow }

constructor TRow.Create(const AColumnCount: SG);
var
  i: SG;
begin
  FColumns := TColumns.Create;
  FColumns.OwnsObjects := True;
  FColumns.Count := AColumnCount;
  for i := 0 to FColumns.Count - 1 do
    FColumns[i] := TCell.Create;
end;

destructor TRow.Destroy;
//var
//  i: SG;
begin
  try
    FColumns.Free;
{  for i := 0 to FColumns.Count - 1 do
    FColumns[i].Free; // TODO nil

  SetLength(FColumns, 0);}
  finally
    inherited;
  end;
end;

function TRow.GetCell(const AColumnIndex: SG): ICell;
begin
  Result := FColumns[AColumnIndex];
end;

function TRow.GetColumnCount: SG;
begin
  Result := FColumns.Count;
end;

function TRow.GetHeight: SG;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to FColumns.Count - 1 do
  begin
    Result := Max(FColumns[i].TextLines.LineCount, Result);
  end;
end;

procedure TRow.SetCell(const AColumnIndex: SG; const ACell: ICell);
begin
  FColumns[AColumnIndex] := ACell as TCell;
end;

end.
