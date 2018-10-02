unit uRow;

interface

uses
  uTypes,
  uCell;

type
  TColumns = array of TCell;

  TRow = class
  private
    FColumns: TColumns;
    public
      constructor Create(const AColumnCount: SG);
      destructor Destroy; override;

      function GetHeight: SG;

      property Columns: TColumns read FColumns;
  end;

implementation

uses
  Math;

{ TRow }

constructor TRow.Create(const AColumnCount: SG);
var
  i: SG;
begin
  SetLength(FColumns, AColumnCount);
  for i := 0 to Length(FColumns) - 1 do
    FColumns[i] := TCell.Create;
end;

destructor TRow.Destroy;
var
  i: SG;
begin
  for i := 0 to Length(FColumns) - 1 do
    FColumns[i].Free;

  SetLength(FColumns, 0);

  inherited;
end;

function TRow.GetHeight: SG;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to Length(FColumns) - 1 do
  begin
    Result := Max(FColumns[i].LineCount, Result);
  end;
end;

end.
