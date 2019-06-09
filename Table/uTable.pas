unit uTable;

interface

uses
  uConsoleColor,
  uTypes,
  uRow,
  uCell;

type
  TRows = array of TRow;

  TTable = class
  private
    FData: TRows;
    function GetColumnCount: SG;
  public
    constructor Create(const ARowCount: SG);
    destructor Destroy; override;

    procedure Clear;

    property Data: TRows read FData;
    property ColumnCount: SG read GetColumnCount;
  end;

implementation

uses
  Math;

{ TTable }

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
  inherited Create;

  SetLength(FData, ARowCount);
end;

destructor TTable.Destroy;
begin
  try
    Clear;
  finally
    inherited;
  end;
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

end.
