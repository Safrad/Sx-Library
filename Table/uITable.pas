unit uITable;

interface

uses
  uTypes,
  uColumn,
  uIRow,
  uICell;

type
{  ITable = class
  public
    procedure Clear; virtual; abstract;
    procedure AddRow(const ARow: IRow); virtual; abstract;
    function GetCell(const AColumnIndex: SG; const ARowIndex: SG): ICell; virtual; abstract;
    procedure SetCell(const AColumnIndex: SG; const ARowIndex: SG; const ACell: ICell); virtual; abstract;
    function GetColumnCount: SG; virtual; abstract;
    function GetRowCount: SG; virtual; abstract;
    procedure RemoveRow(const ARowIndex: SG); virtual; abstract;
    function GetColumnName(const AColumnIndex: SG): string; virtual; abstract;
  end;}

  ITable = interface(IInterface)

    // Input
    procedure SetCaption(const AValue: string);
    procedure Clear;
    procedure AddRow(const ARow: IRow);
    procedure RemoveRow(const ARowIndex: SG);
    procedure SetCell(const AColumnIndex: SG; const ARowIndex: SG; const ACell: ICell);

    // Output
    function GetCaption: string;
    function GetCell(const AColumnIndex: SG; const ARowIndex: SG): ICell;
    function GetColumnCount: SG;
    function GetRow(const ARowIndex: SG): IRow;
    function GetRowCount: SG;
    function GetColumnName(const AColumnIndex: SG): string;
    function GetColumn(const AColumnIndex: SG): TColumn;
    function GetColumns: TColumns;
  end;

implementation

end.
