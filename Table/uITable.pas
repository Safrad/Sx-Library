unit uITable;

interface

uses
  uTypes,

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
    procedure Clear;
    procedure AddRow(const ARow: IRow);
    function GetCell(const AColumnIndex: SG; const ARowIndex: SG): ICell;
    procedure SetCell(const AColumnIndex: SG; const ARowIndex: SG; const ACell: ICell);
    function GetColumnCount: SG;
    function GetRow(const ARowIndex: SG): IRow;
    function GetRowCount: SG;
    procedure RemoveRow(const ARowIndex: SG);
    function GetColumnName(const AColumnIndex: SG): string;
  end;

implementation

end.
