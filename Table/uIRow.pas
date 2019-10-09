unit uIRow;

interface

uses
  uTypes,
  uICell;

type
{  IRow = class// interface(IInterface)
  public
    function GetCell(const AColumnIndex: SG): ICell; virtual; abstract;
    procedure SetCell(const AColumnIndex: SG; const ACell: ICell); virtual; abstract;
    function GetColumnCount: SG; virtual; abstract;
  end;}
  IRow = interface(IInterface)
    function GetCell(const AColumnIndex: SG): ICell;
    procedure SetCell(const AColumnIndex: SG; const ACell: ICell);
    function GetColumnCount: SG;
    function GetHeight: SG;
  end;

implementation

end.
