unit uICell;

interface

uses
  uTypes;

type
  ICell = class // interface(IInterface)
  public
    function GetData: Variant; virtual; abstract;
    procedure SetData(const AData: Variant); virtual; abstract;
  end;

implementation

end.
