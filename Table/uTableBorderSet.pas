unit uTableBorderSet;

interface

uses
  uItemType;

type
  TTableBorderSet = class
  public
    class function Get(const Value: TItemType): string; virtual; abstract;
  end;

implementation

{ TTableBorderSet }

end.

