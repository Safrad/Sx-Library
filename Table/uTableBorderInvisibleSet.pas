unit uTableBorderInvisibleSet;

interface

uses
  uTableBorderSet,
  uItemType,
  uStrings;

type
  TTableBorderInvisibleSet = class(TTableBorderSet)
  public
    class function Get(const Value: TItemType): string; override;
  end;

implementation

{ TTableBorderInvisibleSet }

class function TTableBorderInvisibleSet.Get(const Value: TItemType): string;
begin
  Result := CharSpace;
end;

end.

