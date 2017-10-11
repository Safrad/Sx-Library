unit uTableBorderTextSet;

interface

uses
  uTableBorderSet, uItemType, uStrings;

type
  TTableBorderTextSet = class(TTableBorderSet)
  public
    class function Get(const Value: TItemType): string; override;
  end;

implementation

{ TTableBorderTextSet }

class function TTableBorderTextSet.Get(const Value: TItemType): string;
begin
  case Value of
    itHorizontal:
      Result := '-';
    itVertical:
      Result := '|';
  else
    Result := '+';
  end;
end;

end.

