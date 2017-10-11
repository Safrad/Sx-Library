unit uTableBorderLightSet;

interface

uses
  uTableBorderSet, uItemType, uStrings;

type
  TTableBorderLightSet = class(TTableBorderSet)
  public
    class function Get(const Value: TItemType): string; override;
  end;

implementation

uses
  uChar;

{ TTableBorderLightSet }

class function TTableBorderLightSet.Get(const Value: TItemType): string;
const
  data: array[TItemType] of Char = (
    CharLightHorizontal,
    CharLightVertical,
    CharLightVerticalAndRight,
    CharLightVerticalAndLeft,
    CharLightVerticalAndHorizontal,
    CharLightDownAndRight,
    CharLightDownAndHorizontal,
    CharLightDownAndLeft,
    CharLightUpAndRight,
    CharLightUpAndHorizontal,
    CharLightUpAndLeft);
begin
  Result := data[Value];
end;

end.

