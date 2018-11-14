unit uTableBorderLightSet;

interface

uses
  uTableBorderSet, uItemType;

type
  TTableBorderLightSet = class(TTableBorderSet)
  public
    class function Get(const Value: TItemType): string; override;
  end;

implementation

uses
  uUnicodeChar;

{ TTableBorderLightSet }

class function TTableBorderLightSet.Get(const Value: TItemType): string;
const
  data: array[TItemType] of Char = (
    TUnicodeChar.LightHorizontal,
    TUnicodeChar.LightVertical,
    TUnicodeChar.LightVerticalAndRight,
    TUnicodeChar.LightVerticalAndLeft,
    TUnicodeChar.LightVerticalAndHorizontal,
    TUnicodeChar.LightDownAndRight,
    TUnicodeChar.LightDownAndHorizontal,
    TUnicodeChar.LightDownAndLeft,
    TUnicodeChar.LightUpAndRight,
    TUnicodeChar.LightUpAndHorizontal,
    TUnicodeChar.LightUpAndLeft);
begin
  Result := data[Value];
end;

end.

