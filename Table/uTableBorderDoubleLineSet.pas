unit uTableBorderDoubleLineSet;

interface

uses
  uTableBorderSet, uItemType;

type
  TTableBorderDoubleLineSet = class(TTableBorderSet)
  public
    class function Get(const Value: TItemType): string; override;
  end;

implementation

uses
  uUnicodeChar;

{ TTableBorderDoubleLineSet }

class function TTableBorderDoubleLineSet.Get(const Value: TItemType): string;
const
  data: array[TItemType] of Char = (
    TUnicodeChar.DoubleHorizontal,
    TUnicodeChar.DoubleVertical,
    TUnicodeChar.DoubleVerticalAndRight,
    TUnicodeChar.DoubleVerticalAndLeft,
    TUnicodeChar.DoubleVerticalAndHorizontal,
    TUnicodeChar.DoubleDownAndRight,
    TUnicodeChar.DoubleDownAndHorizontal,
    TUnicodeChar.DoubleDownAndLeft,
    TUnicodeChar.DoubleUpAndRight,
    TUnicodeChar.DoubleUpAndHorizontal,
    TUnicodeChar.DoubleUpAndLeft);
begin
  Result := data[Value];
end;

end.

