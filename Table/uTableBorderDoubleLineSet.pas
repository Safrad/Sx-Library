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
    CharDoubleHorizontal,
    CharDoubleVertical,
    CharDoubleVerticalAndRight,
    CharDoubleVerticalAndLeft,
    CharDoubleVerticalAndHorizontal,
    CharDoubleDownAndRight,
    CharDoubleDownAndHorizontal,
    CharDoubleDownAndLeft,
    CharDoubleUpAndRight,
    CharDoubleUpAndHorizontal,
    CharDoubleUpAndLeft);
begin
  Result := data[Value];
end;

end.

