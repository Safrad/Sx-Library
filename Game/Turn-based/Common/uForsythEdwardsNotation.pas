unit uForsythEdwardsNotation;

interface

uses
  uChar;

type
  TForsythEdwardsNotation = class
  public
    const
      None = '-';
      Separator = CharSpace;
    class function SideColorToChar(const ASideString: string): Char;
  end;

implementation

uses
  uStrings;

{ TForsythEdwardsNotation }

class function TForsythEdwardsNotation.SideColorToChar(const ASideString: string): Char;
begin
  Result := LowCase(FirstChar(ASideString));
end;

end.
