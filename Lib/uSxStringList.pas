unit uSxStringList;

interface

uses Classes;

type
  TSxStringList = class(TStringList)
  private
    function GetDelimitedTextWithoutQuotes: string;
  public
    property DelimitedTextWithoutQuotes: string read GetDelimitedTextWithoutQuotes;
  end;

implementation

{ TSxStringList }

function TSxStringList.GetDelimitedTextWithoutQuotes: string;
var
  S: string;
  I: Integer;
begin
    Result := '';
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    Result := Result + S;
    if I < Count - 1 then
      Result := Result + Delimiter;
  end;
end;

end.
