unit uSxStringList;

interface

uses
  Types,
  Classes,

  uTypes;

type
  TSxStringList = class(TStringList)
  private
    FDelimiter: string;
    FDuplicates: TDuplicates;
    FSorted: BG;
    function GetDelimitedTextWithoutQuotes: string;
    procedure SetDelimiter(const Value: string);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetSorted(const Value: BG);
  public
    constructor Create;

    function Get(const AIndex: SG): string;
    procedure Add(const AString: string);

    property Duplicates: TDuplicates read FDuplicates write SetDuplicates;
    property Sorted: BG read FSorted write SetSorted;

    property Delimiter: string read FDelimiter write SetDelimiter;
    property DelimitedTextWithoutQuotes: string read GetDelimitedTextWithoutQuotes;
  end;

implementation

{ TSxStringList }

procedure TSxStringList.Add(const AString: string);
begin
//  inherited Add(PChar(AString));
  inherited Add(AString);
end;

constructor TSxStringList.Create;
begin
  inherited;

//  OwnObjects := False; // string is reference counted
end;

function TSxStringList.Get(const AIndex: SG): string;
begin
//  Result := PChar(Self[AIndex]^);
  Result := inherited Strings[AIndex];
end;

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
      Result := Result + FDelimiter;
  end;
end;

procedure TSxStringList.SetDelimiter(const Value: string);
begin
  FDelimiter := Value;
end;

procedure TSxStringList.SetDuplicates(const Value: TDuplicates);
begin
  FDuplicates := Value;
end;

procedure TSxStringList.SetSorted(const Value: BG);
begin
  FSorted := Value;
end;

end.
