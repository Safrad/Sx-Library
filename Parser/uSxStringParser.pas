unit uSxStringParser;

interface

uses
  SysUtils,
  uTypes,
  uSxCustomLineParser;

type
  TSxStringParser = class(TSxCustomLineParser)
  private
    FText: string;
    FIndex: SG;
    procedure SetText(const Value: string);
  protected
    function GetActualChar: Char; override;
    function GetEndOfInput: BG; override;
  public
    procedure ReadNextChar; override;

    property Text: string read FText write SetText;
  end;

implementation

{ TSxStringParser }

function TSxStringParser.GetActualChar: Char;
begin
  if GetEndOfInput then
    Result := #0
//    raise Exception.Create('Read after end.');
  else
    Result := FText[FIndex];
end;

function TSxStringParser.GetEndOfInput: BG;
begin
  Result :=  FIndex > Length(FText);
end;

procedure TSxStringParser.ReadNextChar;
begin
  inherited;

  Inc(FIndex);
end;

procedure TSxStringParser.SetText(const Value: string);
begin
  FIndex := 1;
  FText := Value;
end;

end.
