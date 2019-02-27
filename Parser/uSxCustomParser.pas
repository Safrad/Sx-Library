unit uSxCustomParser;

interface

uses
  uTypes,
  uStrings;

type
  TSxCustomParser = class
  protected
    function GetActualChar: Char; virtual; abstract;
    function GetEndOfInput: BG; virtual; abstract;
  public
    property ActualChar: Char read GetActualChar;
    property EndOfInput: BG read GetEndOfInput;

    procedure ReadNextChar; virtual; abstract;
    procedure ReadExpectedChar(const AChars: array of Char);
  end;

implementation

uses
  SysUtils,
  uEParseError;

{ TSxCustomParser }

procedure TSxCustomParser.ReadExpectedChar(const AChars: array of Char);
var
  C: Char;
  i: SG;
begin
  C := ActualChar;
  for i := 0 to Length(AChars) - 1 do
  begin
    if C = AChars[i] then
    begin
      ReadNextChar;
      Exit;
    end;
  end;

  raise EParseError.Create(AChars, C);
end;

end.
