unit uSxCustomLineParser;

interface

uses
  uTypes,
  uSxCustomParser;

type
  TSxCustomLineParser = class(TSxCustomParser)
  private
    FLastChar: Char;
    FColumnIndex: SG;
    FLineIndex: SG;
    procedure SetColumnIndex(const Value: SG);
    procedure SetLineIndex(const Value: SG);
  public
    procedure ReadNextChar; override;

    property ColumnIndex: SG read FColumnIndex write SetColumnIndex;
    property LineIndex: SG read FLineIndex write SetLineIndex;
  end;

implementation

uses
  uChar;

{ TSxCustomLineParser }

procedure TSxCustomLineParser.ReadNextChar;
var
  C: Char;
begin
  inherited;

  C := GetActualChar;

  if C = CharLF then
  begin
    Inc(FLineIndex);
    FColumnIndex := 0;
  end
  else if FLastChar = CharCR then
  begin
    Inc(FLineIndex);
    FColumnIndex := 0;
    if C <> CharCR then
      Inc(FColumnIndex);
  end
  else
    Inc(FColumnIndex);

  FLastChar := C;
end;

procedure TSxCustomLineParser.SetColumnIndex(const Value: SG);
begin
  FColumnIndex := Value;
end;

procedure TSxCustomLineParser.SetLineIndex(const Value: SG);
begin
  FLineIndex := Value;
end;

end.
