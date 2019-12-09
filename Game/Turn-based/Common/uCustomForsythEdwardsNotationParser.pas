unit uCustomForsythEdwardsNotationParser;

interface

uses
  uCustomParser,

  uPosition;

type
  TCustomForsythEdwardsNotationParser = class(TInterfacedObject, ICustomParser)
  private
    FPosition: TPosition;
    procedure SetPosition(const Value: TPosition);
  public
    procedure Parse(const AText: string); virtual; abstract;

    property Position: TPosition read FPosition write SetPosition;
  end;

implementation

{ TCustomForsythEdwardsNotationParser }

procedure TCustomForsythEdwardsNotationParser.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
end;

end.
