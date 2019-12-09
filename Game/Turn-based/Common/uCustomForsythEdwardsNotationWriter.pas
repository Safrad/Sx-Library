unit uCustomForsythEdwardsNotationWriter;

interface

uses
  uCustomWriter,

  uPosition;

type
  TCustomForsythEdwardsNotationWriter = class(TInterfacedObject, ICustomWriter)
  private
    FPosition: TPosition;
    procedure SetPosition(const Value: TPosition);
  public
    function AsString: string; virtual; abstract;

    property Position: TPosition read FPosition write SetPosition;
  end;

implementation

{ TCustomForsythEdwardsNotationWriter }

procedure TCustomForsythEdwardsNotationWriter.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
end;

end.
