unit uUserMoveCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TUserMoveCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TUserMoveCommand }

constructor TUserMoveCommand.Create;
begin
  inherited;

  Description := 'Do specified move or moves.';
end;

procedure TUserMoveCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.DoMoves(AParameters);
  if XBoardProtocol.AutoPlay then
  begin
    InternalEngine.DoMoveAfterStop := True;
    InternalEngine.Start;
  end;
end;

function TUserMoveCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

