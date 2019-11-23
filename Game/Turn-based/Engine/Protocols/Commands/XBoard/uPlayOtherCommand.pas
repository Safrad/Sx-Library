unit uPlayOtherCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TPlayOtherCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TPlayOtherCommand }

constructor TPlayOtherCommand.Create;
begin
  inherited;

  Description :=
    'Leave force mode and set the engine to play the color that is not on move. ' +
    'Associate the opponent''s clock with the color that is on move, the engine''s clock with the color that is not on move. ' +
    'Start the opponent''s clock. If pondering is enabled, the engine should begin pondering. If the engine later receives a move, ' +
    'it should start thinking and eventually reply.';
end;

procedure TPlayOtherCommand.Execute(const AParameters: string);
begin
  inherited;

  XBoardProtocol.AutoPlay := True;
end;

function TPlayOtherCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

