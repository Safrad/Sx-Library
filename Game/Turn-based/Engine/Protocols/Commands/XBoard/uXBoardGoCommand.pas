unit uXBoardGoCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TXBoardGoCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils;

{ TXBoardGoCommand }

constructor TXBoardGoCommand.Create;
begin
  inherited;

  Description :=
    'Leave force mode and set the engine to play the color that is on move. ' +
    'Associate the engine''s clock with the color that is on move, ' +
    'the opponent''s clock with the color that is not on move. ' +
    'Start the engine''s clock. Start thinking and eventually make a move.';
end;

procedure TXBoardGoCommand.Execute(const AParameters: string);
begin
  inherited;

  if AParameters <> '' then
    raise EArgumentException.Create('No parameters required.');

  XBoardProtocol.AutoPlay := True;
  InternalEngine.DoMoveAfterStop := True;
  InternalEngine.Start;
end;

function TXBoardGoCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

