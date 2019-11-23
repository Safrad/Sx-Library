unit uResultCommand;

interface

uses
  uEngineCommand;

type
  TResultCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TResultCommand }

constructor TResultCommand.Create;
begin
  inherited;

  Description :=
    'When detects that the game has ended by rule. ' +
    'ResultCode is a PGN result code (1-0, 0-1, or 1/2-1/2), and comment is the reason. ' +
    'Here "by rule" means that the game is definitely over because of what happened on the board. ' +
    'In normal chess, this includes checkmate, stalemate, triple repetition, the 50 move rule, or insufficient material; ' +
    'it does not include loss on time or the like.';
end;

procedure TResultCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.Stop;
end;

function TResultCommand.GetSyntax: string;
begin
  Result := '[ResultCode Comment]';
end;

end.

