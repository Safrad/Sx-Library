unit uSetScoreCommand;

interface

uses
  uUnsupportedCommand;

type
  TSetScoreCommand = class(TUnsupportedCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
  end;

implementation

{ TSetScoreCommand }

constructor TSetScoreCommand.Create;
begin
  inherited;

  Description :=
    'Instructs the engine to treat future search requests on the current position ' +
    '(also when it is encountered inside a larger search tree) upto the given "Depth" '+
    'as if these result is "Score" centi-Pawn in favor of the side that has the move in this position.';
end;

function TSetScoreCommand.GetSyntax: string;
begin
  Result := '[Score Depth]';
end;

end.


