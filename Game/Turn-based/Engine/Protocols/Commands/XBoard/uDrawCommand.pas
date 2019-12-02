unit uDrawCommand;

interface

uses
  uSimpleEngineCommand;

type
  TDrawCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TDrawCommand }

constructor TDrawCommand.Create;
begin
  inherited;

  Description :=
    'The engine''s opponent offers the engine a draw. ' +
    'To accept the draw, send "offer draw". ' +
    'To decline, ignore the offer (that is, send nothing). ' +
    'If you''re playing on ICS, it''s possible for the draw offer to have been withdrawn by the time you accept it, ' +
    'so don''t assume the game is over because you accept a draw offer. ' +
    'Continue playing until tells you the game is over.';
end;

procedure TDrawCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.OpponentRequireDraw;
end;

end.

