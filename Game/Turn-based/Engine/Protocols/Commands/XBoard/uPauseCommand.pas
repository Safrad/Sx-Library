unit uPauseCommand;

interface

uses
  uSimpleEngineCommand;

type
  TPauseCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TPauseCommand }

constructor TPauseCommand.Create;
begin
  inherited;

  Description :=
    'Puts the engine into a special state where it does not think, ponder, or otherwise consume significant CPU time. '+
    'The current thinking or pondering (if any) is suspended and both player''s clocks are stopped. ' +
    'The only command that the interface may send to the engine while it is in the paused state is "resume". ' +
    'The paused thinking or pondering (if any) resumes from exactly where it left off, ' +
    'and the clock of the player on move resumes running from where it stopped.';
end;

procedure TPauseCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Pause;
end;

end.

