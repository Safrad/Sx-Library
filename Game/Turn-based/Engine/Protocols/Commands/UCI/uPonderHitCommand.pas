unit uPonderHitCommand;

interface

uses
  uSimpleEngineCommand;

type
  TPonderHitCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TPonderHitCommand }

constructor TPonderHitCommand.Create;
begin
  inherited;

  Description := 'Used if the engine was told to ponder on the same move.';
end;

procedure TPonderHitCommand.ExecuteNoParam;
begin
  inherited;

  // TODO : Implement
end;

end.
