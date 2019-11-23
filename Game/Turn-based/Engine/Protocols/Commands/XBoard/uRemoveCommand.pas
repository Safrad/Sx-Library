unit uRemoveCommand;

interface

uses
  uSimpleEngineCommand;

type
  TRemoveCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TRemoveCommand }

constructor TRemoveCommand.Create;
begin
  inherited;

  Description := 'Retract a move. It sends this command only when the user is on move.';
end;

procedure TRemoveCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Undo;
  InternalEngine.Undo;
end;

end.

