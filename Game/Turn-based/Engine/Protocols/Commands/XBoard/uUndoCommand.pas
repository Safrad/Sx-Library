unit uUndoCommand;

interface

uses
  uSimpleEngineCommand;

type
  TUndoCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TUndoCommand }

constructor TUndoCommand.Create;
begin
  inherited;

  Description := 'Back up one move and only in analyze mode start analyzing.';
end;

procedure TUndoCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Undo;
end;

end.

