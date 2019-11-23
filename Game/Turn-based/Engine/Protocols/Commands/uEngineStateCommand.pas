unit uEngineStateCommand;

interface

uses
  uSimpleEngineCommand;

type
  TEngineStateCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TEngineState }

constructor TEngineStateCommand.Create;
begin
  inherited;

  Description := 'Displays engine state.';
end;

procedure TEngineStateCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.EngineStateToConsole;
end;

end.
