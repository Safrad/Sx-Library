unit uEvalCommand;

interface

uses
  uSimpleEngineCommand;

type
  TEvalCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TEvalCommand }

constructor TEvalCommand.Create;
begin
  inherited;

  Description := 'Displays evaluation of current position.';
end;

procedure TEvalCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.EvalToConsole;
end;

end.
