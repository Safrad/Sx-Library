unit uIsReadyCommand;

interface

uses
  uSimpleEngineCommand;

type
  TIsReadyCommand = class(TSimpleEngineCommand)
  public
    constructor Create;
    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uTextType;

{ TIsReadyCommand }

constructor TIsReadyCommand.Create;
begin
  inherited;

  Description :=
    'Used to wait for the engine to be ready again or to ping the engine to find out if it is still alive.';
end;

procedure TIsReadyCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Output.StartWrite;
  try
    InternalEngine.Output.WriteLine('readyok', ccKeyword);
  finally
    InternalEngine.Output.StopWrite;
  end;
end;

end.
