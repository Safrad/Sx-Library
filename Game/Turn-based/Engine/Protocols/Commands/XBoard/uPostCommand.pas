unit uPostCommand;

interface

uses
  uSimpleEngineCommand;

type
  TPostCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uXBoardEngineOutput;

{ TPostCommand }

constructor TPostCommand.Create;
begin
  inherited;

  Description := 'Turn on thinking/pondering output.';
end;

procedure TPostCommand.ExecuteNoParam;
begin
  inherited;

  TXBoardEngineOutput(InternalEngine.Output).Enabled := True;
end;

end.

