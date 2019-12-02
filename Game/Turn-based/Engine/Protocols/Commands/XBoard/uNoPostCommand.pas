unit uNoPostCommand;

interface

uses
  uSimpleEngineCommand;

type
  TNoPostCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uXBoardEngineOutput;

{ TNoPostCommand }

constructor TNoPostCommand.Create;
begin
  inherited;

  Description := 'Turn off thinking/pondering output.';
end;

procedure TNoPostCommand.ExecuteNoParam;
begin
  inherited;

  TXBoardEngineOutput(InternalEngine.Output).Enabled := False;
end;

end.

