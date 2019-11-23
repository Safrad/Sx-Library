unit uComputerCommand;

interface

uses
  uSimpleEngineCommand;

type
  TComputerCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TComputerCommand }

constructor TComputerCommand.Create;
begin
  inherited;

  Description := 'The opponent is also a computer chess engine.';
end;

procedure TComputerCommand.ExecuteNoParam;
begin
  inherited;

end;

end.

