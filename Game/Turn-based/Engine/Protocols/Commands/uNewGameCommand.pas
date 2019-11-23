unit uNewGameCommand;

interface

uses
  uSimpleEngineCommand;

type
  TNewGameCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TNewGameCommand }

constructor TNewGameCommand.Create;
begin
  inherited;

  Description := 'Used when the next search will be from a different game.';
end;

procedure TNewGameCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.SetStartPos;
end;

end.
