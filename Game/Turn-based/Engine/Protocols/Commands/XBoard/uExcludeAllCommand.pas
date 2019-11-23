unit uExcludeAllCommand;

interface

uses
  uSimpleEngineCommand;

type
  TExcludeAllCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TExcludeAllCommand }

constructor TExcludeAllCommand.Create;
begin
  inherited;

  Description := 'No moves are possible. After this command call "Include" commands.';
end;

procedure TExcludeAllCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.RootMoves.ExcludeAll;
end;

end.

