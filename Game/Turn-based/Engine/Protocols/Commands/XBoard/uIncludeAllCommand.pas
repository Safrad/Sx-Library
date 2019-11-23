unit uIncludeAllCommand;

interface

uses
  uSimpleEngineCommand;

type
  TIncludeAllCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TIncludeAllCommand }

constructor TIncludeAllCommand.Create;
begin
  inherited;

  Description := 'All moves are possible, default state. After this command call "Exclude" commands.';
end;

procedure TIncludeAllCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.RootMoves.IncludeAll;
end;

end.

