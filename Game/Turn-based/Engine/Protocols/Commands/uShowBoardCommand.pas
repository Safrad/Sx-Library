unit uShowBoardCommand;

interface

uses
  uSimpleEngineCommand;

type
  TShowBoardCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TShowBoardCommand }

constructor TShowBoardCommand.Create;
begin
  inherited;

  Description := 'Displays board of current position.';
end;

procedure TShowBoardCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.WriteBoardToConsole;
end;

end.
