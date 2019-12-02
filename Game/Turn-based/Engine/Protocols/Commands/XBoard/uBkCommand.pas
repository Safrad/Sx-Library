unit uBkCommand;

interface

uses
  uSimpleEngineCommand;

type
  TBkCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TBkCommand }

constructor TBkCommand.Create;
begin
  inherited;

  Description := 'Show book moves from this position, if any.';
end;

procedure TBkCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.BookMoves;
end;

end.

