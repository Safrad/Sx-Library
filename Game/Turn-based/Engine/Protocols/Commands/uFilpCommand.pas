unit uFilpCommand;

interface

uses
  uSimpleEngineCommand,
  uETypes;

type
  TFlipCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParams; override;
  end;

implementation

{ TFlipCommand }

constructor TFlipCommand.Create;
begin
  inherited;

  Description := 'Change side to move.';
end;

procedure TFlipCommand.ExecuteNoParams;
begin
  inherited;

  InternalEngine.DoMove(NullMoveStr, NullMoveStr);
end;

end.
