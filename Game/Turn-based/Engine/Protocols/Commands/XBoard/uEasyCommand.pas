unit uEasyCommand;

interface

uses
  uSimpleEngineCommand;

type
  TEasyCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TEasyCommand }

constructor TEasyCommand.Create;
begin
  inherited;

  Description := 'Turn off pondering (thinking on the opponent''s time, also known as "permanent brain").';
end;

procedure TEasyCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.CommonOptions.Ponder.Value := False;
end;

end.
