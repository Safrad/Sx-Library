unit uHardCommand;

interface

uses
  uSimpleEngineCommand;

type
  THardCommand = class(TSimpleEngineCommand)
  public
    constructor Create;
    procedure ExecuteNoParam; override;
  end;

implementation

{ THardCommand }

constructor THardCommand.Create;
begin
  inherited;

  Description := 'Turn on pondering (thinking on the opponent''s time, also known as "permanent brain").';
end;

procedure THardCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.CommonOptions.Ponder.Value := True;
end;

end.
