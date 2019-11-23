unit uResumeCommand;

interface

uses
  uSimpleEngineCommand;

type
  TResumeCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TResumeCommand }

constructor TResumeCommand.Create;
begin
  inherited;

  Description := 'Wake up engine from pause mode.';
end;

procedure TResumeCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Resume;
end;

end.

