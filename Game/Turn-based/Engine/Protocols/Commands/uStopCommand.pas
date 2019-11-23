unit uStopCommand;

interface

uses
  uSimpleEngineCommand;

type
  TStopCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

{ TStopCommand }

constructor TStopCommand.Create;
begin
  inherited;

  Description := 'Stop calculating as soon as possible.';
end;

procedure TStopCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Stop;
end;

end.
