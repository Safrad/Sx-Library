unit uRestartCommand;

interface

uses
  uExitCommand;

type
  TRestartCommand = class(TExitCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uCommonApplication;

{ TRestartCommand }

constructor TRestartCommand.Create;
begin
  inherited;

  Description := 'Restart application.';
end;

procedure TRestartCommand.ExecuteNoParam;
begin
  CommonApplication.RestartAfterClose := True;

  inherited;
end;

end.
