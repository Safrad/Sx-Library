unit uExitCommand;

interface

uses
  uCommonApplication,
  uSimpleCommand;

type
  TExitCommand = class(TSimpleCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

  TQuitCommand = class(TExitCommand);

implementation

{ TExitCommand }

constructor TExitCommand.Create;
begin
  inherited;

  Description := 'Terminate application.';
end;

procedure TExitCommand.ExecuteNoParam;
begin
  inherited;

  CommonApplication.Terminate;
end;

end.
