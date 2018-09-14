unit uHelpCommand;

interface

uses
  uCommands,
  uSimpleCommand;

type
  THelpCommand = class(TSimpleCommand)
  private
    FCommands: TCommands;
    procedure SetCommands(const Value: TCommands);
  public
    constructor Create;

    procedure ExecuteNoParam; override;

    property Commands: TCommands read FCommands write SetCommands;
  end;

implementation

{ THelpCommand }

constructor THelpCommand.Create;
begin
  inherited;

  Description := 'Display this help.';
end;

procedure THelpCommand.ExecuteNoParam;
begin
  inherited;

  if IsConsole then
    Commands.PreviewToConsole
  else
    Response := Commands.PreviewAsString;
end;

procedure THelpCommand.SetCommands(const Value: TCommands);
begin
  FCommands := Value;
end;

end.
