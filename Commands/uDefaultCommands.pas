unit uDefaultCommands;

interface

uses
  uCommands;

type
  TDefaultCommands = class(TCommands)
  public
    constructor Create;

  end;

implementation

uses
  uFiles,
  uExitCommand,
  uRestartCommand,
  uHelpCommand,
  uAboutCommand,
  uStateCommand,
  uShowFileCommand;

{ TDefaultCommands }

constructor TDefaultCommands.Create;
var
  FHelpCommand: THelpCommand;
  FExitCommand: TExitCommand;
  FQuitCommand: TQuitCommand;
  FRestartCommand: TRestartCommand;
  FAboutCommand: TAboutCommand;
  FStateCommand: TStateCommand;
  FShowLogCommand,
  FShowIniCommand,
  FShowLocalIniCommand: TShowFileCommand;
begin
  inherited;

  FHelpCommand := THelpCommand.Create;
  FHelpCommand.Commands := Self;
  Add(FHelpCommand);

  FExitCommand := TExitCommand.Create;
  Add(FExitCommand);

  FQuitCommand := TQuitCommand.Create;
  Add(FQuitCommand);

  FRestartCommand := TRestartCommand.Create;
  Add(FRestartCommand);

  FAboutCommand := TAboutCommand.Create;
  Add(FAboutCommand);

  FStateCommand := TStateCommand.Create;
  Add(FStateCommand);

  FShowLogCommand := TShowFileCommand.Create;
  FShowLogCommand.Shortcut := 'ShowLog';
  FShowLogCommand.Description := 'Show log file.';
  FShowLogCommand.FileName := MainLogFileName;
  Add(FShowLogCommand);

  FShowIniCommand := TShowFileCommand.Create;
  FShowIniCommand.Shortcut := 'ShowIni';
  FShowIniCommand.Description := 'Show main configuration file.';
  FShowIniCommand.FileName := MainIniFileName;
  Add(FShowIniCommand);

  FShowLocalIniCommand := TShowFileCommand.Create;
  FShowLocalIniCommand.Shortcut := 'ShowLocalIni';
  FShowLocalIniCommand.Description := 'Show local configuration file.';
  FShowLocalIniCommand.FileName := LocalIniFileName;
  Add(FShowLocalIniCommand);
end;

end.
