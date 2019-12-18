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
  SysUtils,

  uMainLog,
  uFileLogger,
  uDIniFile,
  uExitCommand,
  uRestartCommand,
  uHelpCommand,
  uAboutCommand,
  uStateCommand,
  uSystemInfoCommand,
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
  FSystemInfoCommand: TSystemInfoCommand;
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

  FSystemInfoCommand := TSystemInfoCommand.Create;
  Add(FSystemInfoCommand);

  if Assigned(MainLog) and (MainLog is TFileLogger) and FileExists(TFileLogger(MainLog).FileName) then
  begin
    FShowLogCommand := TShowFileCommand.Create;
    FShowLogCommand.Shortcut := 'ShowLog';
    FShowLogCommand.Description := 'Show log file.';
    FShowLogCommand.FileName := TFileLogger(MainLog).FileName;
    Add(FShowLogCommand);
  end;

  if Assigned(MainIni) and FileExists(MainIni.FileName) then
  begin
    FShowIniCommand := TShowFileCommand.Create;
    FShowIniCommand.Shortcut := 'ShowIni';
    FShowIniCommand.Description := 'Show main configuration file.';
    FShowIniCommand.FileName := MainIni.FileName;
    Add(FShowIniCommand);
  end;

  if Assigned(LocalMainIni) and FileExists(LocalMainIni.FileName) then
  begin
    FShowLocalIniCommand := TShowFileCommand.Create;
    FShowLocalIniCommand.Shortcut := 'ShowLocalIni';
    FShowLocalIniCommand.Description := 'Show local configuration file.';
    FShowLocalIniCommand.FileName := LocalMainIni.FileName;
    Add(FShowLocalIniCommand);
  end;
end;

end.
