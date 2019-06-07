unit uHelpCommand;

interface

uses
  uCommands,
  uCustomCommand;

type
  THelpCommand = class(TCustomCommand)
  private
    FCommands: TCommands;
    procedure SetCommands(const Value: TCommands);
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;

    property Commands: TCommands read FCommands write SetCommands;
  end;

implementation

uses
  uConsole,
  uConsoleColor,
  uUnsupportedCommand,
  uStrings,
  uEParseError;

{ THelpCommand }

constructor THelpCommand.Create;
begin
  inherited;

  Description := 'Displays name, parameters and description of all commands or the specific command. Grayed commands are disabled.';
  TConsole.WriteLine('Enter ''help'' for list of all commands.', ccGreen);
end;

procedure THelpCommand.Execute(const AParameters: string);

  procedure OutputText(const AText: string; const AConsoleColor: TConsoleColor = ccLightGray);
  begin
    if IsConsole then
      TConsole.Write(AText, AConsoleColor)
    else
      Response := Response + AText;
  end;

var
  Command: TCustomCommand;
begin
  inherited;

  if AParameters = '' then
  begin
    if IsConsole then
      Commands.PreviewToConsole
    else
      Response := Commands.PreviewAsString;
  end
  else
  begin
    Command := Commands.FindByString(AParameters);
    if Command <> nil then
    begin
      Response := '';
      if Command.Syntax <> '' then
      begin
        OutputText('Name and parameters: ', ccGreen);
        OutputText(Command.GetShortcutAndSyntax + LineSep);
      end;

      if Command.Description <> '' then
      begin
        OutputText(Command.Description + LineSep);
      end;

      if Command is TUnsupportedCommand then
        OutputText('Command is not supported.' + LineSep, ccGray);

      if not Command.Enabled then
      begin
        OutputText('Command is currently disabled.' + LineSep, ccGray);
      end;
    end
    else
    begin
  		raise EParseError.Create(['Valid command'], AParameters);
    end;
  end;
end;

function THelpCommand.GetSyntax: string;
begin
  Result := '[{empty} or a valid command]';
end;

procedure THelpCommand.SetCommands(const Value: TCommands);
begin
  FCommands := Value;
end;

end.
