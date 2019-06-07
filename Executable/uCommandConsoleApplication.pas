unit uCommandConsoleApplication;

interface

uses
  SysUtils,

  uTypes,
  uConsoleReader,
  uConsoleApplication,
  uFileNameArgument,
  uCommands,
  uCustomCommand;

type
  TCommandConsoleApplication = class(TConsoleApplication)
  strict private
    FConsoleReader: TConsoleReader;
    FCommands: TCommands;
    FStartupArgument: TFileNameArgument;
    procedure DoCommand(const ACommandAsText: string; const AParameters: string);
    procedure ParseOneLine(const ALine: string);
  protected
    procedure AddArguments; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Wait; override;

    // Can be overridden to handle unknown commands
    procedure ParseText(const AText: string); virtual;
    procedure OnCommandException(const ACommand: TCustomCommand; const AException: Exception); virtual;
  public
    procedure Run; override;
    procedure Terminate; override;

    property Commands: TCommands read FCommands;
  end;

implementation

uses
  uFiles,
  uMsg,
  uLog,
  uChar,
  uAPI,
  uStrings,
  uConsole,
  uCustomArgument,
  uDefaultCommands;

{ TCommandConsoleApplication }

procedure TCommandConsoleApplication.AddArguments;
begin
  inherited;

  FStartupArgument := TFileNameArgument.Create;
  FStartupArgument.Shortcut := 'startup';
  FStartupArgument.Description := 'File with startup commands.';
  FStartupArgument.MustExists := True;
  FStartupArgument.RequireCheck := rcOptional;
  Arguments.Add(FStartupArgument);
end;

procedure TCommandConsoleApplication.DoCommand(const ACommandAsText, AParameters: string);
var
  Command: TCustomCommand;
begin
  Command := nil;
  try
    Command := Commands.FindByStringException(ACommandAsText);
    Command.Execute(AParameters);
    // Command can be removed
    Command := Commands.FindByString(ACommandAsText);
    if (Command <> nil) and (Command.Response <> '') then
      Information(Command.Response);
  except
    on E: Exception do
    begin
      OnCommandException(Command, E);
    end;
  end;
end;

procedure TCommandConsoleApplication.Finalize;
begin
  try
    if FConsoleReader <> nil then
    begin
      FConsoleReader.TerminateAndWaitFor;
      FreeAndNil(FConsoleReader);
    end;

    FCommands.Free;
    FStartupArgument.Free;
  finally
    inherited;
  end;
end;

procedure TCommandConsoleApplication.Initialize;
begin
  TConsole.FlushEveryLine := True;

  inherited;

  FConsoleReader := TConsoleReader.Create;
  if FStartupArgument.Value <> '' then
    FConsoleReader.StartupText := ReadStringFromFile(FStartupArgument.Value);
  FConsoleReader.OnReadInputText := ParseText;

  FCommands := TDefaultCommands.Create;
end;

procedure TCommandConsoleApplication.OnCommandException(const ACommand: TCustomCommand; const AException: Exception);
begin
  Fatal(AException, ACommand);
end;

procedure TCommandConsoleApplication.ParseOneLine(const ALine: string);
const
  CommandParameterSeparator = CharSpace;
  CommentsPrefixes = [';', '#'];
var
  CommandAsText: string;
  Parameters: string;
  InLineIndex: SG;
begin
  if MainLog.IsLoggerFor(mlDebug) then
    MainLogAdd(ProgramInput(ALine), mlDebug);

  if ALine = '' then
    Exit; // Skip empty line

  if CharInSet(ALine[1], CommentsPrefixes) then
    Exit; // Skip comments

  InLineIndex := 1;
  CommandAsText := ReadToChar(ALine, InLineIndex, CommandParameterSeparator);
  SkipSpace(ALine, InLineIndex);
  Parameters := Copy(ALine, InLineIndex);

  DoCommand(CommandAsText, Parameters);
end;

procedure TCommandConsoleApplication.ParseText(const AText: string);
var
  InTextIndex: SG;
begin
  InTextIndex := 1;
  while InTextIndex <= Length(AText) do
  begin
    ParseOneLine(ReadToNewLine(AText, InTextIndex));
  end;
end;

procedure TCommandConsoleApplication.Run;
begin
  if Initialized then
    FConsoleReader.Start;

  inherited;
end;

procedure TCommandConsoleApplication.Terminate;
begin
  inherited;

  FConsoleReader.Terminate;
end;

procedure TCommandConsoleApplication.Wait;
begin
  // No code, do not wait
end;

end.
