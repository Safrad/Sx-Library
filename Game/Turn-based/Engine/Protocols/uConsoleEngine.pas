unit uConsoleEngine;

interface

uses
  SysUtils,

  uCommandConsoleApplication,
  uCommonProtocol,
  uCustomCommand,
  uInternalEngine;

type
  TConsoleEngine = class(TCommandConsoleApplication)
  private
    FInternalEngine: TInternalEngine;
    FProtocol: TCommonProtocol;
    FUCICommand: TCustomCommand;
    FUSICommand: TCustomCommand;
    FXBoardCommand: TCustomCommand;
    procedure CreateCommands;
    procedure AddCommand(const ACommand: TCustomCommand);
    procedure SetInternalEngine(const Value: TInternalEngine);
    procedure SetProtocol(const Value: TCommonProtocol);
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure ParseText(const AText: string); override;
    procedure OnCommandException(const ACommand: TCustomCommand; const AException: Exception); override;
  public
    property InternalEngine: TInternalEngine read FInternalEngine write SetInternalEngine;
    property Protocol: TCommonProtocol read FProtocol write SetProtocol;
  end;

implementation

uses
  Winapi.Windows,

  uTypes,
  uMsg,
  uConsole,

  uNoProtocolEngineOutput,

  uSimpleEngineCommand,
  uEngineCommand,

  uUCICommand,
  uUSICommand,
  uXBoardCommand,
  uShowBoardCommand,
  uEngineStateCommand,
  uEvalCommand,
  uBenchmarkCommand,
  uPerftCommand,
  uDebugCommand;

{ TConsoleEngine }

procedure TConsoleEngine.AddCommand(const ACommand: TCustomCommand);
begin
  Assert(ACommand is TCustomCommand);
  if ACommand is TSimpleEngineCommand then
  begin
    Assert(FInternalEngine <> nil);
    TSimpleEngineCommand(ACommand).InternalEngine := FInternalEngine;
  end
  else if ACommand is TEngineCommand then
  begin
    Assert(FInternalEngine <> nil);
    TEngineCommand(ACommand).InternalEngine := FInternalEngine;
  end;
  Commands.Add(ACommand);
end;

procedure TConsoleEngine.CreateCommands;
begin
  FUCICommand := TUCICommand.Create;
  TUCICommand(FUCICommand).ConsoleEngine := Self;
  Commands.Add(FUCICommand);

  FUSICommand := TUSICommand.Create;
  TUCICommand(FUSICommand).ConsoleEngine := Self;
  Commands.Add(FUSICommand);

  FXBoardCommand := TXBoardCommand.Create;
  TXBoardCommand(FXBoardCommand).ConsoleEngine := Self;
  Commands.Add(FXBoardCommand);

  AddCommand(TEngineStateCommand.Create);
  AddCommand(TShowBoardCommand.Create);
  AddCommand(TEvalCommand.Create);
  AddCommand(TBenchmarkCommand.Create);
  AddCommand(TPerftCommand.Create);
  AddCommand(TDebugCommand.Create);
end;

procedure TConsoleEngine.Finalize;
begin
  try
    FreeAndNil(FProtocol);
  finally
    inherited;
  end;
end;

procedure TConsoleEngine.Initialize;
begin
  inherited;

  Console.FlushEveryLine := False; // Flush in TProtocolEngineOutput.StopWrite
  Commands.Delete(Commands.FindByString('Exit'));
end;

procedure TConsoleEngine.OnCommandException(const ACommand: TCustomCommand; const AException: Exception);
var
  s: string;
begin
  if not (AException is EAbort) then
  begin
    if ACommand <> nil then
      s := 'Command ' + ACommand.Shortcut + ': '
    else
      s := '';
    s := s + AException.Message;
    if FInternalEngine.Output <> nil then
      FInternalEngine.Output.TellGUIError(s)
    else
      ErrorMsg(s);
  end;
end;

procedure TConsoleEngine.ParseText(const AText: string);
begin
  try
    inherited;
  except
    if (FProtocol = nil) or (not FProtocol.UnknownCommand(AText)) then
      raise;
  end;
end;

procedure TConsoleEngine.SetInternalEngine(const Value: TInternalEngine);
begin
  FInternalEngine := Value;
  if FInternalEngine <> nil then
  begin
    {$WARN CONSTRUCTING_ABSTRACT OFF}
    FInternalEngine.Output := TNoProtocolEngineOutput.Create;
    {$WARN CONSTRUCTING_ABSTRACT ON}
    CreateCommands;
  end;
end;

procedure TConsoleEngine.SetProtocol(const Value: TCommonProtocol);
begin
  if FProtocol <> Value then
  begin
    Commands.Disable(FUCICommand);
    Commands.Disable(FUSICommand);
    Commands.Disable(FXBoardCommand);

    FProtocol := Value;
    Assert(FInternalEngine <> nil);
    FProtocol.InternalEngine := FInternalEngine;
    FProtocol.Initialize;

    Commands.Add(FProtocol.Commands);
  end;
end;

end.
