unit uCommandConsoleApplication;

interface

uses
  uTypes,
  uConsoleReader,
  uConsoleApplication,
  uFileNameArgument,
  uCommands;

type
  TCommandConsoleApplication = class(TConsoleApplication)
  strict private
    FConsoleReader: TConsoleReader;
    FCommands: TCommands;
    FStartupArgument: TFileNameArgument;
    procedure ParseText(const AText: string);
  protected
    procedure AddArguments; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Wait; override;
  public
    procedure Run; override;
    procedure Terminate; override;

    property Commands: TCommands read FCommands;
  end;

implementation

uses
  SysUtils,
  uFiles,
  uMsg,
  uLog,
  uChar,
  uAPI,
  uStrings,
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

procedure TCommandConsoleApplication.Finalize;
begin
  try
    if FConsoleReader <> nil then
    begin
      FConsoleReader.TerminateAndWaitFor;
      FreeAndNil(FConsoleReader);
    end;

    FCommands.Free;
  finally
    inherited;
  end;
end;

procedure TCommandConsoleApplication.Initialize;
begin
  inherited;

  FConsoleReader := TConsoleReader.Create;
  FConsoleReader.StartupText := FStartupArgument.Value;
  FConsoleReader.OnReadInputText := ParseText;

  FCommands := TDefaultCommands.Create;
end;

procedure TCommandConsoleApplication.ParseText(const AText: string);
begin
  FCommands.Parse(AText);
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
