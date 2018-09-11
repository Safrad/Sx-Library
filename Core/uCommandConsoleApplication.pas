unit uCommandConsoleApplication;

interface

uses
  uTypes,
  uConsoleReader,
  uConsoleApplication,
  uFileNameArgument;

type
  TCommandConsoleApplication = class(TConsoleApplication)
  private
    FTerminated: BG;
    FConsoleReader: TConsoleReader;
    FStartupArgument: TFileNameArgument;
    procedure SetTerminated(const Value: BG);

    procedure ParseText(const AText: string);
    procedure InternalParseCommand(const ACommand, AParameters: string);
  protected
    procedure Finalize; override;
    procedure AddArguments; override;
    function ParseCommand(const ACommand, AParameters: string): BG; virtual; abstract;
    procedure Wait; override;
  public
    constructor Create;

    procedure Initialize; override;

    property Terminated: BG read FTerminated write SetTerminated;
  end;

implementation

uses
  Windows,
  SysUtils,
  uFiles,
  uMsg,
  uLog,
  uChar,
  uAPI,
  uStrings,
  uCustomArgument;

function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  Result := True;
	if LogWarning then
    MainLogAdd('Aborted by user.', mlWarning);
end;

{ TCommandConsoleApplication }

procedure TCommandConsoleApplication.AddArguments;
begin
  inherited;

  FStartupArgument := TFileNameArgument.Create;
  FStartupArgument.Shortcut := 'startup';
  FStartupArgument.Description := 'File with startup commands.';
  FStartupArgument.MustExists := True;
  FStartupArgument.RequireCheck := rcOptional;
  FArguments.Add(FStartupArgument);
end;

constructor TCommandConsoleApplication.Create;
begin
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True { add } );

  inherited;
end;

procedure TCommandConsoleApplication.Finalize;
begin
  if FConsoleReader <> nil then
  begin
    FConsoleReader.TerminateAndWaitFor;
    FreeAndNil(FConsoleReader);
  end;

  inherited;
end;

procedure TCommandConsoleApplication.Initialize;
begin
  inherited;

  FConsoleReader := TConsoleReader.Create;
  FConsoleReader.StartupText := FStartupArgument.Value;
  FConsoleReader.OnReadInputText := ParseText;
  FConsoleReader.Start;
end;

procedure TCommandConsoleApplication.InternalParseCommand(const ACommand, AParameters: string);
begin
  if (ACommand = 'quit') or (ACommand = 'exit') then
  begin
    FConsoleReader.Terminate;
    Terminated := True
  end
  else if (ACommand = 'showlog') then
  begin
    APIOpen(MainLogFileName);
  end
  else if (ACommand = 'showini') then
  begin
    APIOpen(MainIniFileName);
  end
  else if (ACommand = 'showlocalini') then
  begin
    APIOpen(LocalIniFileName);
  end
  else if (ACommand = 'restart') then
  begin
    RestartAfterClose := True;
    FConsoleReader.Terminate;
    Terminated := True
  end
  else
  begin
    try
      if not ParseCommand(ACommand, AParameters) then
      begin
        Warning('Unknown command: ' + ACommand);
      end;
    except
      on E: Exception do
        Fatal(E, Self);
    end;
  end;
end;

procedure TCommandConsoleApplication.ParseText(const AText: string);
var
	Command, Parameters: string;
	InLineIndex: SG;
begin
  InLineIndex := 1;
  while InLineIndex <= Length(AText) do
  begin
    Command := ReadToChars(AText, InLineIndex, [CharSpace, CharCR]);
    Parameters := ReadToNewLine(AText, InLineIndex);
    InternalParseCommand(LowerCase(Command), Parameters);
  end;
end;

procedure TCommandConsoleApplication.SetTerminated(const Value: BG);
begin
  FTerminated := Value;
end;

procedure TCommandConsoleApplication.Wait;
begin
  // No code, do not wait
end;

end.
