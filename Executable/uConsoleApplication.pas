(*
Example of use:

MyProgram.dpr:

program MyProgram;

uses
  uMain in 'uMain.pas';

{$R *.RES}

var
  Main: TMain;
begin
  Main := TMain.Create;
  try
    Main.Run;
  finally
    Main.Free;
  end;
end.

uMain.pas:
unit uMain;

interface

uses
  uConsoleApplication;

type
  TMain = class(TConsoleApplication)
  private
    ...: TDirectoryArgument;
    ...: TSwitchArgument;
    ...
  protected
    procedure AddArguments; override;
    procedure OnRun; override;
  end;

...
*)

unit uConsoleApplication;

interface

{$APPTYPE CONSOLE}

uses
  uUIApplication,
  uTypes,
  uArguments;

type
  TConsoleApplication = class(TUIApplication)
  private
    FAbortedBySystem: BG;
  protected
    procedure Initialize; override;
    procedure AbortedBySystem; virtual;
    procedure Wait; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run; override;
  end;

implementation

uses
  SysUtils,
{$IF defined(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  uLog,
  uCommonApplication,
  uCommonOutput,
  uConsole,
  uConsoleColor,
  uConsoleOutputInfo,
  uConsoleSplashScreen;

{$IF defined(MSWINDOWS)}
function GetConsoleWindow: HWND; stdcall; external kernel32;

function CtrlTypeToString(const dwCtrlType: DWORD): string;
begin
  case dwCtrlType of
  CTRL_C_EVENT:
    Result := 'Received Ctrl+C signal.';
  CTRL_BREAK_EVENT:
    Result := 'Received Ctrl+Break signal.';
  CTRL_CLOSE_EVENT:
    Result := 'Received close event.'; // User close console window (Ctrl+C, Close button etc.)
  CTRL_LOGOFF_EVENT:
    Result := 'Received logoff event.';
  CTRL_SHUTDOWN_EVENT:
    Result := 'Received shutdown event.';
  else
    Result := 'Received unknown close event.';
  end;
end;

function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  Result := True; // function handles the control signal
	if LogWarning then
    MainLogAdd(CtrlTypeToString(dwCtrlType), mlWarning);
  TConsoleApplication(CommonApplication).AbortedBySystem;
end;
{$ENDIF}

{ TConsoleApplication }

procedure TConsoleApplication.AbortedBySystem;
begin
  FAbortedBySystem := True;
  Terminate;
end;

constructor TConsoleApplication.Create;
begin
{$IF defined(MSWINDOWS)}
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True { add } );
{$ENDIF}
  inherited;
end;

destructor TConsoleApplication.Destroy;
begin
  try
    inherited;
  finally
    Wait;
{$IF defined(MSWINDOWS)}
    SetConsoleCtrlHandler(@ConsoleCtrlHandler, False { remove } );
{$ENDIF}
    CommonOutput := nil; // Interface
  end;
end;

procedure TConsoleApplication.Initialize;
begin
  CommonOutput := TConsoleOutputInfo.Create;

  SplashScreen := TConsoleSplashScreen.Create;
  inherited;

  if FMinimizedArgument.Exists then
    ShowWindow(GetConsoleWindow, SW_MINIMIZE);
end;

procedure TConsoleApplication.Run;
begin
  inherited;

  SplashScreen.Free;
  SplashScreen := nil;
end;

procedure TConsoleApplication.Wait;
begin
  if (not TConsole.IsRedirected) and (not FAbortedBySystem) then
  begin
    TConsole.WriteLine('');
    TConsole.Write('Press Enter to continue...');
    Readln;
  end;
end;

end.
