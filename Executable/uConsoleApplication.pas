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
    FShowVersionInfo: BG;
    procedure WriteVersionInfo;
    procedure SetShowVersionInfo(const Value: BG);
  protected
    procedure Initialize; override;
    procedure AbortedBySystem; virtual;
    procedure Wait; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property ShowVersionInfo: BG read FShowVersionInfo write SetShowVersionInfo;
  end;

implementation

uses
  SysUtils,
  Windows,
  uLog,
  uStartState,
  uDefaultArguments,
  uCodePage,
  uConsole,
  uConsoleColor,
  uProjectInfo,
  uMsg,
  uChar,
  uFiles,
  uDIniFile,
  uCommonApplication;

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
  end;
end;

function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  Result := True; // function handles the control signal
	if LogWarning then
    MainLogAdd(CtrlTypeToString(dwCtrlType), mlWarning);
  TConsoleApplication(CommonApplication).AbortedBySystem;
end;

{ TConsoleApplication }

procedure TConsoleApplication.AbortedBySystem;
begin
  FAbortedBySystem := True;
  Terminate;
end;

constructor TConsoleApplication.Create;
begin
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True { add } );
  FShowVersionInfo := True;

  inherited;
end;

destructor TConsoleApplication.Destroy;
begin
  try
    inherited;
  finally
    Wait;
    SetConsoleCtrlHandler(@ConsoleCtrlHandler, False { remove } );
  end;
end;

procedure TConsoleApplication.Initialize;
begin
  {$ifdef UNICODE}
  TConsole.CodePage := cpUTF8;
  {$endif}
  WriteVersionInfo;

  inherited;

  if FMinimizedArgument.Exists then
    ShowWindow(GetConsoleWindow, SW_MINIMIZE);
end;

procedure TConsoleApplication.SetShowVersionInfo(const Value: BG);
begin
  FShowVersionInfo := Value;
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

procedure TConsoleApplication.WriteVersionInfo;
begin
  if FShowVersionInfo then
  begin
    TConsole.WriteLine(GetProjectInfo(piProductName) + ' [Version ' + GetProjectInfo(piProductVersion) + ']', ccWhite);
    if GetProjectInfo(piFileDescription) <> '' then
      TConsole.WriteLine(GetProjectInfo(piFileDescription), ccLightGray);
    if GetProjectInfo(piLegalCopyright) <> '' then
      TConsole.WriteLine(GetProjectInfo(piLegalCopyright) + CharSpace + GetProjectInfo(piCompanyName), ccGray);
    TConsole.WriteLine('');
  end;
end;

end.
