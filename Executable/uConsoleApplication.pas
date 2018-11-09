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
    FShowVersionInfo: BG;
    procedure WriteVersionInfo;
    procedure SetShowVersionInfo(const Value: BG);
  protected
    procedure Initialize; override;
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
  uProjectInfo,
  uMsg,
  uChar,
  uFiles,
  uDIniFile;

function GetConsoleWindow: HWND; stdcall; external kernel32;

{ TConsoleApplication }

function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  Result := True;
	if LogWarning then
    MainLogAdd('Aborted by user.', mlWarning);
end;

constructor TConsoleApplication.Create;
begin
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True { add } );
  FShowVersionInfo := True;

  inherited;
end;

destructor TConsoleApplication.Destroy;
begin
  inherited;

  Wait;
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, False { remove } );
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
  if TStartState.RunFromIDE then
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
    TConsole.WriteLine(GetProjectInfo(piProductName) + ' [Version ' + GetProjectInfo(piFileVersion) + ']', ccWhite);
    if GetProjectInfo(piFileDescription) <> '' then
      TConsole.WriteLine(GetProjectInfo(piFileDescription), ccLightGray);
    if GetProjectInfo(piLegalCopyright) <> '' then
      TConsole.WriteLine(GetProjectInfo(piLegalCopyright) + CharSpace + GetProjectInfo(piCompanyName), ccGray);
    TConsole.WriteLine('');
  end;
end;

end.
