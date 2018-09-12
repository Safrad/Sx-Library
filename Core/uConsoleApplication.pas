{
	Usability in "Project file" (*.dpr):

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

}

unit uConsoleApplication;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  uTypes,
  uCommonApplication,
  uArguments;

type
  TConsoleApplication = class(TCommonApplication)
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
  uDefaultArguments,
  uStart,
  uConsole,
  uProjectInfo,
  uMsg,
  uChar,
  uFiles,
  uDIniFile,
  uUsageInfo;

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
  WriteVersionInfo;

  inherited;
end;

procedure TConsoleApplication.SetShowVersionInfo(const Value: BG);
begin
  FShowVersionInfo := Value;
end;

procedure TConsoleApplication.Wait;
begin
  if DebugHook <> 0 then
  begin
    // Run from IDE
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
    if GetProjectInfo(piLegalCopyright) <> '' then
      TConsole.WriteLine(GetProjectInfo(piLegalCopyright) + CharSpace + GetProjectInfo(piCompanyName), ccGray);
    TConsole.WriteLine('');
  end;
end;

end.
