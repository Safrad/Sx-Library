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
    procedure Wait;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ShowVersionInfo: BG read FShowVersionInfo write SetShowVersionInfo;
  end;

implementation

uses
  SysUtils,
  uLog,
  uDefaultArguments,
  uStart,
  uLicense,
  uConsole,
  uProjectInfo,
  uMsg,
  uChar,
  uFiles,
  uDIniFile,
  uUsageInfo;

{ TConsoleApplication }

constructor TConsoleApplication.Create;
begin
  FShowVersionInfo := True;

  inherited;
end;

destructor TConsoleApplication.Destroy;
begin
  inherited;

  Wait;
end;

procedure TConsoleApplication.Finalize;
begin
  FArguments.WriteUnused;

  inherited;
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
  if IsDebug then
    Readln;
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
