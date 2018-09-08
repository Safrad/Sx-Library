unit uCommonApplication;

interface

uses
  uTypes,
  uArguments;

type
  TCommonApplication = class
  private
    FRestartAfterClose: BG;
    procedure SetArguments(const Value: TArguments);
    procedure SetRestartAfterClose(const Value: BG);
  protected
    FArguments: TArguments;
    procedure AddArguments; virtual; abstract;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure OnRun; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run;

    /// <summary>
    /// Runs the with exception. For testing purposes.
    /// </summary>
    procedure RunWithException;

    property RestartAfterClose: BG read FRestartAfterClose write SetRestartAfterClose;

    property Arguments: TArguments read FArguments write SetArguments;
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
  uAPI,
  uChar,
  uFiles,
  uDIniFile,
  uUsageInfo;

{ TCommonApplication }

constructor TCommonApplication.Create;
begin
  inherited;

  Initialize;
end;

destructor TCommonApplication.Destroy;
begin
  Finalize;

  inherited;
end;

procedure TCommonApplication.Finalize;
begin
  FArguments.Free;

  RWStart(MainIni, True);

  FreeAndNil(MainIni);
  FreeAndNil(LocalMainIni);
  FreeAndNil(MainLog);

  if FRestartAfterClose then
    ShellExecuteDirectNoExitCode(ExeFileName, ExeParameters);
end;

procedure TCommonApplication.Initialize;
begin
  InitializeLog;
  MainIni := TDIniFile.Create(MainIniFileName);
  LocalMainIni := TDIniFile.Create(LocalIniFileName);

  RWStart(MainIni, False);
  TryUploadData;

  if not ValidLicense then
    Exit;

  FArguments := TDefaultArguments.Create;
  AddArguments;
  FArguments.Parse;
  if FArguments.ShowRequired <> '' then
    raise EArgumentException.Create(FArguments.ShowRequired);
  if FArguments.Check <> '' then
    raise EArgumentException.Create(FArguments.Check);
end;

procedure TCommonApplication.Run;
begin
  try
    OnRun;
  except
    on E: Exception do
    begin
      ExitCode := 1;
      Fatal(E);
    end;
  end;
end;

procedure TCommonApplication.RunWithException;
begin
  Arguments.Parse;
  OnRun;
end;

procedure TCommonApplication.SetArguments(const Value: TArguments);
begin
  FArguments := Value;
end;

procedure TCommonApplication.SetRestartAfterClose(const Value: BG);
begin
  FRestartAfterClose := Value;
end;

end.
