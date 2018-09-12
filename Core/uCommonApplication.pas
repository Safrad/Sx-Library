unit uCommonApplication;

interface

uses
  uTypes,
  uArguments;

type
  TCommonApplication = class
  strict private
    FArguments: TArguments;
    FRestartAfterClose: BG;
    FInitialized: BG;
    FTerminated: BG;

    procedure SetArguments(const Value: TArguments);
    procedure SetRestartAfterClose(const Value: BG);
  protected
    procedure AddArguments; virtual; abstract;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure OnRun; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run; virtual;
    procedure Terminate; virtual;

    /// <summary>
    /// Runs the with exception. For testing purposes.
    /// </summary>
    procedure RunWithException;

    property Arguments: TArguments read FArguments write SetArguments;
    property RestartAfterClose: BG read FRestartAfterClose write SetRestartAfterClose;
    property Initialized: BG read FInitialized;
    property Terminated: BG read FTerminated;
  end;

var
  CommonApplication: TCommonApplication;

implementation

uses
  SysUtils,
  uLog,
  uDefaultArguments,
  uStart,
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
  CommonApplication := Self;

  ExitCode := 1;

  inherited;

  try
    if IsDebug then
      Finalize; // For testing

    Initialize;
    FInitialized := True;
  except
    on E: Exception do
      Fatal(E, Self);
  end;
end;

destructor TCommonApplication.Destroy;
begin
  Finalize;

  inherited;
end;

procedure TCommonApplication.Finalize;
begin
  FInitialized := False;

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
  if not FInitialized then
    Exit;
  try
    OnRun;
    ExitCode := 0;
    FArguments.WriteUnused;
  except
    on E: Exception do
    begin
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

procedure TCommonApplication.Terminate;
begin
  FTerminated := True;
end;

end.
