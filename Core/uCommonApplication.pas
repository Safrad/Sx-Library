unit uCommonApplication;

interface

uses
  uTypes,
  uArguments,
  uSwitchArgument,
  uApplicationStatistics;

type
  TCommonApplication = class
  strict private
    FArguments: TArguments;
    FStatistics: TApplicationStatistics;
    FRestartAfterClose: BG;
    FInitialized: BG;
    FTerminated: BG;

    procedure RestartIfNeeded;
    procedure SetArguments(const Value: TArguments);
    procedure SetRestartAfterClose(const Value: BG);
  protected
    FMinimizedArgument: TSwitchArgument;
    procedure AddArguments; virtual;
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
    property Statistics: TApplicationStatistics read FStatistics;
  end;

var
  CommonApplication: TCommonApplication;

implementation

uses
  SysUtils,
  uLog,
  uExternalApplication,
  uDefaultArguments,
  uCustomArgument,
  uConsole,
  uProjectInfo,
  uMsg,
  uChar,
  uFiles,
  uDIniFile,
  uDictionary;

{ TCommonApplication }

procedure TCommonApplication.AddArguments;
begin
  FMinimizedArgument := TSwitchArgument.Create;
  FMinimizedArgument.Shortcut := 'minimized';
  FMinimizedArgument.Description := 'Minimizes application';
  FMinimizedArgument.RequireCheck := rcOptional;
  Arguments.Add(FMinimizedArgument);
end;

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
  try
    FInitialized := False;
    Finalize;
  except
    on E: Exception do
      Fatal(E, Self);
  end;

  inherited;
end;

procedure TCommonApplication.Finalize;
begin
  FArguments.Free;

  FreeAndNil(Dictionary);

  FStatistics.Free;

  FreeAndNil(MainIni);
  FreeAndNil(LocalMainIni);
  FreeAndNil(MainLog);

  RestartIfNeeded;
end;

procedure TCommonApplication.Initialize;
begin
  InitializeLog;
  MainIni := TDIniFile.Create(MainIniFileName);
  LocalMainIni := TDIniFile.Create(LocalIniFileName);

  FStatistics := TApplicationStatistics.Create;

  Dictionary := TDictionary.Create;

  FArguments := TDefaultArguments.Create;
  AddArguments;
  FArguments.Parse;
  if FArguments.ShowRequired <> '' then
    raise EArgumentException.Create(FArguments.ShowRequired);
  if FArguments.Check <> '' then
    raise EArgumentException.Create(FArguments.Check);
end;

procedure TCommonApplication.RestartIfNeeded;
var
  ExternalApplication: TExternalApplication;
begin
  if FRestartAfterClose then
  begin
    ExternalApplication := TExternalApplication.Create;
    try
      ExternalApplication.FileName := ExeFileName;
      ExternalApplication.Parameters := ExeParameters;
      ExternalApplication.Execute;
    finally
      ExternalApplication.Free;
    end;

  end;
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
