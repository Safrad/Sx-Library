unit uCommonApplication;

interface

uses
  uFirst,
  uTypes,
  uArguments,
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
    procedure InitializeMainLog; virtual;
    procedure InitializeMainIni; virtual;
    procedure InitializeLocalMainIni; virtual;
    procedure InitializeStatistics; virtual;
    procedure InitializeDictionary; virtual;
    procedure InitializeArguments; virtual;
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
  uDictionary,
  uMainTimer;

{ TCommonApplication }

procedure TCommonApplication.AddArguments;
begin
  // No Code
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
  try
    FArguments.Free;
    FreeAndNil(Dictionary);
    FStatistics.Free;
    FreeAndNil(LocalMainIni);
    FreeAndNil(MainIni);
    FreeAndNil(MainLog);
  finally
    RestartIfNeeded;
  end;
end;

procedure TCommonApplication.Initialize;
begin
  InitializeMainLog;
  InitializeMainIni;
  InitializeLocalMainIni;
  InitializeStatistics;
  InitializeDictionary;
  InitializeArguments;
end;

procedure TCommonApplication.InitializeArguments;
begin
  FArguments := TDefaultArguments.Create;
  AddArguments;
  FArguments.Parse;
  if FArguments.ShowRequired <> '' then
    raise EArgumentException.Create(FArguments.ShowRequired);
  if FArguments.Check <> '' then
    raise EArgumentException.Create(FArguments.Check);
end;

procedure TCommonApplication.InitializeDictionary;
begin
  Dictionary := TDictionary.Create;
end;

procedure TCommonApplication.InitializeLocalMainIni;
var
  LocalMainIniFileName: TFileName;
begin
  LocalMainIniFileName := LocalAppDataDir + GetProjectInfo(piInternalName) + '.ini';
  CreateDirsEx(ExtractFilePath(LocalMainIniFileName)); // InitializeLog creates the same directory, but can be overriden
  LocalMainIni := TDIniFile.Create(LocalMainIniFileName);
end;

procedure TCommonApplication.InitializeMainIni;
var
  MainIniFileName: TFileName;
begin
  MainIniFileName := AppDataDir + GetProjectInfo(piInternalName) + '.ini';
  CreateDirsEx(ExtractFilePath(MainIniFileName));
  MainIni := TDIniFile.Create(MainIniFileName);
end;

procedure TCommonApplication.InitializeMainLog;
begin
  InitializeLog;
  MainLog.Add('Initialization sequence time [s]: ' + FloatToStr(MainTimer.IntervalFrom(ApplicationStartTicks) / MainTimer.Frequency), mlDebug);
end;

procedure TCommonApplication.InitializeStatistics;
begin
  FStatistics := TApplicationStatistics.Create;
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
