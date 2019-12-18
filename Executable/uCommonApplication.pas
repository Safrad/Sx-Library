unit uCommonApplication;

interface

uses
  uFirst,
  uTypes,
  uTimeSpan,
  uArguments,
  uApplicationStatistics,
  uApplicationModuleManager,
  uCustomSplashScreen;

type
  TCommonApplication = class
  strict private
    // Input
    FArguments: TArguments;
    FModuleManager: TApplicationModuleManager;
    FRestartAfterClose: BG;
    FSplashScreen: TCustomSplashScreen;

    // Output
    FInitialized: BG;
    FTerminated: BG;
    FStatistics: TApplicationStatistics;
    FInitializationSequenceTime: TTimeSpan;

    // Local
    procedure RestartIfNeeded;

    // Properties
    procedure SetArguments(const Value: TArguments);
    procedure SetRestartAfterClose(const Value: BG);
    procedure SetSplashScreen(const Value: TCustomSplashScreen);
  protected
    procedure InitializeMainLog; virtual;
    procedure InitializeMainIni; virtual;
    procedure InitializeLocalMainIni; virtual;
    procedure InitializeStatistics; virtual;
    procedure InitializeDictionary; virtual;
    procedure InitializeArguments; virtual;
    procedure InitializeModules; virtual;
    procedure AddArguments; virtual;
    procedure AddModules; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure OnRun; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    // Process
    procedure Run; virtual;
    procedure Terminate; virtual;

    /// <summary>
    /// Runs the with exception. For testing purposes.
    /// </summary>
    procedure RunWithException;

    // Input
    property Arguments: TArguments read FArguments write SetArguments;
    property ModuleManager: TApplicationModuleManager read FModuleManager;
    property RestartAfterClose: BG read FRestartAfterClose write SetRestartAfterClose;
    property SplashScreen: TCustomSplashScreen read FSplashScreen write SetSplashScreen;

    // Output
    property Initialized: BG read FInitialized;
    property Terminated: BG read FTerminated;
    property Statistics: TApplicationStatistics read FStatistics;
    property InitializationSequenceTime: TTimeSpan read FInitializationSequenceTime;
  end;

var
  CommonApplication: TCommonApplication;

implementation

uses
  SysUtils,
  uMainLog,
  uMainLogDecorator,
{$ifdef MSWINDOWS}
  uExternalApplication,
{$endif}
  uDefaultArguments,
  uCustomArgument,
  uProjectInfo,
  uMsg,
  uChar,
  uFiles,
  uSystemPaths,
  uDIniFile,
  uDictionary,
  uMainTimer;

{ TCommonApplication }

procedure TCommonApplication.AddArguments;
begin
  // No Code
end;

procedure TCommonApplication.AddModules;
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
    if FModuleManager <> nil then
    begin
      FModuleManager.UnloadModules;
      FreeAndNil(FModuleManager);
    end;
    FreeAndNil(FArguments);
    FreeAndNil(Dictionary);
    FreeAndNil(FStatistics);
    FreeAndNil(LocalMainIni);
    FreeAndNil(MainIni);
    FreeAndNil(MainLog);
  finally
    RestartIfNeeded;
  end;
end;

procedure TCommonApplication.Initialize;
begin
  if Assigned(SplashScreen) then
  begin
    FSplashScreen.Show;
    FSplashScreen.AddMessage('Starting initialization sequence.');
  end;
  InitializeMainLog;
  InitializeMainIni;
  InitializeLocalMainIni;
  InitializeStatistics;
  InitializeDictionary;
  InitializeArguments;
  InitializeModules;
end;

procedure TCommonApplication.InitializeArguments;
begin
  FArguments := TDefaultArguments.Create;
  FArguments.Name := 'Command-Line Parameters';
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
  LocalMainIniFileName := SystemPaths.LocalAppDataDir + GetProjectInfo(piInternalName) + '.ini';
  CreateDirsEx(ExtractFilePath(LocalMainIniFileName)); // InitializeLog creates the same directory, but can be overriden
  LocalMainIni := TDIniFile.Create(LocalMainIniFileName);
end;

procedure TCommonApplication.InitializeMainIni;
var
  MainIniFileName: TFileName;
begin
  MainIniFileName := SystemPaths.AppDataDir + GetProjectInfo(piInternalName) + '.ini';
  CreateDirsEx(ExtractFilePath(MainIniFileName));
  MainIni := TDIniFile.Create(MainIniFileName);
end;

procedure TCommonApplication.InitializeMainLog;
begin
  uMainLogDecorator.InitializeMainLog;
end;

procedure TCommonApplication.InitializeModules;
begin
  FModuleManager := TApplicationModuleManager.Create;
  AddModules;
  FModuleManager.LoadModules;
end;

procedure TCommonApplication.InitializeStatistics;
begin
  FStatistics := TApplicationStatistics.Create;
end;

procedure TCommonApplication.RestartIfNeeded;
{$ifdef MSWINDOWS}
var
  ExternalApplication: TExternalApplication;
begin
  if FRestartAfterClose then
  begin
    ExternalApplication := TExternalApplication.Create;
    try
      ExternalApplication.FileName := SystemPaths.ExeFileName;
      ExternalApplication.Parameters := SystemPaths.ExeParameters;
      ExternalApplication.CurrentDirectory := GetCurrentDir;
      ExternalApplication.KeepRunning := True;
      ExternalApplication.Execute;
    finally
      ExternalApplication.Free;
    end;
  end;
{$else}
begin
  // TODO : Implement
{$endif}
end;

procedure TCommonApplication.Run;
begin
  if not FInitialized then
    Exit;
  try
    FInitializationSequenceTime.Ticks := MainTimer.IntervalFrom(ApplicationStartTicks);
    if Assigned(FSplashScreen) then
      FSplashScreen.AddMessage('Initialization sequence done in ' + FInitializationSequenceTime.ToStringInSeconds + ' seconds.');
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

procedure TCommonApplication.SetSplashScreen(const Value: TCustomSplashScreen);
begin
  FSplashScreen := Value;
end;

procedure TCommonApplication.Terminate;
begin
  FTerminated := True;
end;

end.
