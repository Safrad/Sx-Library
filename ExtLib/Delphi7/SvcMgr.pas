
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1995-2001 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit SvcMgr;

{$J+,H+,X+}

interface

uses
  Windows, Messages, SysUtils, Classes, WinSvc;

type

  { TEventLogger }

  TEventLogger = class(TObject)
  private
    FName: String;
    FEventLog: Integer;
  public
    constructor Create(Name: String);
    destructor Destroy; override;
    procedure LogMessage(Message: String; EventType: DWord = 1;
      Category: Word = 0; ID: DWord = 0);
  end;

  { TDependency }

  TDependency = class(TCollectionItem)
  private
    FName: String;
    FIsGroup: Boolean;
  protected
    function GetDisplayName: string; override;
  published
    property Name: String read FName write FName;
    property IsGroup: Boolean read FIsGroup write FIsGroup;
  end;

  { TDependencies }

  TDependencies = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TDependency;
    procedure SetItem(Index: Integer; Value: TDependency);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    property Items[Index: Integer]: TDependency read GetItem write SetItem; default;
  end;

{ TServiceThread }

const

  CM_SERVICE_CONTROL_CODE = WM_USER + 1;

type

  TService = class;

  TServiceThread = class(TThread)
  private
    FService: TService;
  protected
    procedure Execute; override;
  public
    constructor Create(Service: TService);
    procedure ProcessRequests(WaitForMessage: Boolean);
  end;

  { TService }

  TServiceController = procedure(CtrlCode: DWord); stdcall;

  TServiceType = (stWin32, stDevice, stFileSystem);

  TCurrentStatus = (csStopped, csStartPending, csStopPending, csRunning,
    csContinuePending, csPausePending, csPaused);

  TErrorSeverity = (esIgnore, esNormal, esSevere, esCritical);

  TStartType = (stBoot, stSystem, stAuto, stManual, stDisabled);

  TServiceEvent = procedure(Sender: TService) of object;
  TContinueEvent = procedure(Sender: TService; var Continued: Boolean) of object;
  TPauseEvent = procedure(Sender: TService; var Paused: Boolean) of object;
  TStartEvent = procedure(Sender: TService; var Started: Boolean) of object;
  TStopEvent = procedure(Sender: TService; var Stopped: Boolean) of object;

  TService = class(TDataModule)
  private
    FAllowStop: Boolean;
    FAllowPause: Boolean;
    FDependencies: TDependencies;
    FDisplayName: String;
    FErrCode: DWord;
    FErrorSeverity: TErrorSeverity;
    FEventLogger: TEventLogger;
    FInteractive: Boolean;
    FLoadGroup: String;
    FParams: TStringList;
    FPassword: String;
    FServiceStartName: String;
    FServiceThread: TServiceThread;
    FServiceType: TServiceType;
    FStartType: TStartType;
    FStatus: TCurrentStatus;
    FStatusHandle: THandle;
    FTagID: DWord;
    FWaitHint: Integer;
    FWin32ErrorCode: DWord;
    FBeforeInstall: TServiceEvent;
    FAfterInstall: TServiceEvent;
    FBeforeUninstall: TServiceEvent;
    FAfterUninstall: TServiceEvent;
    FOnContinue: TContinueEvent;
    FOnExecute: TServiceEvent;
    FOnPause: TPauseEvent;
    FOnShutdown: TServiceEvent;
    FOnStart: TStartEvent;
    FOnStop: TStopEvent;
    LastStatus: TCurrentStatus;
    CheckPoint: DWORD;
    function GetDisplayName: String;
    function GetParamCount: Integer;
    function GetParam(Index: Integer): String;
    procedure SetStatus(Value: TCurrentStatus);
    procedure SetDependencies(Value: TDependencies);
    function GetNTDependencies: String;
    function GetNTServiceType: Integer;
    function GetNTStartType: Integer;
    function GetNTErrorSeverity: Integer;
    function GetNTControlsAccepted: Integer;
    procedure SetOnContinue(Value: TContinueEvent);
    procedure SetOnPause(Value: TPauseEvent);
    procedure SetOnStop(Value: TStopEvent);
    function GetTerminated: Boolean;
    function AreDependenciesStored: Boolean;
    procedure SetInteractive(Value: Boolean);
    procedure SetPassword(const Value: string);
    procedure SetServiceStartName(const Value: string);
  protected
    procedure Main(Argc: DWord; Argv: PLPSTR);
    procedure Controller(CtrlCode: DWord);
    procedure DoStart; virtual;
    function DoStop: Boolean; virtual;
    function DoPause: Boolean; virtual;
    function DoContinue: Boolean; virtual;
    procedure DoInterrogate; virtual;
    procedure DoShutdown; virtual;
    function DoCustomControl(CtrlCode: DWord): Boolean; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer); override;
    destructor Destroy; override;
    function GetServiceController: TServiceController; virtual; abstract;
    procedure ReportStatus;
    procedure LogMessage(Message: String; EventType: DWord = 1;
      Category: Integer = 0; ID: Integer = 0);
    property ErrCode: DWord read FErrCode write FErrCode;
    property ParamCount: Integer read GetParamCount;
    property Param[Index: Integer]: String read GetParam;
    property ServiceThread: TServiceThread read FServiceThread;
    property Status: TCurrentStatus read FStatus write SetStatus;
    property Terminated: Boolean read GetTerminated;
    property Win32ErrCode: DWord read FWin32ErrorCode write FWin32ErrorCode;
  published
    property AllowStop: Boolean read FAllowStop write FAllowStop default True;
    property AllowPause: Boolean read FAllowPause write FAllowPause default True;
    property Dependencies: TDependencies read FDependencies write SetDependencies stored AreDependenciesStored;
    property DisplayName: String read GetDisplayName write FDisplayName;
    property ErrorSeverity: TErrorSeverity read FErrorSeverity write FErrorSeverity default esNormal;
    property Interactive: Boolean read FInteractive write SetInteractive default False;
    property LoadGroup: String read FLoadGroup write FLoadGroup;
    property Password: String read FPassword write SetPassword;
    property ServiceStartName: String read FServiceStartName write SetServiceStartName;
    property ServiceType: TServiceType read FServiceType write FServiceType default stWin32;
    property StartType: TStartType read FStartType write FStartType default stAuto;
    property TagID: DWord read FTagID write FTagID default 0;
    property WaitHint: Integer read FWaitHint write FWaitHint default 5000;
    property BeforeInstall: TServiceEvent read FBeforeInstall write FBeforeInstall;
    property AfterInstall: TServiceEvent read FAfterInstall write FAfterInstall;
    property BeforeUninstall: TServiceEvent read FBeforeUninstall write FBeforeUninstall;
    property AfterUninstall: TServiceEvent read FAfterUninstall write FAfterUninstall;
    property OnContinue: TContinueEvent read FOnContinue write SetOnContinue;
    property OnExecute: TServiceEvent read FOnExecute write FOnExecute;
    property OnPause: TPauseEvent read FOnPause write SetOnPause;
    property OnShutdown: TServiceEvent read FOnShutdown write FOnShutdown;
    property OnStart: TStartEvent read FOnStart write FOnStart;
    property OnStop: TStopEvent read FOnStop write SetOnStop;
  end;

  { TServiceApplication }

  TServiceApplication = class(TComponent)
  private
    FEventLogger: TEventLogger;
    FTitle: string;
    procedure OnExceptionHandler(Sender: TObject; E: Exception);
    function GetServiceCount: Integer;
  protected
    procedure DoHandleException(E: Exception); dynamic;
    procedure RegisterServices(Install, Silent: Boolean);
    procedure DispatchServiceMain(Argc: DWord; Argv: PLPSTR);
    function Hook(var Message: TMessage): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ServiceCount: Integer read GetServiceCount;
    // The following uses the current behaviour of the IDE module manager
    procedure CreateForm(InstanceClass: TComponentClass; var Reference); virtual;
    procedure Initialize; virtual;
    procedure Run; virtual;
    property Title: string read FTitle write FTitle;
  end;

var
  Application: TServiceApplication = nil;

implementation

uses
  Forms, Dialogs, Consts;

{ TEventLogger }

constructor TEventLogger.Create(Name: String);
begin
  FName := Name;
  FEventLog := 0;
end;

destructor TEventLogger.Destroy;
begin
  if FEventLog <> 0 then
    DeregisterEventSource(FEventLog);
  inherited Destroy;
end;

procedure TEventLogger.LogMessage(Message: String; EventType: DWord;
  Category: Word; ID: DWord);
var
  P: Pointer;
begin
  P := PChar(Message);
  if FEventLog = 0 then
    FEventLog := RegisterEventSource(nil, PChar(FName));
  ReportEvent(FEventLog, EventType, Category, ID, nil, 1, 0, @P, nil);
end;

{ TDependency }

function TDependency.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name else
    Result := inherited GetDisplayName;
end;

{ TDependencies }

constructor TDependencies.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TDependency);
end;

function TDependencies.GetItem(Index: Integer): TDependency;
begin
  Result := TDependency(inherited GetItem(Index));
end;

procedure TDependencies.SetItem(Index: Integer; Value: TDependency);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TDependencies.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TServiceThread }

constructor TServiceThread.Create(Service: TService);
begin
  FService := Service;
  inherited Create(True);
end;

procedure TServiceThread.Execute;
var
  msg: TMsg;
  Started: Boolean;
begin
  PeekMessage(msg, 0, WM_USER, WM_USER, PM_NOREMOVE); { Create message queue }
  try
    FService.Status := csStartPending;
    Started := True;
    if Assigned(FService.OnStart) then FService.OnStart(FService, Started);
    if not Started then Exit;
    try
      FService.Status := csRunning;
      if Assigned(FService.OnExecute) then
        FService.OnExecute(FService)
      else
        ProcessRequests(True);
      ProcessRequests(False);
    except
      on E: Exception do
        FService.LogMessage(Format(SServiceFailed,[SExecute, E.Message]));
    end;
  except
    on E: Exception do
      FService.LogMessage(Format(SServiceFailed,[SStart, E.Message]));
  end;
end;

procedure TServiceThread.ProcessRequests(WaitForMessage: Boolean);
const
  ActionStr: array[1..5] of String = (SStop, SPause, SContinue, SInterrogate,
    SShutdown);
var
  msg: TMsg;
  OldStatus: TCurrentStatus;
  ErrorMsg: String;
  ActionOK, Rslt: Boolean;
begin
  while True do
  begin
    if Terminated and WaitForMessage then break;
    if WaitForMessage then
      Rslt := GetMessage(msg, 0, 0, 0)
    else
      Rslt := PeekMessage(msg, 0, 0, 0, PM_REMOVE);
    if not Rslt then break;
    if msg.hwnd = 0 then { Thread message }
    begin
      if msg.message = CM_SERVICE_CONTROL_CODE then
      begin
        OldStatus := FService.Status;
        try
          ActionOK := True;
          case msg.wParam of
            SERVICE_CONTROL_STOP: ActionOK := FService.DoStop;
            SERVICE_CONTROL_PAUSE: ActionOK := FService.DoPause;
            SERVICE_CONTROL_CONTINUE: ActionOK := FService.DoContinue;
            SERVICE_CONTROL_SHUTDOWN: FService.DoShutDown;
            SERVICE_CONTROL_INTERROGATE: FService.DoInterrogate;
          else
            ActionOK := FService.DoCustomControl(msg.wParam);
          end;
          if not ActionOK then
            FService.Status := OldStatus;
        except
          on E: Exception do
          begin
            if msg.wParam <> SERVICE_CONTROL_SHUTDOWN then
              FService.Status := OldStatus;
            if msg.wParam in [1..5] then
              ErrorMsg := Format(SServiceFailed, [ActionStr[msg.wParam], E.Message])
            else
              ErrorMsg := Format(SCustomError,[msg.wParam, E.Message]);
            FService.LogMessage(ErrorMsg);
          end;
        end;
      end else
        DispatchMessage(msg);
    end else
      DispatchMessage(msg);
  end;
end;

{ TService }

constructor TService.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner);
  FWaitHint := 5000;
  FInteractive := False;
  FServiceType := stWin32;
  FParams := TStringList.Create;
  FDependencies := TDependencies.Create(Self);
  FErrorSeverity := esNormal;
  FStartType := stAuto;
  FTagID := 0;
  FAllowStop := True;
  FAllowPause := True;
end;

destructor TService.Destroy;
begin
  FDependencies.Free;
  FParams.Free;
  FEventLogger.Free;
  inherited Destroy;
end;

function TService.GetDisplayName: String;
begin
  if FDisplayName <> '' then
    Result := FDisplayName
  else
    Result := Name;
end;

procedure TService.SetInteractive(Value: Boolean);
begin
  if Value = FInteractive then Exit;
  if Value then
  begin
    Password := '';
    ServiceStartName := '';
  end;
  FInteractive := Value;
end;

procedure TService.SetPassword(const Value: string);
begin
  if Value = FPassword then Exit;
  if Value <> '' then
    Interactive := False;
  FPassword := Value;
end;

procedure TService.SetServiceStartName(const Value: string);
begin
  if Value = FServiceStartName then Exit;
  if Value <> '' then
    Interactive := False;
  FServiceStartName := Value;
end;

procedure TService.SetDependencies(Value: TDependencies);
begin
  FDependencies.Assign(Value);
end;

function TService.AreDependenciesStored: Boolean;
begin
  Result := FDependencies.Count > 0;
end;

function TService.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function TService.GetParam(Index: Integer): String;
begin
  Result := FParams[Index];
end;

procedure TService.SetOnContinue(Value: TContinueEvent);
begin
  FOnContinue := Value;
  AllowPause := True;
end;

procedure TService.SetOnPause(Value: TPauseEvent);
begin
  FOnPause := Value;
  AllowPause := True;
end;

procedure TService.SetOnStop(Value: TStopEvent);
begin
  FOnStop := Value;
  AllowStop := True;
end;

function TService.GetTerminated: Boolean;
begin
  Result := False;
  if Assigned(FServiceThread) then
    Result := FServiceThread.Terminated;
end;

function TService.GetNTDependencies: String;
var
  i, Len: Integer;
  P: PChar;
begin
  Result := '';
  Len := 0;
  for i := 0 to Dependencies.Count - 1 do
  begin
    Inc(Len, Length(Dependencies[i].Name) + 1); // For null-terminator
    if Dependencies[i].IsGroup then Inc(Len);
  end;
  if Len <> 0 then
  begin
    Inc(Len); // For final null-terminator;
    SetLength(Result, Len);
    P := @Result[1];
    for i := 0 to Dependencies.Count - 1 do
    begin
      if Dependencies[i].IsGroup then
      begin
        P^ := SC_GROUP_IDENTIFIER;
        Inc(P);
      end;
      P := StrECopy(P, PChar(Dependencies[i].Name));
      Inc(P);
    end;
    P^ := #0;
  end;
end;

function TService.GetNTServiceType: Integer;
const
  NTServiceType: array[TServiceType] of Integer = ( SERVICE_WIN32_OWN_PROCESS,
    SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER);
begin
  Result := NTServiceType[FServiceType];
  if (FServiceType = stWin32) and Interactive then
    Result := Result or SERVICE_INTERACTIVE_PROCESS;
  if (FServiceType = stWin32) and (Application.ServiceCount > 1) then
    Result := (Result xor SERVICE_WIN32_OWN_PROCESS) or SERVICE_WIN32_SHARE_PROCESS;
end;

function TService.GetNTStartType: Integer;
const
  NTStartType: array[TStartType] of Integer = (SERVICE_BOOT_START,
    SERVICE_SYSTEM_START, SERVICE_AUTO_START, SERVICE_DEMAND_START,
    SERVICE_DISABLED);
begin
  Result := NTStartType[FStartType];
  if (FStartType in [stBoot, stSystem]) and (FServiceType <> stDevice) then
    Result := SERVICE_AUTO_START;
end;

function TService.GetNTErrorSeverity: Integer;
const
  NTErrorSeverity: array[TErrorSeverity] of Integer = (SERVICE_ERROR_IGNORE,
    SERVICE_ERROR_NORMAL, SERVICE_ERROR_SEVERE, SERVICE_ERROR_CRITICAL);
begin
  Result := NTErrorSeverity[FErrorSeverity];
end;

function TService.GetNTControlsAccepted: Integer;
begin
  Result := SERVICE_ACCEPT_SHUTDOWN;
  if AllowStop then Result := Result or SERVICE_ACCEPT_STOP;
  if AllowPause then Result := Result or SERVICE_ACCEPT_PAUSE_CONTINUE;
end;

procedure TService.LogMessage(Message: String; EventType: DWord; Category, ID: Integer);
begin
  if FEventLogger = nil then
    FEventLogger := TEventLogger.Create(Name);
  FEventLogger.LogMessage(Message, EventType, Category, ID);
end;

procedure TService.ReportStatus;
const
  NTServiceStatus: array[TCurrentStatus] of Integer = (SERVICE_STOPPED,
    SERVICE_START_PENDING, SERVICE_STOP_PENDING, SERVICE_RUNNING,
    SERVICE_CONTINUE_PENDING, SERVICE_PAUSE_PENDING, SERVICE_PAUSED);
  PendingStatus: set of TCurrentStatus = [csStartPending, csStopPending,
    csContinuePending, csPausePending];
var
  ServiceStatus: TServiceStatus;
begin
  with ServiceStatus do
  begin
    dwWaitHint := FWaitHint;
    dwServiceType := GetNTServiceType;
    if FStatus = csStartPending then
      dwControlsAccepted := 0 else
      dwControlsAccepted := GetNTControlsAccepted;
    if (FStatus in PendingStatus) and (FStatus = LastStatus) then
      Inc(CheckPoint) else
      CheckPoint := 0;
    dwCheckPoint := CheckPoint;
    LastStatus := FStatus;
    dwCurrentState := NTServiceStatus[FStatus];
    dwWin32ExitCode := Win32ErrCode;
    dwServiceSpecificExitCode := ErrCode;
    if ErrCode <> 0 then
      dwWin32ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
    if not SetServiceStatus(FStatusHandle, ServiceStatus) then
      LogMessage(SysErrorMessage(GetLastError));
  end;
end;

procedure TService.SetStatus(Value: TCurrentStatus);
begin
  FStatus := Value;
  if not (csDesigning in ComponentState) then
    ReportStatus;
end;

procedure TService.Main(Argc: DWord; Argv: PLPSTR);
type
  PPCharArray = ^TPCharArray;
  TPCharArray = array [0..1024] of PChar;
var
  i: Integer;
  Controller: TServiceController;
begin
  for i := 0 to Argc - 1 do
    FParams.Add(PPCharArray(Argv)[i]);
  Controller := GetServiceController();
  FStatusHandle := RegisterServiceCtrlHandler(PChar(Name), @Controller);
  if (FStatusHandle = 0) then
    LogMessage(SysErrorMessage(GetLastError)) else
    DoStart;
end;

procedure TService.Controller(CtrlCode: DWord);
begin
  PostThreadMessage(ServiceThread.ThreadID, CM_SERVICE_CONTROL_CODE, CtrlCode, 0);
  if ServiceThread.Suspended then ServiceThread.Resume;
end;

procedure TService.DoStart;
begin
  try
    Status := csStartPending;
    try
      FServiceThread := TServiceThread.Create(Self);
      FServiceThread.Resume;
      FServiceThread.WaitFor;
      FreeAndNil(FServiceThread);
    finally
      Status := csStopped;
    end;
  except
    on E: Exception do
      LogMessage(Format(SServiceFailed,[SExecute, E.Message]));
  end;
end;

function TService.DoStop: Boolean;
begin
  Result := True;
  Status := csStopPending;
  if Assigned(FOnStop) then FOnStop(Self, Result);
  if Result then ServiceThread.Terminate;
end;

function TService.DoPause: Boolean;
begin
  Result := True;
  Status := csPausePending;
  if Assigned(FOnPause) then FOnPause(Self, Result);
  if Result then
  begin
    Status := csPaused;
    ServiceThread.Suspend;
  end;
end;

function TService.DoContinue: Boolean;
begin
  Result := True;
  Status := csContinuePending;
  if Assigned(FOnContinue) then FOnContinue(Self, Result);
  if Result then
    Status := csRunning;
end;

procedure TService.DoInterrogate;
begin
  ReportStatus;
end;

procedure TService.DoShutdown;
begin
  Status := csStopPending;
  try
    if Assigned(FOnShutdown) then FOnShutdown(Self);
  finally
    { Shutdown cannot abort, it must stop regardless of any exception }
    ServiceThread.Terminate;
  end;
end;

function TService.DoCustomControl(CtrlCode: DWord): Boolean;
begin
  Result := True;
end;

{ TServiceApplication }

type
  TServiceClass = class of TService;

procedure ServiceMain(Argc: DWord; Argv: PLPSTR); stdcall;
begin
  Application.DispatchServiceMain(Argc, Argv);
end;

procedure DoneServiceApplication;
begin
  with Forms.Application do
  begin
    if Handle <> 0 then ShowOwnedPopups(Handle, False);
    ShowHint := False;
    Destroying;
    DestroyComponents;
  end;
  with Application do
  begin
    Destroying;
    DestroyComponents;
  end;
end;

constructor TServiceApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventLogger := TEventLogger.Create(ExtractFileName(ParamStr(0)));
  Forms.Application.HookMainWindow(Hook);
end;

destructor TServiceApplication.Destroy;
begin
  FEventLogger.Free;
  Forms.Application.OnException := nil;
  Forms.Application.UnhookMainWindow(Hook);
  inherited Destroy;
end;

procedure TServiceApplication.DispatchServiceMain(Argc: DWord; Argv: PLPSTR);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TService) and
       (AnsiCompareText(PChar(Argv^), Components[i].Name) = 0) then
    begin
      TService(Components[i]).Main(Argc, Argv);
      break;
    end;
end;

function TServiceApplication.GetServiceCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TService then
      Inc(Result);
end;

procedure TServiceApplication.RegisterServices(Install, Silent: Boolean);

  procedure InstallService(Service: TService; SvcMgr: Integer);
  var
    TmpTagID, Svc: Integer;
    PTag, PSSN: Pointer;
    Path: string;
  begin
    Path := ParamStr(0);
    with Service do
    begin
      if Assigned(BeforeInstall) then BeforeInstall(Service);
      TmpTagID := TagID;
      if TmpTagID > 0 then PTag := @TmpTagID else PTag := nil;
      if ServiceStartName = '' then
        PSSN := nil else
        PSSN := PChar(ServiceStartName);
      Svc := CreateService(SvcMgr, PChar(Name), PChar(DisplayName),
        SERVICE_ALL_ACCESS, GetNTServiceType, GetNTStartType, GetNTErrorSeverity,
        PChar(Path), PChar(LoadGroup), PTag, PChar(GetNTDependencies),
        PSSN, PChar(Password));
      TagID := TmpTagID;
      if Svc = 0 then
        RaiseLastOSError;
      try
        try
          if Assigned(AfterInstall) then AfterInstall(Service);
        except
          on E: Exception do
          begin
            DeleteService(Svc);
            raise;
          end;
        end;
      finally
        CloseServiceHandle(Svc);
      end;
    end;
  end;

  procedure UninstallService(Service: TService; SvcMgr: Integer);
  var
    Svc: Integer;
  begin
    with Service do
    begin
      if Assigned(BeforeUninstall) then BeforeUninstall(Service);
      Svc := OpenService(SvcMgr, PChar(Name), SERVICE_ALL_ACCESS);
      if Svc = 0 then RaiseLastOSError;
      try
        if not DeleteService(Svc) then RaiseLastOSError;
      finally
        CloseServiceHandle(Svc);
      end;
      if Assigned(AfterUninstall) then AfterUninstall(Service);
    end;
  end;


var
  SvcMgr: Integer;
  i: Integer;
  Success: Boolean;
  Msg: string;
begin
  Success := True;
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SvcMgr = 0 then RaiseLastOSError;
  try
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TService then
      try
        if Install then
          InstallService(TService(Components[i]), SvcMgr) else
          UninstallService(TService(Components[i]), SvcMgr)
      except
        on E: Exception do
        begin
          Success := False;
          if Install then
            Msg := SServiceInstallFailed else
            Msg := SServiceUninstallFailed;
          with TService(Components[i]) do
            MessageDlg(Format(Msg, [DisplayName, E.Message]), mtError, [mbOK],0);
        end;
      end;
    if Success and not Silent then
      if Install then
        MessageDlg(SServiceInstallOK, mtInformation, [mbOk], 0) else
        MessageDlg(SServiceUninstallOK, mtInformation, [mbOk], 0);
  finally
    CloseServiceHandle(SvcMgr);
  end;
end;

function TServiceApplication.Hook(var Message: TMessage): Boolean;
begin
  Result := Message.Msg = WM_ENDSESSION;
end;

procedure TServiceApplication.CreateForm(InstanceClass: TComponentClass;
  var Reference);
begin
  if InstanceClass.InheritsFrom(TService) then
  begin
    try
      TComponent(Reference) := InstanceClass.Create(Self);
    except
      TComponent(Reference) := nil;
      raise;
    end;
  end else
    Forms.Application.CreateForm(InstanceClass, Reference);
end;

procedure TServiceApplication.DoHandleException(E: Exception);
begin
  FEventLogger.LogMessage(E.Message);
end;

procedure TServiceApplication.Initialize;
begin
  Forms.Application.ShowMainForm :=False;
  Forms.Application.Initialize;
end;

procedure TServiceApplication.OnExceptionHandler(Sender: TObject; E: Exception);
begin
  DoHandleException(E);
end;

type
  TServiceTableEntryArray = array of TServiceTableEntry;

  TServiceStartThread = class(TThread)
  private
    FServiceStartTable: TServiceTableEntryArray;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(Services: TServiceTableEntryArray);
  end;

constructor TServiceStartThread.Create(Services: TServiceTableEntryArray);
begin
  FreeOnTerminate := False;
  ReturnValue := 0;
  FServiceStartTable := Services;
  inherited Create(False);
end;

procedure TServiceStartThread.DoTerminate;
begin
  inherited DoTerminate;
  PostMessage(Forms.Application.Handle, WM_QUIT, 0, 0);
end;

procedure TServiceStartThread.Execute;
begin
  if StartServiceCtrlDispatcher(FServiceStartTable[0]) then
    ReturnValue := 0
  else
    ReturnValue := GetLastError;
end;

procedure TServiceApplication.Run;

  function FindSwitch(const Switch: string): Boolean;
  begin
    Result := FindCmdLineSwitch(Switch, ['-', '/'], True);
  end;

var
  ServiceStartTable: TServiceTableEntryArray;
  ServiceCount, i, J: Integer;
  StartThread: TServiceStartThread;
begin
  AddExitProc(DoneServiceApplication);
  if FindSwitch('INSTALL') then
    RegisterServices(True, FindSwitch('SILENT'))
  else if FindSwitch('UNINSTALL') then
    RegisterServices(False, FindSwitch('SILENT'))
  else
  begin
    Forms.Application.OnException := OnExceptionHandler;
    ServiceCount := 0;
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TService then Inc(ServiceCount);
    SetLength(ServiceStartTable, ServiceCount + 1);
    FillChar(ServiceStartTable[0], SizeOf(TServiceTableEntry) * (ServiceCount + 1), 0);
    J := 0;
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TService then
      begin
        ServiceStartTable[J].lpServiceName := PChar(Components[i].Name);
        ServiceStartTable[J].lpServiceProc := @ServiceMain;
        Inc(J);
      end;
    StartThread := TServiceStartThread.Create(ServiceStartTable);
    try
      while not Forms.Application.Terminated do
        Forms.Application.HandleMessage;
      Forms.Application.Terminate;
      if StartThread.ReturnValue <> 0 then
        FEventLogger.LogMessage(SysErrorMessage(StartThread.ReturnValue));
    finally
      StartThread.Free;
    end;
  end;
end;

procedure InitApplication;
begin
  Application := TServiceApplication.Create(nil);
end;

procedure DoneApplication;
begin
  Application.Free;
  Application := nil;
end;

initialization
  InitApplication;
finalization
  DoneApplication;
end.
