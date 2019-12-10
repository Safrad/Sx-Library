unit uExternalApplication;

interface

uses
  SysUtils,
  Winapi.Windows,

  uTypes,
  uTimeSpan,

  uCustomExternalApplication;

type
  TStartupType = (stNormalApplication, stConsoleApplication);

  TConsoleStartupWindow = (cswDefault, cswDetachedProcess, cswNewWindow, cswNoWindow);

  TProcessPriority = (ppDefault, ppLow, ppBelowNormal, ppNormal, ppAboveNormal, ppHigh, ppRealTime);

  TExitCode = U4;

  TExternalApplication = class(TCustomExternalApplication)
  private
    _sd: pointer;
    _sa: PSecurityAttributes;
    FAbortWaitFor: BG;
    FProcessInformation: TProcessInformation;
    FStartupType: TStartupType;
    FProcessPriority: TProcessPriority;
    FWaitForTimeOut: TTimeSpan;
    FConsoleStartupWindow: TConsoleStartupWindow;
    FConsoleCreateNewProcessGroup: BG;
    FCreateSuspended: BG;

    function GetPriorityCreationFlag: DWORD;
    procedure InitializeSecurityAttributes;
    procedure FreeSecurityAttributes;
    function GetConsoleStartupWindowToFlag: DWORD;
    procedure CheckParameters;

    // Properties
    function GetRunning: BG;
    procedure SetStartupType(const Value: TStartupType);
    procedure SetProcessPriority(const Value: TProcessPriority);
    procedure SeWaitFortTimeOut(const Value: TTimeSpan);
    procedure SetConsoleStartupWindow(const Value: TConsoleStartupWindow);
    procedure SetConsoleCreateNewProcessGroup(const Value: BG);
    procedure SetCreateSuspended(const Value: BG);
  protected
    FStartupInfo: TStartupInfo;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property StartupType: TStartupType read FStartupType write SetStartupType;
    property CreateSuspended: BG read FCreateSuspended write SetCreateSuspended;
    property ConsoleStartupWindow: TConsoleStartupWindow read FConsoleStartupWindow write SetConsoleStartupWindow;
    property ConsoleCreateNewProcessGroup: BG read FConsoleCreateNewProcessGroup write SetConsoleCreateNewProcessGroup;
    property ProcessPriority: TProcessPriority read FProcessPriority write SetProcessPriority;

    property WaitForTimeOut: TTimeSpan read FWaitForTimeOut write SeWaitFortTimeOut;

    // Process
    procedure Execute; override;
    procedure Resume;
    procedure Suspend;
    procedure WaitFor; // Wait till not terminated or time out
    procedure Close; override;
    procedure TerminateAndWaitFor;
    procedure AbortWaitFor;

    // Output
    property Running: BG read GetRunning;
    property ProcessInformation: TProcessInformation read FProcessInformation;
  end;

/// <returns>process exit code or throw exception</returns>
function RunAndWaitForApplication(const AFileName: TFileName; const AParameters: string; const ACurrentDirectory: string; const AStartupType: TStartupType): TExitCode;

procedure RaiseExceptionIfError(const AIsOk: Boolean);

implementation

uses
  Math,

  uMainLog,
  uStrings,
  uOperatingSystem,
  uMainTimer,
  uETimeOutException;

function RunAndWaitForApplication(const AFileName: TFileName; const AParameters: string; const ACurrentDirectory: string; const AStartupType: TStartupType): TExitCode;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    ExternalApplication.FileName := AFileName;
    ExternalApplication.Parameters := AParameters;
    ExternalApplication.CurrentDirectory := ACurrentDirectory;
    ExternalApplication.StartupType := AStartupType;

    ExternalApplication.Execute;
    ExternalApplication.CheckErrorCode;
    ExternalApplication.WaitFor;
    Result := ExternalApplication.ExitCode;
  finally
    ExternalApplication.Free;
  end;
end;

procedure RaiseExceptionIfError(const AIsOk: Boolean);
begin
  if not AIsOk then
    RaiseLastOSError;
end;

{ TExternalApplicaton }

procedure TExternalApplication.AbortWaitFor;
begin
  FAbortWaitFor := True;
end;

procedure TExternalApplication.FreeSecurityAttributes;
begin
  if (_sa <> nil) then
    GlobalFree(dword(_sa));
  if (_sd <> nil) then
    GlobalFree(dword(_sd));
end;

procedure TExternalApplication.InitializeSecurityAttributes;
begin
  if OperatingSystem.IsNT then
  begin
    _sd := pointer(GlobalAlloc(GPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
    RaiseExceptionIfError(InitializeSecurityDescriptor(_sd, SECURITY_DESCRIPTOR_REVISION));
    RaiseExceptionIfError(SetSecurityDescriptorDacl(_sd, true, nil, false));
    _sa := PSecurityAttributes(GlobalAlloc(GPTR, sizeof(TSecurityAttributes)));
    _sa.nLength := sizeof(TSecurityAttributes);
    _sa.lpSecurityDescriptor := _sd;
    _sa.bInheritHandle := true;
  end
  else
  begin
    _sd := nil;
    _sa := nil;
  end;
end;

procedure TExternalApplication.CheckParameters;
begin
  if FConsoleCreateNewProcessGroup and (FConsoleStartupWindow = cswNewWindow) then
    raise EArgumentException.Create('Can not combine "ConsoleCreateNewProcessGroup" with "ConsoleStartupWindow.cswNewWindow"');
end;

procedure TExternalApplication.Close;
begin
  if FStartupType = stConsoleApplication then
  begin
    if not GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, FProcessInformation.dwProcessId) then
      RaiseLastOSError;
  end
  else
    inherited;
end;

constructor TExternalApplication.Create;
begin
  inherited;

  FStartupType := stNormalApplication;
  FWaitForTimeOut.Seconds := 1;

  FStartupInfo.cb := SizeOf(FStartupInfo);
  InitializeSecurityAttributes;
end;

destructor TExternalApplication.Destroy;
begin
  try
    FreeSecurityAttributes;
  finally
    inherited;
  end;
end;

procedure TExternalApplication.Execute;
var
  CreationFlags: DWORD;
begin
  inherited;

  CheckParameters;

	FStartupInfo.dwFlags := FStartupInfo.dwFlags or STARTF_USESHOWWINDOW;
  FStartupInfo.wShowWindow := FStartupWindowState.ToWindowsAPIParameter;
  InitializeSecurityAttributes;

  CreationFlags := GetPriorityCreationFlag or CREATE_UNICODE_ENVIRONMENT;
  if FCreateSuspended then
    CreationFlags := CreationFlags or CREATE_SUSPENDED;

  if FStartupType = stConsoleApplication then
  begin
    CreationFlags := CreationFlags or GetConsoleStartupWindowToFlag;
    if FConsoleCreateNewProcessGroup then
      CreationFlags := CreationFlags or CREATE_NEW_PROCESS_GROUP;

{    FStartupInfo.dwFlags := FStartupInfo.dwFlags or STARTF_USESIZE or STARTF_USECOUNTCHARS or STARTF_USEFILLATTRIBUTE;
    FStartupInfo.dwXCountChars := 128;
    FStartupInfo.dwYCountChars := 1024;
    FStartupInfo.dwXSize := FStartupInfo.dwXCountChars * 8;
    FStartupInfo.dwYSize := 400; // StartupInfo.dwYCountChars * 8;
    FStartupInfo.dwFillAttribute := FOREGROUND_INTENSITY or BACKGROUND_BLUE;}
  end;

  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('CreateProcess ' + FFileName + ' ' + FParameters, mlDebug);
  if CreateProcess(
    nil,
    PChar(JoinFileNameAndParameters(FFileName, FParameters)),
    _sa,
    nil,
    True,
    CreationFlags,
    nil,
    PChar(fCurrentDirectory),
    FStartupInfo,
    FProcessInformation) then
  begin
    FHandle := FProcessInformation.hProcess;
  end
  else
  begin
    FHandle := INVALID_HANDLE_VALUE;
    FErrorCode := GetLastError;
  end;
end;

function TExternalApplication.GetConsoleStartupWindowToFlag: DWORD;
begin
  case FConsoleStartupWindow of
  cswDetachedProcess: Result := DETACHED_PROCESS;
  cswNewWindow: Result := CREATE_NEW_CONSOLE;
  cswNoWindow: Result := CREATE_NO_WINDOW;
  else // cswDefault
    Result := 0;
  end;
end;

function TExternalApplication.GetPriorityCreationFlag: DWORD;
begin
  case FProcessPriority of
  ppLow: Result := IDLE_PRIORITY_CLASS;
  ppBelowNormal: Result := BELOW_NORMAL_PRIORITY_CLASS;
  ppNormal: Result := NORMAL_PRIORITY_CLASS;
  ppAboveNormal: Result := ABOVE_NORMAL_PRIORITY_CLASS;
  ppHigh: Result := HIGH_PRIORITY_CLASS;
  ppRealTime: Result := REALTIME_PRIORITY_CLASS;
  else
    Result := 0;
  end;
end;

function TExternalApplication.GetRunning: BG;
var
  ExitCode: TExitCode;
begin
  if FHandle = INVALID_HANDLE_VALUE then
    Result := False
  else
  begin
    ExitCode := GetExitCode;
    Result := ExitCode = STILL_ACTIVE;
  end;
end;

procedure TExternalApplication.Resume;
begin
  ResumeThread(FProcessInformation.hThread);
end;

procedure TExternalApplication.Suspend;
begin
  SuspendThread(FProcessInformation.hThread);
end;

procedure TExternalApplication.SetConsoleCreateNewProcessGroup(const Value: BG);
begin
  FConsoleCreateNewProcessGroup := Value;
end;

procedure TExternalApplication.SetConsoleStartupWindow(const Value: TConsoleStartupWindow);
begin
  FConsoleStartupWindow := Value;
end;

procedure TExternalApplication.SetCreateSuspended(const Value: BG);
begin
  FCreateSuspended := Value;
end;

procedure TExternalApplication.SetProcessPriority(const Value: TProcessPriority);
begin
  FProcessPriority := Value;
end;

procedure TExternalApplication.SetStartupType(const Value: TStartupType);
begin
  FStartupType := Value;
end;

procedure TExternalApplication.SeWaitFortTimeOut(const Value: TTimeSpan);
begin
  FWaitForTimeOut := Value;
end;

procedure TExternalApplication.TerminateAndWaitFor;
begin
  Terminate;
end;

procedure BreakableWaitForSingleObject(const AHandle: THandle; const ATimeOut: TTimeSpan; var AAbort: BG);
var
  StartTime: TTimeSpan;
  SleepTime: TTimeSpan;
  RemainTimeInTicks: S8;
  Result: DWORD;
  MaximalSleepTime: TTimeSpan;
begin
  Assert(AHandle <> INVALID_HANDLE_VALUE);
  if AHandle = INVALID_HANDLE_VALUE then
  begin
    Exit;
  end;

  MaximalSleepTime.Milliseconds := LoopSleepTime;

  StartTime := MainTimer.Value;
  RemainTimeInTicks := ATimeOut.Ticks;
  while True do
  begin
    if AAbort then
    begin
      AAbort := False;
      raise EAbort.Create('Wait internally abadoned.');
    end;
    SleepTime.Ticks := Min(S8(MaximalSleepTime.Ticks), RemainTimeInTicks);

    Result := WaitForSingleObject(AHandle, SleepTime.Milliseconds);
    if Result = WAIT_TIMEOUT then
    begin
      RemainTimeInTicks := S8(ATimeOut.Ticks) - S8(MainTimer.IntervalFrom(StartTime).Ticks);
      if RemainTimeInTicks <= 0 then
      begin
        raise ETimeOutException.Create('Time out (' + ATimeOut.ToStringInSeconds +  '). Waiting for event aborted.');
      end;
      Continue
    end
    else if Result = WAIT_OBJECT_0 then
      Break
    else if Result = WAIT_FAILED then
      raise Exception.Create('Wait failed.')
    else if Result = WAIT_ABANDONED then
      raise Exception.Create('Wait abadoned.')
    else
      raise EArgumentException.Create('Invalid WaitForSingleObject result.');
  end;
end;

procedure TExternalApplication.WaitFor;
begin
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogEnter('ExternalApplication.WaitFor');

  BreakableWaitForSingleObject(FHandle, FWaitForTimeOut, FAbortWaitFor);

  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.LogLeave('ExternalApplication.WaitFor');
end;

end.
