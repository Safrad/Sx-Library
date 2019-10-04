unit uCustomExternalApplication;

interface

uses
  SysUtils,
  Winapi.Windows,

  uTypes,
  uStartupWindowState,
  uTImeSpan;

type
  TExitCode = U4;

  TCustomExternalApplication = class
  private
    FKeepRunning: BG;
    FUpdateProcessMemoryInfoTime: TTimeSpan;
    FAllocatedMemory: U8;
    FAllocatedMemoryPeak: U8;

    procedure UpdateProcessMemoryInfo;
    // Properties
    function GetAllocatedMemory: U8;
    function GetAllocatedMemoryPeak: U8;
    procedure SetFileName(const Value: TFileName);
    procedure SetParameters(const Value: string);
    procedure SetCurrentDirectory(const Value: string);
    procedure SetStartupWindowState(const Value: TStartupWindowState);
    procedure SetAllowOnlyOneInstance(const Value: BG);
    procedure SetKeepRunning(const Value: BG);
    function GetProcessId: DWORD;
  protected
    // Input
    FFileName: TFileName;
    FParameters: string;
    FCurrentDirectory: string;
    FStartupWindowState: TStartupWindowState;
    FAllowOnlyOneInstance: BG;

    // Output
    FHandle: THandle;
    FErrorCode: U4;

    function GetExitCode: TExitCode;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property FileName: TFileName read FFileName write SetFileName;
    property Parameters: string read FParameters write SetParameters;
    property CurrentDirectory: string read FCurrentDirectory write SetCurrentDirectory;
    property StartupWindowState: TStartupWindowState read FStartupWindowState write SetStartupWindowState;
    property AllowOnlyOneInstance: BG read FAllowOnlyOneInstance write SetAllowOnlyOneInstance;
    property KeepRunning: BG read FKeepRunning write SetKeepRunning;

    // Process
    procedure Execute; virtual;
    procedure Terminate(const AExitCode: UINT = DBG_TERMINATE_PROCESS); virtual;
    procedure Close; virtual;

    // raise Exception
    procedure CheckErrorCode;

    // Display error code
    procedure ShowErrorCode;

    // @raise exception if Exit Code <> 0
    procedure CheckExitCode;

    // Output
    property ProcessId: DWORD read GetProcessId;
    property ExitCode: TExitCode read GetExitCode;
    property ErrorCode: U4 read FErrorCode;
    property Handle: THandle read FHandle;
    property AllocatedMemory: U8 read GetAllocatedMemory;
    property AllocatedMemoryPeak: U8 read GetAllocatedMemoryPeak;
  end;

implementation

uses
  Winapi.psAPI,

  uMsg,
  uFiles,
  uEExternalApplication,
  uEIOException,
  uLog,
  uMainTimer;

{ TCustomExternalApplication }

procedure TCustomExternalApplication.CheckErrorCode;
begin
  if FErrorCode <> 0 then
    raise EIOException.Create(FFileName, FErrorCode);
end;

procedure TCustomExternalApplication.Terminate(const AExitCode: UINT = DBG_TERMINATE_PROCESS);
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    if not FKeepRunning then
    begin
      if GetExitCode = STILL_ACTIVE then
      begin
        if LogDebug then
          MainLog.Add('Terminating ' + FFileName + ', ExitCode: ' + IntToStr(AExitCode), mlDebug);

        if not TerminateProcess(FHandle, AExitCode) then
          RaiseLastOSError;
        // TODO : Kill process tree CreateJobObject / AssignProcessToJobObject
        Assert(GetExitCode <> STILL_ACTIVE);
      end;
    end;
  end;
end;

procedure TCustomExternalApplication.UpdateProcessMemoryInfo;
var
  ProcessMemoryCounters: _PROCESS_MEMORY_COUNTERS;
begin
  if (FHandle = INVALID_HANDLE_VALUE) then
  begin
    FAllocatedMemory := 0;
    FAllocatedMemoryPeak := 0;
  end
  else if MainTimer.IntervalFrom(FUpdateProcessMemoryInfoTime).Milliseconds >= 1000 then
  begin
    FillChar(ProcessMemoryCounters, SizeOf(ProcessMemoryCounters), 0);
    ProcessMemoryCounters.cb := SizeOf(ProcessMemoryCounters);
    if not GetProcessMemoryInfo(FHandle, @ProcessMemoryCounters, ProcessMemoryCounters.cb) then
      RaiseLastOSError;
    FAllocatedMemory := ProcessMemoryCounters.WorkingSetSize;
    FAllocatedMemoryPeak := ProcessMemoryCounters.PeakWorkingSetSize;

    FUpdateProcessMemoryInfoTime := MainTimer.Value;
  end;
end;

procedure TCustomExternalApplication.CheckExitCode;
var
  ExitCode: TExitCode;
begin
  ExitCode := GetExitCode;
  if (ExitCode <> 0) and (ExitCode <> STILL_ACTIVE) then
  begin
    raise EExternalApplication.Create(FFilename + ' ' + FParameters, ExitCode, '');
  end;
end;

function TerminateAppEnum(Ahwnd: HWND; AParam: LPARAM ): BOOL; stdcall;
const
  WM_CLOSE = $10;
  WM_QUIT = $12;
var
  dwID: DWORD;
begin
  dwID := 0;
  GetWindowThreadProcessId(Ahwnd, dwID) ;

  if dwID = DWORD(AParam) then
  begin
    PostMessage(Ahwnd, WM_CLOSE, 0, 0); // GUI and some Console
    PostMessage(Ahwnd, WM_QUIT, 0, 0); // GUI
    Result := False; // Stop call TerminateAppEnum
  end
  else
    Result := True; // Call TerminateAppEnum again
end;

procedure TCustomExternalApplication.Close;
begin
  EnumWindows(@TerminateAppEnum, LPARAM(ProcessId));
end;

constructor TCustomExternalApplication.Create;
begin
  inherited;

  FHandle := INVALID_HANDLE_VALUE;
  FStartupWindowState.WindowState := hwsNormal;
  FStartupWindowState.Active := True;
  FAllowOnlyOneInstance := True;
end;

destructor TCustomExternalApplication.Destroy;
begin
  try
    Terminate;
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  finally
    inherited;
  end;
end;

procedure TCustomExternalApplication.Execute;
begin
  if FAllowOnlyOneInstance and (FHandle <> INVALID_HANDLE_VALUE) then
    Terminate;

  if FFileName = '' then
    raise EArgumentException.Create('File name is empty.');
  if FCurrentDirectory = '' then
    raise EArgumentException.Create('Current directory is empty.');
end;

function TCustomExternalApplication.GetAllocatedMemory: U8;
begin
  UpdateProcessMemoryInfo;
  Result := FAllocatedMemory;
end;

function TCustomExternalApplication.GetAllocatedMemoryPeak: U8;
begin
  UpdateProcessMemoryInfo;
  Result := FAllocatedMemoryPeak;
end;

function TCustomExternalApplication.GetExitCode: TExitCode;
begin
  Assert(FHandle <> INVALID_HANDLE_VALUE);
  GetExitCodeProcess(FHandle, Result);
end;

function TCustomExternalApplication.GetProcessId: DWORD;
begin
  Result := Winapi.Windows.GetProcessId(FHandle);
end;

procedure TCustomExternalApplication.SetAllowOnlyOneInstance(const Value: BG);
begin
  FAllowOnlyOneInstance := Value;
end;

procedure TCustomExternalApplication.SetCurrentDirectory(const Value: string);
begin
  FCurrentDirectory := Value;
end;

procedure TCustomExternalApplication.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TCustomExternalApplication.SetKeepRunning(const Value: BG);
begin
  FKeepRunning := Value;
end;

procedure TCustomExternalApplication.SetParameters(const Value: string);
begin
  FParameters := Value;
end;

procedure TCustomExternalApplication.SetStartupWindowState(const Value: TStartupWindowState);
begin
  FStartupWindowState := Value;
end;

procedure TCustomExternalApplication.ShowErrorCode;
begin
  if FErrorCode <> 0 then
    IOError(FFileName, FErrorCode);
end;

end.
