unit uExternalApplication;

interface

uses
  uTypes,
  uStartupWindowState,
  SysUtils;

type
  TExitCode = U4;

  TProcessOutput = record
    ErrorCode: U4;
    ExitCode: TExitCode;
    OutputText: string;
  end;

  TExternalApplication = class
  private
    FFileName: TFileName;
    FFileNameWithoutVariables: TFileName;
    FHandle: THandle;
    FExists: BG;
    FParameters: string;
    FCurrentDirectory: string;
    FStartupWindowState: TStartupWindowState;
    FProcessOutput: TProcessOutput;
    FErrorCode: U4;
    FAbort: BG;
    procedure SetFileName(const Value: TFileName);
    function GetExitCode: TExitCode;
    procedure SetParameters(const Value: string);
    procedure SetCurrentDirectory(const Value: string);
    procedure SetProcessOutput(const Value: TProcessOutput);
    procedure SetStartupWindowState(const Value: TStartupWindowState);
    procedure SetErrorCode(const Value: U4);
    procedure SetAbort(const Value: BG);
  public
    constructor Create;
    destructor Destroy; override;

    // Does not update ProcessOutput
    procedure Execute;

    // Update ProcessOutput
    procedure WaitFor;

    // Update ProcessOutput
    procedure ExecuteWithOutputText;

    // raise Exception
    procedure CheckErrorCode;

    // Display error code
    procedure ShowErrorCode;

    // @raise exception if Exit Code <> 0
    procedure CheckExitCode;

    property FileName: TFileName read FFileName write SetFileName;
    property FileNameWithoutVariables: TFileName read FFileNameWithoutVariables;
    property Parameters: string read FParameters write SetParameters;
    property CurrentDirectory: string read FCurrentDirectory write SetCurrentDirectory;
    property StartupWindowState: TStartupWindowState read FStartupWindowState write SetStartupWindowState;

    property ErrorCode: U4 read FErrorCode write SetErrorCode;

    property Handle: THandle read FHandle;
    property Exists: BG read FExists;
    property ProcessOutput: TProcessOutput read FProcessOutput write SetProcessOutput;
    property Abort: BG read FAbort write SetAbort;
  end;

implementation

uses
  Windows,
  uStartupEnvironment,
  Forms,
  ShellAPI,
  uMsg,
  uEIOException,
  uEExternalApplication,
	uLog;

{ TExternalApplicaton }

procedure TExternalApplication.ShowErrorCode;
begin
  if FErrorCode <> 0 then
    IOError(FFileName, FErrorCode);
end;

procedure TExternalApplication.CheckErrorCode;
begin
  if FErrorCode <> 0 then
    raise EIOException.Create(FFileName, FErrorCode);
end;

procedure TExternalApplication.CheckExitCode;
begin
  if FProcessOutput.ExitCode <> 0 then
  begin
    raise EExternalApplication.Create(FFilename + ' ' + FParameters, FProcessOutput.ExitCode, FProcessOutput.OutputText);
  end;
end;

constructor TExternalApplication.Create;
begin
  inherited;

  FHandle := INVALID_HANDLE_VALUE;
  FCurrentDirectory := GetCurrentDir;
  FStartupWindowState.WindowState := hwsNormal;
  FStartupWindowState.Active := True;
end;

destructor TExternalApplication.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);

  inherited;
end;

procedure TExternalApplication.Execute;
var
	lpExecInfo: TShellExecuteInfo;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);

  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  FillChar(lpExecInfo, SizeOf(lpExecInfo), 0);

  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  if IsConsole then
    lpExecInfo.fMask := lpExecInfo.fMask or SEE_MASK_FLAG_NO_UI;
  lpExecInfo.Wnd := GetActiveWindow();
  lpExecInfo.lpVerb := 'open';
  lpExecInfo.lpParameters := PChar(FParameters);
  lpExecInfo.lpFile := PChar(FFileNameWithoutVariables);
  lpExecInfo.nShow := FStartupWindowState.ToWindowsAPIParameter;
  lpExecInfo.hProcess := INVALID_HANDLE_VALUE;
  lpExecInfo.lpDirectory := PWideChar(FCurrentDirectory);

  if LogDebug then
    MainLogAdd('ShellExecuteEx ' + FFileName + ' ' + FParameters, mlDebug);
  if ShellExecuteEx(@lpExecInfo) then
  begin
    FHandle := lpExecInfo.hProcess;
  end
  else
  begin
    FHandle := INVALID_HANDLE_VALUE;
    FErrorCode := GetLastError;
  end;
end;

procedure TExternalApplication.ExecuteWithOutputText;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  CreateProcessResult: BG;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);

  FillChar(FProcessOutput, SizeOf(FProcessOutput), 0);
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := FStartupWindowState.ToWindowsAPIParameter;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;
  	if LogDebug then
      MainLogAdd('CreateProcess ' + FFileName + ' ' + FParameters, mlDebug);
    CreateProcessResult := CreateProcess(nil, PChar('"' + FFileNameWithoutVariables + '" ' + FParameters),
                            nil, nil, True, 0, nil,
                            PChar(CurrentDirectory), SI, PI);
    if not CreateProcessResult then
    begin
      FHandle := INVALID_HANDLE_VALUE;
      FErrorCode := GetLastError;
      Exit;
    end;
    FHandle := PI.hProcess;
    CloseHandle(StdOutPipeWrite);
    StdOutPipeWrite := 0;
    try
      repeat
        WasOK := ReadFile(StdOutPipeRead, Buffer, Length(Buffer) - 1, BytesRead, nil);
        if BytesRead > 0 then
        begin
          Buffer[BytesRead] := #0;
          FProcessOutput.OutputText := FProcessOutput.OutputText + string(Buffer);
        end;
      until not WasOK or (BytesRead = 0);

      WaitFor;
    finally
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  finally
    CloseHandle(StdOutPipeWrite);
    CloseHandle(StdOutPipeRead);
  end;
end;

function TExternalApplication.GetExitCode: TExitCode;
begin
  if FHandle <> 0 then
  begin
    GetExitCodeProcess(FHandle, Result);
  end
  else
    Result := 0;
end;

procedure TExternalApplication.SetAbort(const Value: BG);
begin
  FAbort := Value;
end;

procedure TExternalApplication.SetCurrentDirectory(const Value: string);
begin
  FCurrentDirectory := Value;
end;

procedure TExternalApplication.SetErrorCode(const Value: U4);
begin
  FErrorCode := Value;
end;

procedure TExternalApplication.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  FFileNameWithoutVariables := StartupEnvironment.RemoveVariables(FFileName);

  FExists := FileExists(FFileNameWithoutVariables);
end;

procedure TExternalApplication.SetParameters(const Value: string);
begin
  FParameters := Value;
end;

procedure TExternalApplication.SetProcessOutput(const Value: TProcessOutput);
begin
  FProcessOutput := Value;
end;

procedure TExternalApplication.SetStartupWindowState(const Value: TStartupWindowState);
begin
  FStartupWindowState := Value;
end;

procedure TExternalApplication.WaitFor;
var
  WaitResult: DWORD;
begin
  // WaitForSingleObject(PI.hProcess, INFINITE);
  FAbort := False;
  repeat
    Sleep(LoopSleepTime);
    WaitResult := WaitForSingleObject(FHandle, LoopSleepTime);
    if FAbort then
      Break;
  until not ((WaitResult <> WAIT_OBJECT_0)); // WaitResult = WAIT_TIMEOUT;

  FProcessOutput.ExitCode := GetExitCode;
end;

end.
