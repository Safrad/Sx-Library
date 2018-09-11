unit uAPI;

interface

uses
	uTypes,
	SysUtils, Windows;

type
  TProcessOutput = record
    ExitCode: DWORD;
    OutputText: string;
  end;

// @Return process exit code
function ShellExecuteDirect(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): U4;

procedure ShellExecuteDirectNoExitCode(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_NORMAL);

function ShellExecuteHandle(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_NORMAL): THandle;

procedure APIOpen(FileName: TFileName; const Params: string = '');

// @Return process exit code and output
procedure ExecuteProcess(out ProcessOutput: TProcessOutput; const AFileName: TFileName; AParams: string = ''; const ACurrentDirectory: string = ''; const AShowCmd: Word = SW_HIDE);

// @Return process output
// @raise exception if Exit Code <> 0
procedure ExecuteProcessCheckExitCode(out ProcessOutput: TProcessOutput; const AFileName: TFileName; AParams: string = ''; const ACurrentDirectory: string = ''; const AShowCmd: Word = SW_HIDE);

procedure PropertiesDialog(FileName: TFileName);

var
  AbortAPI: BG;

implementation

uses
  uShellExecute,
  uEIOException,
  ShellAPI,
  uEExternalApplication,
	uMsg, uFiles, uLog, uStrings, uFile, uSxThread;

function ShellExecuteDirect(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): U4;
var
	FAgain : BG;
	ErrorCode: U4;
	lpExecInfo: TShellExecuteInfo;
	i: UG;
begin
	FAgain := True;
	while FAgain do
	begin
 //		ErrorCode := ShellExecute(0, OpenString, PChar('"' + RemoveEV(FFileName) + '"'), PChar(Params), PChar(CurrentDirectory), ShowCmd);
		lpExecInfo.cbSize := SizeOf(lpExecInfo);
		FillChar(lpExecInfo, SizeOf(lpExecInfo), 0);

    lpExecInfo.cbSize := SizeOf(lpExecInfo);
    lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
    lpExecInfo.Wnd := GetActiveWindow();
    lpExecInfo.lpVerb := 'open';
    lpExecInfo.lpParameters := PChar(Params);
    lpExecInfo.lpFile := PChar(FFileName);
    lpExecInfo.nShow := ShowCmd; //SW_SHOWNORMAL;

{		lpExecInfo.lpFile := PChar(FFileName);
		lpExecInfo.lpParameters := PChar(Params);
    lpExecInfo.lpVerb := PChar('open');
		lpExecInfo.lpDirectory := PChar(CurrentDirectory);
		lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT; //SEE_MASK_CLASSNAME | SEE_MASK_IDLIST | SEE_MASK_FLAG_NO_UI;

		lpExecInfo.Wnd := GetActiveWindow;
		lpExecInfo.nShow := ShowCmd;}
		if ShellExecuteEx(@lpExecInfo) then
		begin
			repeat
				Sleep(LoopSleepTime);
				i := WaitForSingleObject(lpExecInfo.hProcess, LoopSleepTime);
        if AbortAPI then Break;
			until not((i <> WAIT_OBJECT_0)); //i = WAIT_TIMEOUT)); // WAIT_OBJECT_0
  		GetExitCodeProcess(lpExecInfo.hProcess, Result);
      FAgain := False;
//  		FAgain := (ErrorCode <= 32) and IOErrorRetry(FFileName, ErrorCode);
    end
    else
    begin
      ErrorCode := GetLastError;
			FAgain := (ErrorCode <= 32) and IOErrorRetry(FFileName, ErrorCode);
    end;
	end;
	CloseHandle(lpExecInfo.hProcess);
end;

procedure ShellExecuteDirectNoExitCode(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_NORMAL);
var
	lpExecInfo: TShellExecuteInfo;
begin
  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  FillChar(lpExecInfo, SizeOf(lpExecInfo), 0);

  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  lpExecInfo.Wnd := GetActiveWindow();
  lpExecInfo.lpVerb := 'open';
  lpExecInfo.lpParameters := PChar(Params);
  lpExecInfo.lpFile := PChar(FFileName);
  lpExecInfo.nShow := ShowCmd;

  ShellExecuteEx(@lpExecInfo);
	CloseHandle(lpExecInfo.hProcess);
end;

function ShellExecuteHandle(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_NORMAL): THandle;
var
	lpExecInfo: TShellExecuteInfo;
begin
  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  FillChar(lpExecInfo, SizeOf(lpExecInfo), 0);

  lpExecInfo.cbSize := SizeOf(lpExecInfo);
  lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_DDEWAIT;
  lpExecInfo.Wnd := GetActiveWindow();
  lpExecInfo.lpVerb := 'open';
  lpExecInfo.lpParameters := PChar(Params);
  lpExecInfo.lpFile := PChar(FFileName);
  lpExecInfo.nShow := ShowCmd;
  lpExecInfo.hProcess := INVALID_HANDLE_VALUE;

  ShellExecuteEx(@lpExecInfo);
  Result := lpExecInfo.hProcess;
end;

procedure APIOpen(FileName: TFileName; const Params: string = '');
var
	ShellExecuteThread: TShellExecute;
begin
	if LogDebug then
    MainLogAdd('ShellExecute ' + FileName + ' ' + Params, mlDebug);
//	ShellExecute(0, OpenString, PChar('"' + RemoveEV(FileName) + '"'), PChar(Params), nil, SW_ShowNormal);
	ShellExecuteThread := TShellExecute.Create;
	ShellExecuteThread.Name := 'ShellExecute';
  ShellExecuteThread.FileName := FileName;
  ShellExecuteThread.Params := Params;
	ShellExecuteThread.Start;
end;

procedure ExecuteProcess(out ProcessOutput: TProcessOutput; const AFileName: TFileName; AParams: string = ''; const ACurrentDirectory: string = ''; const AShowCmd: Word = SW_HIDE);
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  Handle: Boolean;
  WaitResult: DWORD;
  CurrentDirectory: string;
begin
  FillChar(ProcessOutput, SizeOf(ProcessOutput), 0);
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := AShowCmd;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;
    if ACurrentDirectory = '' then
      CurrentDirectory := GetCurrentDir
    else
      CurrentDirectory := ACurrentDirectory;
//    Replace(AParams, '"', '');
    Handle := CreateProcess(nil, PChar('"' + AFileName + '" ' + AParams),
                            nil, nil, True, 0, nil,
                            PChar(CurrentDirectory), SI, PI);
{    Handle := CreateProcess(nil, PChar('cmd.exe /C "' + AFileName + '" ' + AParams),
                            nil, nil, True, 0, nil,
                            PChar(CurrentDirectory), SI, PI);}
    if not Handle then
    begin
      raise EIOException.Create(AFileName, GetLastError);
    end;
    CloseHandle(StdOutPipeWrite);
    try
      repeat
        WasOK := ReadFile(StdOutPipeRead, Buffer, Length(Buffer) - 1, BytesRead, nil);
        if BytesRead > 0 then
        begin
          Buffer[BytesRead] := #0;
          ProcessOutput.OutputText := ProcessOutput.OutputText + string(Buffer);
        end;
      until not WasOK or (BytesRead = 0);

      // WaitForSingleObject(PI.hProcess, INFINITE);
      repeat
        Sleep(LoopSleepTime);
        WaitResult := WaitForSingleObject(PI.hProcess, LoopSleepTime);
        if AbortAPI then Break;
      until not((WaitResult <> WAIT_OBJECT_0));

      GetExitCodeProcess(PI.hProcess, ProcessOutput.ExitCode);
    finally
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  finally
    CloseHandle(StdOutPipeWrite);
    CloseHandle(StdOutPipeRead);
  end;
end;

procedure ExecuteProcessCheckExitCode(out ProcessOutput: TProcessOutput; const AFileName: TFileName; AParams: string = ''; const ACurrentDirectory: string = ''; const AShowCmd: Word = SW_HIDE);
begin
  ExecuteProcess(ProcessOutput, AFilename, AParams, ACurrentDirectory, AShowCmd);
  if ProcessOutput.ExitCode <> 0 then
  begin
    raise EExternalApplication.Create(AFilename + ' ' + AParams, ProcessOutput.ExitCode, ProcessOutput.OutputText);
  end;
end;

procedure PropertiesDialog(FileName: TFileName);
var
	sei: TShellExecuteInfo;
begin
	while True do
	begin
		FillChar(sei, SizeOf(sei), 0);
		sei.cbSize := SizeOf(sei);
		sei.lpFile := PChar(FileName);
		sei.lpVerb := 'properties';
		sei.fMask  := SEE_MASK_INVOKEIDLIST;
		if ShellExecuteEx(@sei) = False then
			if IOErrorRetry(FileName, GetLastError) then
        Continue;
		Break;
	end;
end;

end.

