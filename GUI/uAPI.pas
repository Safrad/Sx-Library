unit uAPI;

interface

uses
	uTypes,
	SysUtils, Windows;

// @Return process exit code
function RunBat(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;

// @Return process exit code
function ShellExecuteDirect(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): U4;

procedure ShellExecuteDirectNoExitCode(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_NORMAL);

procedure APIOpen(FileName: TFileName; const Params: string = '');

function GetDosOutput(const ACommandLine: string; const AWorkDir: string = ''): string;

procedure PropertiesDialog(FileName: TFileName);

var
  AbortAPI: BG;

implementation

uses
  uShellExecute,
  ShellAPI,
	Forms,
	uMsg, uFiles, uLog, uStrings, uFile, uSxThread;

function RunBat(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;
var
	BatFileName: string;
begin
	BatFileName := InstanceTempDir + 'TempRun.bat';
	if FileExists(BatFileName) then
	  DeleteFileEx(BatFileName);
	WriteStringToFile(BatFileName, Copy(CurrentDirectory, 1, 2) + FileSep + 'cd "' + CurrentDirectory + '"' + FileSep + 'call "' + FFileName + '"' + CharSpace + Params + LineSep, False, fcAnsi);
	try
		Result := ShellExecuteDirect(BatFileName, '', CurrentDirectory, ShowCmd);
	finally
//		DeleteFileEx(BatFileName); ShellExecuteDirect is asynchronous
	end;
end;

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
        Application.ProcessMessages;
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

function GetDosOutput(const ACommandLine: string; const AWorkDir: string): string;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  Handle: Boolean;
begin
  Result := '';
  with SA do begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + ACommandLine),
                            nil, nil, True, 0, nil,
                            PChar(AWorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, Length(Buffer) - 1, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + Buffer;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
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
			if IOErrorRetry(FileName, GetLastError) then Continue;
		Break;
	end;
end;

end.

