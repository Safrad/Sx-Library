unit uAPI;

interface

uses
	uTypes,
	Classes, SysUtils, Windows;

// @Return process exit code
function RunBat(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;

// @Return process exit code
function ShellExecuteDirect(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): U4;

procedure ShellExecuteDirectNoExitCode(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_NORMAL);

procedure APIOpen(FileName: TFileName; const Params: string = '');

function GetDosOutput(const ACommandLine: string; const AWorkDir: string = ''): string;

procedure PropertiesDialog(FileName: TFileName);
function KeyToStr(const AVirtualKeyCode: U2): string;


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

function KeyToStr(const AVirtualKeyCode: U2): string;
const
  KeyCodeToString: array[U1] of string = (
    '', // 0
    'L. Button', // VK_LBUTTON
    'R. Button', // VK_RBUTTON
    'Cancel', // VK_CANCEL
    'M.Button', // VK_MBUTTON
    'X Button 1',
    'X Button 2',
    '', // Undefined
    'Backspace', // VK_BACK
    'Tab', // VK_TAB
    '', // Reserved
    '', // Reserved
    'Clear', // VK_CLEAR
    'Enter', // VK_RETURN
    '', // Undefined
    '', // Undefined
    'Shift', // VK_SHIFT
    'Ctrl', // VK_CONTROL
    'Alt', // VK_MENU
    'Pause', // VK_PAUSE
    'Caps Lock', // VK_CAPITAL
    'Kana/Hangul', // VK_KANA, VK_HANGUL
    '', // Undefined
    'Junja', // VK_JUNJA
    'Final', // VK_FINAL
    'Hanja/Kanji', // VK_HANJA, VK_KANJI
    '', // Undefined
    'ESC', // VK_ESCAPE
    'Convert', // VK_CONVERT
    'Nonconvert', // VK_NONCONVERT
    'Accept', // VK_ACCEPT
    'Mode Change', // VK_MODECHANGE
    'Space', // VK_SPACE
    'PgUp', // VK_PRIOR
    'PgDn', // VK_NEXT
    'End', // VK_END
    'Home', // VK_HOME
    'Left', // VK_LEFT
    'Up', // VK_UP
    'Right', // VK_RIGHT
    'Down', // VK_DOWN
    'Select', // VK_SELECT
    'Print', // VK_PRINT
    'Execute', // VK_EXECUTE
    'Print Screen', // VK_SNAPSHOT
    'Insert', // VK_INSERT
    'Delete', // VK_DELETE
    'Help', // VK_HELP
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '', // Undefined
    '', // Undefined
    '', // Undefined
    '', // Undefined
    '', // Undefined
    '', // Undefined
    '', // Undefined
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    'L.Win', // VK_LWIN
    'R.Win', // VK_RWIN
    'Apps', // VK_APPS
    '', // Reserved
    'Sleep', // VK_SLEEP
    'Num 0',
    'Num 1',
    'Num 2',
    'Num 3',
    'Num 4',
    'Num 5',
    'Num 6',
    'Num 7',
    'Num 8',
    'Num 9',
    'Num *', // VK_MULTIPLY
    'Num +', // VK_ADD
    'Separator', // VK_SEPARATOR
    'Num -', // VK_SUBTRACT
    'Num .', // VK_DECIMAL
    'Num /', // VK_DIVIDE
    'F1',
    'F2',
    'F3',
    'F4',
    'F5',
    'F6',
    'F7',
    'F8',
    'F9',
    'F10',
    'F11',
    'F12',
    'F13',
    'F14',
    'F15',
    'F16',
    'F17',
    'F18',
    'F19',
    'F20',
    'F21',
    'F22',
    'F23',
    'F24',
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    'Num Lock', // VK_NUMLOCK
    'Scroll Lock', // VK_SCROLL
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    'L. Shift', // VK_LSHIFT
    'R. Shift', // VK_RSHIFT
    'L. Control', // VK_LCONTROL
    'R. Control', // VK_RCONTROL
    'L. Alt', // VK_LMENU
    'R. Alt', // VK_RMENU
    'Back', // VK_BROWSER_BACK
    'Forward', // VK_BROWSER_FORWARD
    'Refresh', // VK_BROWSER_REFRESH
    'Stop', // VK_BROWSER_STOP
    'Search', // VK_BROWSER_SEARCH
    'Favorites', // VK_BROWSER_FAVORITES
    'Homepage', // VK_BROWSER_HOME
    'Volume Mute', // VK_VOLUME_MUTE
    'Volume Down', // VK_VOLUME_DOWN
    'Volume Up', // VK_VOLUME_UP
    'Next Track', // VK_MEDIA_NEXT_TRACK
    'Previous Track', // VK_MEDIA_PREV_TRACK
    'Stop Media', // VK_MEDIA_STOP
    'Play/Pause Media', // VK_MEDIA_PLAY_PAUSE
    'E-Mail', // VK_LAUNCH_MAIL
    'Select', // VK_LAUNCH_MEDIA_SELECT
    'Launch 1', // VK_LAUNCH_APP1
    'Launch 2', // VK_LAUNCH_APP2
    '', // Reserved
    '', // Reserved
    ';:', // VK_OEM_1
    'OEM +', // VK_OEM_PLUS
    'OEM ,', // VK_OEM_COMMA
    'OEM -', // VK_OEM_MINUS
    'OEM .', // VK_OEM_PERIOD
    'OEM /', // VK_OEM_2
    'OEM ~', // VK_OEM_3
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Reserved
    '', // Unassigned
    '', // Unassigned
    '', // Unassigned
    '[{', // VK_OEM_4
    '\|', // VK_OEM_5
    ']}', // VK_OEM_6
    '''"', // VK_OEM_7
    'OEM 8', // VK_OEM_8 223
    '', // Reserved
    '', // OEM specific
    'OEM 102', // VK_OEM_102
    '', // OEM specific
    '', // OEM specific
    'Process Key', // VK_PROCESSKEY
    '', // OEM specific
    'Packet', // VK_PACKET
    '', // Unassigned
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    '', // OEM specific
    'Attn', // VK_ATTN
    'CrSel', // VK_CRSEL
    'ExSel', // VK_EXSEL
    'Erase EOF', // VK_EREOF
    'Play', // VK_PLAY
    'Zoom', // VK_ZOOM
    'Noname', // VK_NONAME
    'PA1', // VK_PA1
    'Clear', // VK_OEM_CLEAR
    'None'); // No VK mapping
begin
  Result := KeyCodeToString[AVirtualKeyCode and $ff];

	if Result = '' then
    Result := 'Virtual key code: ' + IntToHex(AVirtualKeyCode and $ff, 2);

  // Prefix
	if AVirtualKeyCode and scAlt <> 0 then
    Result := 'Alt+' + Result;
	if AVirtualKeyCode and scCtrl <> 0 then
    Result := 'Ctrl+' + Result;
	if AVirtualKeyCode and scShift <> 0 then
    Result := 'Shift+' + Result;
end;

end.

