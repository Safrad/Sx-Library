unit uAPI;

interface

uses
	uTypes,
	Classes, SysUtils, Windows;

// @Return process exit code
function RunBat(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;

// @Return process exit code
function ShellExecuteDirect(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;

procedure APIOpen(FileName: TFileName; const Params: string = '');
procedure PropertiesDialog(FileName: TFileName);
function KeyToStr(const Key: U2): string;


var
  AbortAPI: BG;

implementation

uses
	ShellAPI, Forms,
	uMsg, uFiles, uLog, uStrings, uFile;

type
	TShellExecute = class(TThread)
	private
		FAgain: BG;
		ErrorCode: U4;
		FFileName: TFileName;
		FParams: string;
		procedure Synchro;
	protected
		procedure Execute; override;
	public
		{ Public declarations }
		constructor Create(FileName: TFileName; const Params: string = '');
	end;

{ TShellExecute }

constructor TShellExecute.Create(FileName: TFileName; const Params: string = '');
begin
	FreeOnTerminate := True;
	FFileName := FileName;
	FParams := Params;
	inherited Create(True);
end;

const
	OpenString = 'open'; // XP
//	OpenString = 'RunAs'; // Newer Windows

function RunBat(const FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;
var
	BatFileName: string;
begin
	BatFileName := InstanceTempDir + 'TempRun.bat';
	WriteStringToFile(BatFileName, 'cd "' + CurrentDirectory + '"' + FileSep + 'call "' + FFileName + '"' + CharSpace + Params + LineSep, False, fcAnsi);
	try
		Result := ShellExecuteDirect(BatFileName, '', CurrentDirectory, ShowCmd);
	finally
//		DeleteFileEx(BatFileName); ShellExecuteDirect is asynchronous
	end;
end;

function ShellExecuteDirect(FFileName: TFileName; const Params: string = ''; const CurrentDirectory: string = ''; const ShowCmd: Word = SW_HIDE): UG;
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

procedure TShellExecute.Execute;
begin
	FAgain := True;
	while FAgain do
	begin
		ErrorCode := ShellExecute(0, OpenString, PChar('"' + RemoveEV(FFileName) + '"'), PChar(FParams), nil, SW_ShowNormal);
		Synchronize(Synchro);
	end;
end;

procedure TShellExecute.Synchro;
begin
	FAgain := (ErrorCode <= 32) and IOErrorRetry(FFileName, ErrorCode);
end;

procedure APIOpen(FileName: TFileName; const Params: string = '');
var
	ShellExecuteThread: TShellExecute;
begin
	MainLogAdd('ShellExecute ' + FileName + ' ' + Params, mlDebug);
//	ShellExecute(0, OpenString, PChar('"' + RemoveEV(FileName) + '"'), PChar(Params), nil, SW_ShowNormal);
	ShellExecuteThread := TShellExecute.Create(FileName, Params);
	{$ifdef CompilerVersion <= 18}
	ShellExecuteThread.NameThreadForDebugging('ShellExecute');
	ShellExecuteThread.Start;
	{$else}
	ShellExecuteThread.Resume;
	{$endif}
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

function KeyToStr(const Key: U2): string;
begin
	case Key and $ff of
	0: Result := '';
	VK_LBUTTON: Result := 'L. Button';
	VK_RBUTTON: Result := 'R. Button';
	VK_CANCEL: Result := 'Cancel';
	VK_MBUTTON: Result := 'M.Button';
	VK_BACK: Result := 'BkSp';
	VK_TAB: Result := 'Tab';
	VK_CLEAR: Result := 'Clear';
	VK_RETURN: Result := 'Enter';
	VK_SHIFT: Result := 'Shift'; // Old Win
	VK_CONTROL: Result := 'Ctrl'; // Old Win
	VK_MENU: Result := 'Alt'; // Old Win
	VK_PAUSE: Result := 'Pause';
	VK_CAPITAL: Result := 'Caps Lock';
	VK_KANA: Result := 'Kana';
//	VK_HANGUL: Result := 'Hangul';
	VK_JUNJA: Result := 'Junja';
	VK_FINAL: Result := 'Final';
	VK_HANJA: Result := 'Hanja';
//	VK_KANJI: Result := 'Kanji';
	VK_CONVERT: Result := 'Convert';
	VK_NONCONVERT: Result := 'Nonconvert';
	VK_ACCEPT: Result := 'Accept';
	VK_MODECHANGE: Result := 'Mode Change';
	VK_ESCAPE: Result := 'ESC';
	VK_SPACE: Result := 'Space';
	VK_PRIOR: Result := 'PgUp';//'Page Up';
	VK_NEXT: Result := 'PgDn';//'Page Down';
	VK_END: Result := 'End';
	VK_HOME: Result := 'Home';
	VK_LEFT: Result := 'Left';
	VK_UP: Result := 'Up';
	VK_RIGHT: Result := 'Right';
	VK_DOWN: Result := 'Down';
	VK_SELECT: Result := 'Select';
	VK_PRINT: Result := 'Print';
	VK_EXECUTE: Result := 'Execute';
	VK_SNAPSHOT: Result := 'Print Screen';
	VK_INSERT: Result := 'Ins';
	VK_DELETE: Result := 'Del';
	VK_HELP: Result := 'Help';
{ VK_0 thru VK_9 are the same as ASCII '0' thru '9' ($30 - $39) }
{ VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' ($41 - $5A) }
	Ord('0')..Ord('9'): Result := Char(Key and $ff);
	Ord('A')..Ord('Z'): Result := Char(Key and $ff);
	VK_LWIN: Result := 'L.Win';
	VK_RWIN: Result := 'R.Win';
	VK_APPS: Result := 'Apps';
	96..96 + 9: Result := 'Num ' + Char(Ord('0') + (Key and $ff) - 96);
	VK_MULTIPLY: Result := 'Num *';
	VK_ADD: Result := 'Num +';
	VK_SEPARATOR: Result := 'Separator';
	VK_SUBTRACT: Result := 'Num -';
	VK_DECIMAL: Result := 'Num ,';
	VK_DIVIDE: Result := 'Num /';
	112..112 + 23: Result := 'F' + IntToStr((Key and $ff)- 111);

	VK_NUMLOCK: Result := 'Num Lock';
	VK_SCROLL: Result := 'Scroll Lock';
{ VK_L & VK_R - left and right Alt, Ctrl and Shift virtual keys.
	Used only as parameters to GetAsyncKeyState() and GetKeyState().
	No other API or message will distinguish left and right keys in this way. }
	VK_LSHIFT: Result := 'L. Shift';
	VK_RSHIFT: Result := 'R. Shift';
	VK_LCONTROL: Result := 'L. Control';
	VK_RCONTROL: Result := 'R. Control';
	VK_LMENU: Result := 'L. Alt';
	VK_RMENU: Result := 'R. Alt';
	187: Result := '=';
	189: Result := '-';

	219: Result := '[';
	221: Result := ']';

	186: Result := ';';
	222: Result := '''';
	220: Result := '\';

	188: Result := ',';
	190: Result := '.';
	191: Result := '/';

	192: Result := '~';

	172: Result := 'WWW';
	180: Result := 'Mail';
	170: Result := 'Search';


	VK_PROCESSKEY: Result := 'Process Key';
	VK_ATTN: Result := 'Attn';
	VK_CRSEL: Result := 'CRSEL';
	VK_EXSEL: Result := 'EXSEL';
	VK_EREOF: Result := 'EREOF';
	VK_PLAY: Result := 'Play';
	VK_ZOOM: Result := 'Zoom';
	VK_NONAME: Result := 'Noname';
	VK_PA1: Result := 'PA1';
	VK_OEM_CLEAR: Result := 'OEM Clear';
	else Result := 'SC: ' + IntToStr(Key);
	end;
	if Key and scAlt <> 0 then Result := 'Alt+' + Result;
	if Key and scCtrl <> 0 then Result := 'Ctrl+' + Result;
	if Key and scShift <> 0 then Result := 'Shift+' + Result;
end;

end.

