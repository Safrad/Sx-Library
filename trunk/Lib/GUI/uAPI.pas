//* File:     Lib\GUI\uAPI.pas
//* Created:  1998-01-01
//* Modified: 2008-02-16
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uAPI;

interface

uses
	uTypes,
	Classes, SysUtils;

procedure APIOpen(FileName: TFileName; const Params: string = '');
procedure PropertiesDialog(FileName: TFileName);
function KeyToStr(const Key: U2): string;

implementation

uses
	Windows, ShellAPI,
	uMsg, uFiles, uLog;

procedure APIOpen(FileName: TFileName; const Params: string = '');
label LRetry;
var
	ErrorCode: U4;
begin
	MainLogAdd('ShellExecute ' + FileName + ' ' + Params, mlDebug);
	LRetry:
	ErrorCode := ShellExecute(0, 'open', PChar('"' + RemoveEV(FileName) + '"'), PChar(Params), nil, SW_ShowNormal);
	if ErrorCode <= 32 then
		if IOErrorRetry(FileName, ErrorCode) then goto LRetry;
end;

procedure PropertiesDialog(FileName: TFileName);
label LRetry;
var
	sei: TShellExecuteInfo;
begin
	LRetry:
	FillChar(sei, SizeOf(sei), 0);
	sei.cbSize := SizeOf(sei);
	sei.lpFile := PChar(FileName);
	sei.lpVerb := 'properties';
	sei.fMask  := SEE_MASK_INVOKEIDLIST;
	if ShellExecuteEx(@sei) = False then
		if IOErrorRetry(FileName, GetLastError) then goto LRetry;
end;

function KeyToStr(const Key: U2): string;
begin
	case Key and $ff of
	0: Result := '';
	VK_LBUTTON: Result := 'L. Button';
	VK_RBUTTON: Result := 'R. Button';
	VK_CANCEL: Result := 'Cancel';
	VK_MBUTTON: Result := 'M.Button';
	VK_BACK: Result := 'Back';
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
	Ord('0')..Ord('9'): Result := Chr(Key and $ff);
	Ord('A')..Ord('Z'): Result := Chr(Key and $ff);
	VK_LWIN: Result := 'L.Win';
	VK_RWIN: Result := 'R.Win';
	VK_APPS: Result := 'Apps';
	96..96 + 9: Result := 'Num ' + Chr(Ord('0') + (Key and $ff) - 96);
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

