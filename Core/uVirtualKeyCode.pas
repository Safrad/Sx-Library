unit uVirtualKeyCode;

interface

uses
  uTypes;

function VirtualKeyCodeToText(const AVirtualKeyCode: U2): string;
{$ifdef Unicode}
function VirtualKeyCodeToUnicodeSymbol(const AVirtualKeyCode: U2): string;
{$endif}

// if available KeyToUnicodeSymbol is used otherwise KeyToText is used
function VirtualKeyCodeToString(const AVirtualKeyCode: U2): string;

implementation

uses
  uChar,
  uUnicodeChar,
  SysUtils,
  Windows;

const
  scCommand = $1000;
  scShift = $2000;
  scCtrl = $4000;
  scAlt = $8000;

function VirtualKeyCodeToText(const AVirtualKeyCode: U2): string;
const
  KeyCodeToString: array[U1] of string = (
    '', // 0
    'L. Button', // VK_LBUTTON
    'R. Button', // VK_RBUTTON
    'Cancel', // VK_CANCEL
    'M. Button', // VK_MBUTTON
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
    'Pause/Break', // VK_PAUSE
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
    'L. Win', // VK_LWIN
    'R. Win', // VK_RWIN
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
    '=+', // VK_OEM_PLUS
    ',<', // VK_OEM_COMMA
    '-_', // VK_OEM_MINUS
    '.>', // VK_OEM_PERIOD
    '/?', // VK_OEM_2
    '`~', // VK_OEM_3
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
    'OEM 8', // VK_OEM_8
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
    Result := 'Virtual key code: ' + '0x' + IntToHex(AVirtualKeyCode and $ff, 2);

  // Prefix
	if AVirtualKeyCode and scCommand <> 0 then
    Result := 'Cmd' + '+' + Result;
	if AVirtualKeyCode and scAlt <> 0 then
    Result := 'Alt' + '+' + Result;
	if AVirtualKeyCode and scCtrl <> 0 then
    Result := 'Ctrl' + '+' + Result;
	if AVirtualKeyCode and scShift <> 0 then
    Result := 'Shift' + '+' + Result;
end;

{$ifdef Unicode}
function VirtualKeyCodeToUnicodeSymbol(const AVirtualKeyCode: U2): string;
begin
  case AVirtualKeyCode and $ff of
  VK_TAB: Result := TabKey;
  VK_BACK: Result := BackspaceKey;
  VK_RETURN: Result := AlternativeEnterKey;
  VK_VOLUME_UP: Result := BeamedEightNotes + '+';
  VK_VOLUME_DOWN: Result := BeamedEightNotes + '-';
  VK_LEFT: Result := CharLeftawardsArrow;
  VK_UP: Result := CharUpawardsArrow;
  VK_RIGHT: Result := CharRightawardsArrow;
  VK_DOWN: Result := CharDownawardsArrow;

  VK_SHIFT: Result := ShiftKey;
  VK_CONTROL: Result := ControlKey;
  VK_MENU: Result := MenuKey; // Alt

  VK_LSHIFT: Result := 'Left' + ShiftKey;
  VK_LCONTROL: Result := 'Left' + ControlKey;
  VK_LMENU: Result := 'Left' + MenuKey;
  VK_LWIN: Result := 'Left' + WindowsLogoKey;

  VK_RSHIFT: Result := 'Right' + ShiftKey;
  VK_RCONTROL: Result := 'Right' + ControlKey;
  VK_RMENU: Result := 'Right' + MenuKey;
  VK_RWIN: Result := 'Right' + WindowsLogoKey;

  VK_DELETE: Result := DelKey;
  VK_ESCAPE: Result := EscapeKey;
  VK_SPACE: Result := SpaceKey;
  VK_LBUTTON: Result := 'Left' + MouseChar;
  VK_RBUTTON: Result := 'Right' + MouseChar;
  VK_MBUTTON: Result := 'Middle' + MouseChar;
  VK_XBUTTON1: Result := 'X1' + MouseChar;
  VK_XBUTTON2: Result := 'X2' + MouseChar;

  VK_LAUNCH_MAIL: Result := EMailKey;
  VK_BROWSER_SEARCH: Result := SearchChar;
  VK_MEDIA_STOP: Result := StopChar;
  VK_MEDIA_NEXT_TRACK: Result := NextChar;
  VK_MEDIA_PREV_TRACK: Result := PrevChar;
  VK_MEDIA_PLAY_PAUSE: Result := PlayChar;

  VK_SLEEP: Result := LastQuarterMoonChar;
  VK_BROWSER_REFRESH: Result := RefreshChar;
  VK_BROWSER_FAVORITES: Result := FavoritesChar;

  VK_CAPITAL: Result := CapsLockKey;

  else
    Result := '';
  end;

  if Result <> '' then
  begin
    // Prefix
  	if AVirtualKeyCode and scCommand <> 0 then
      Result := WindowsLogoKey + '+' + Result;
    if AVirtualKeyCode and scAlt <> 0 then
      Result := MenuKey + '+' + Result;
    if AVirtualKeyCode and scCtrl <> 0 then
      Result := ControlKey + '+' + Result;
    if AVirtualKeyCode and scShift <> 0 then
      Result := ShiftKey + '+' + Result;
  end;
end;
{$endif}

function VirtualKeyCodeToString(const AVirtualKeyCode: U2): string;
begin
  Result := VirtualKeyCodeToUnicodeSymbol(AVirtualKeyCode);
  if Result = '' then
    Result := VirtualKeyCodeToText(AVirtualKeyCode)
end;

end.
