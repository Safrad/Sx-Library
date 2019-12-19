unit uKeyboard;

interface

{$ifdef MSWINDOWS}
uses
  uWindowsKeyboard;
type
  TSpecificKeyboard = TWindowsKeyboard;
{$else}
uses
  uCustomKeyboard;
type
  TSpecificKeyboard = TCustomKeyboard;
{$endif}
function Keyboard: TSpecificKeyboard;

implementation

uses
  SysUtils;

var
  GKeyboard: TSpecificKeyboard;

function Keyboard: TSpecificKeyboard;
begin
  Result := GKeyboard;
end;

initialization
{$IFNDEF NoInitialization}
  GKeyboard := TSpecificKeyboard.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GKeyboard);
{$ENDIF NoFinalization}
end.
