unit uConsole;

interface

{$ifdef Console}
  {$ifdef MSWINDOWS}
  uses
    uWindowsConsole;
  type
    TConsole = TWindowsConsole;
  {$endif}
{$endif}

function Console: TConsole;

implementation

uses
  SysUtils;

var
  GConsole: TConsole;

function Console: TConsole;
begin
  if GConsole = nil then
    GConsole := TConsole.Create;

  Result := GConsole;
end;

initialization

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GConsole);
{$ENDIF NoFinalization}

end.

