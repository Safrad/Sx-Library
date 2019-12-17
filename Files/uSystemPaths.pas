unit uSystemPaths;

interface

{$ifdef MSWINDOWS}
uses
  uWindowsSystemPaths;
type
  TSpecificSystemPaths = TWindowsSystemPaths;
{$endif}

{$ifdef ANDROID}
uses
  uAndroidSystemPaths;
type
  TSpecificSystemPaths = TAndroidSystemPaths;
{$endif}

function SystemPaths: TSpecificSystemPaths;

implementation

uses
  SysUtils;

var
  GSystemPaths: TSpecificSystemPaths;

function SystemPaths: TSpecificSystemPaths;
begin
  Result := GSystemPaths;
end;

initialization
{$IFNDEF NoInitialization}
  GSystemPaths := TSpecificSystemPaths.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GSystemPaths);
{$ENDIF NoFinalization}
end.
