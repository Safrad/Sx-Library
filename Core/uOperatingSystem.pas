unit uOperatingSystem;

interface

{$ifdef MSWINDOWS}
uses
  uWindowsOperatingSystem;
type
  TSpecificOperatingSystem = TWindowsOperatingSystem;
{$endif}

{$ifdef ANDROID}
uses
  uAndroidOperatingSystem;
type
  TSpecificOperatingSystem = TAndroidOperatingSystem;
{$endif}

function OperatingSystem: TSpecificOperatingSystem;

implementation

uses
  SysUtils;

var
  GOperatingSystem: TSpecificOperatingSystem;

function OperatingSystem: TSpecificOperatingSystem;
begin
  Result := GOperatingSystem;
end;

initialization
{$IFNDEF NoInitialization}
  GOperatingSystem := TSpecificOperatingSystem.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GOperatingSystem);
{$ENDIF NoFinalization}
end.
