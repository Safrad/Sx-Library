unit uOperatingSystem;

interface

uses
{$ifdef MSWINDOWS}uWindowsOperatingSystem{$else}uCustomOperatingSystem{$endif};

type
  TSpecificOperatingSystem = {$ifdef MSWINDOWS}TWindowsOperatingSystem{$else}TCustomOperatingSystem{$endif};

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
