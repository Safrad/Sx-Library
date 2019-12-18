unit uSystemMemory;

interface

{$ifdef MSWINDOWS}
uses
  uWindowsSystemMemory;
type
  TSpecificSystemMemory = TWindowsSystemMemory;
{$endif}

{$ifdef ANDROID}
uses
  uAndroidSystemMemory;
type
  TSpecificSystemMemory = TAndroidSystemMemory;
{$endif}

function SystemMemory: TSpecificSystemMemory;

implementation

uses
  SysUtils;

var
  GSystemMemory: TSpecificSystemMemory;

function SystemMemory: TSpecificSystemMemory;
begin
  Result := GSystemMemory;
end;

initialization
{$IFNDEF NoInitialization}
  GSystemMemory := TSpecificSystemMemory.Create;
  GSystemMemory.Update;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GSystemMemory);
{$ENDIF NoFinalization}
end.
