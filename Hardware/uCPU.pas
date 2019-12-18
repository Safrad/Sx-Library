unit uCPU;

interface

uses
{$ifdef MSWINDOWS}uWindowsCPU{$else}uCustomCPU{$endif};

type
  TSpecificCPU = {$ifdef MSWINDOWS}TWindowsCPU{$else}TCustomCPU{$endif};

function CPU: TSpecificCPU;

implementation

uses
  SysUtils;

var
  GCPU: TSpecificCPU;

function CPU: TSpecificCPU;
begin
  Result := GCPU;
end;

initialization
{$IFNDEF NoInitialization}
  GCPU := TSpecificCPU.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GCPU);
{$ENDIF NoFinalization}
end.
