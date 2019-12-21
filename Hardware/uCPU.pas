unit uCPU;

interface

{$ifdef MSWINDOWS}
uses
	uWindowsCPU;
type
  TSpecificCPU = TWindowsCPU;
{$else}
uses
	uCustomCPU;
type
  TSpecificCPU = TCustomCPU;
{$endif}

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
