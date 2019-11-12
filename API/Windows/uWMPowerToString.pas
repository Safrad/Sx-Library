unit uWMPowerToString;

interface

uses
  Winapi.Messages,
  Winapi.Windows;

function PowerEvtToString(const APowerEvt: WPARAM): string;
function WMPowerToString(const AWMPower: TWMPower): string;

implementation

uses
  SysUtils,

  uTypes;

function PowerEvtToString(const APowerEvt: WPARAM): string;
begin
  case APowerEvt of
    PWR_SUSPENDREQUEST:
      Result := 'Suspend Request';
    PWR_SUSPENDRESUME:
      Result := 'Suspend Resume';
    PWR_CRITICALRESUME:
      Result := 'Critical Resume';
    else
      Result := IntToStr(APowerEvt);
  end;
end;

function WMPowerToString(const AWMPower: TWMPower): string;
begin
  Result :='WM_PowerBroadcast (PowerEvt: ' + PowerEvtToString(AWMPower.PowerEvt) + ', Unused: ' + IntToStr(AWMPower.Unused) + ')'
end;

end.
