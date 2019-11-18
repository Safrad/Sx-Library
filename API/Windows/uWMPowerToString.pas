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
    PBT_APMQUERYSUSPEND:
      Result := 'Query Suspend';
    PBT_APMQUERYSTANDBY:
      Result := 'Query Standby';
    PBT_APMQUERYSUSPENDFAILED:
      Result := 'Query Suspend Failed';
    PBT_APMQUERYSTANDBYFAILED:
      Result := 'Query Standby Failed';
    PBT_APMSUSPEND: // Frequently used
      Result := 'Query Suspend';
    PBT_APMSTANDBY:
      Result := 'Standby';
    PBT_APMRESUMECRITICAL:
      Result := 'Resume Critical';
    PBT_APMRESUMESUSPEND: // Frequently used
      Result := 'Resume Suspend';
    PBT_APMRESUMESTANDBY:
      Result := 'Resume Standby';
    PBT_APMBATTERYLOW:
      Result := 'Battery Low';
    PBT_APMPOWERSTATUSCHANGE: // Rarely used
      Result := 'Status Change';
    PBT_APMOEMEVENT:
      Result := 'OEM Event';
    PBT_APMRESUMEAUTOMATIC: // Frequently used
      Result := 'Resume Automatic';
    PBT_POWERSETTINGCHANGE:
      Result := 'Power Settings Change';
    else
      Result := IntToStr(APowerEvt);
  end;
end;

function WMPowerToString(const AWMPower: TWMPower): string;
begin
  Result :='WM_PowerBroadcast: ' + PowerEvtToString(AWMPower.PowerEvt);
  if AWMPower.PowerEvt = PBT_POWERSETTINGCHANGE then
    Result := Result + ' (' + IntToHex(AWMPower.Unused)  + 'h)';
end;

end.
