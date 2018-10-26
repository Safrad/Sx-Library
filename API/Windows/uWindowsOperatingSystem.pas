unit uWindowsOperatingSystem;

interface

uses
  uTypes,
  uProjectVersion,
  uCustomOperatingSystem;

type
  TWindowsOperatingSystem = class(TCustomOperatingSystem)
  private
    class function GetIsNT: BG;
    class function GetIsAero: BG;
    class function GetIsRegionCompatible: BG;
    class function GetFlag(const APowerForce: TPowerForce): SG;
  protected
    class function GetComputerNameInternal: string; override;

    // Returns an string which includes the operating system version.
    // For example, "Microsoft Windows XP Professional Version = 5.1.2500".
    class function GetNameInternal: string; override;
    class function GetVersion: TProjectVersion; override;

  public
    class procedure Hibernate; override;
    class procedure Sleep; override;

    class procedure LogOff(const APowerForce: TPowerForce); override;
    class procedure PowerOff(const APowerForce: TPowerForce); override;
    class procedure Restart(const APowerForce: TPowerForce); override;
    class procedure ShutDown(const APowerForce: TPowerForce); override;

    class function GetUptimeInMs: U8; override;

    class procedure SystemProperties;
    class procedure DisplayProperties;
    class procedure RepaintScreen;
    class procedure SetPrivilege;

    property IsNT: BG read GetIsNT;
    property IsAero: BG read GetIsAero;
    property IsRegionCompatible: BG read GetIsRegionCompatible;
  end;

implementation

uses
  SysUtils,
  Windows,
  uWindowsManagementInstrumentation,
  Messages,
  uMsg,
  uAPI;

{ TWindowsOperatingSystem }

class function TWindowsOperatingSystem.GetComputerNameInternal: string;
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName( ComputerName, Size );
  Result := ComputerName;
end;

class function TWindowsOperatingSystem.GetVersion: TProjectVersion;
begin
  Result.Major := Win32MajorVersion;
  Result.Minor := Win32MinorVersion;
  Result.Release := Win32BuildNumber;
  Result.Build := 0;
end;

class procedure TWindowsOperatingSystem.DisplayProperties;
begin
	APIOpen('CONTROL.EXE', 'Desk.cpl, Display,3');
end;

class function TWindowsOperatingSystem.GetFlag(
  const APowerForce: TPowerForce): SG;
begin
  case APowerForce of
  pfForceIfHang: Result := EWX_FORCEIFHUNG;
  pfForce: Result := EWX_FORCE;
  else
    Result := 0;
  end;
end;

class function TWindowsOperatingSystem.GetIsAero: BG;
begin
  // Windows Vista and newer
  Result := Win32MajorVersion >= 6;
end;

class function TWindowsOperatingSystem.GetIsNT: BG;
begin
  // Windows 2000, XP and newer
	Result := Win32Platform >= VER_PLATFORM_WIN32_NT; // GSysInfo.OS.dwMajorVersion >= 5
end;

class function TWindowsOperatingSystem.GetIsRegionCompatible: BG;
begin
  // Windows Me and newer
  Result := not ((Win32MajorVersion < 4) or ((Win32MajorVersion = 4) and (Win32MinorVersion < 10)));
end;

class procedure TWindowsOperatingSystem.Hibernate;
begin
  SetPrivilege;
	if SetSystemPowerState(False, True) = False then
		ErrorMsg(GetLastError);
end;

class procedure TWindowsOperatingSystem.LogOff(const APowerForce: TPowerForce);
begin
  SetPrivilege;
	if ExitWindowsEx(EWX_LOGOFF or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class function TWindowsOperatingSystem.GetNameInternal: string;
begin
  TWindowsManagementInstrumentation.GetDatabaseItem('Win32_OperatingSystem', 'Caption');
end;

class function TWindowsOperatingSystem.GetUptimeInMs: U8;
begin
  Result := GetTickCount;
end;

class procedure TWindowsOperatingSystem.PowerOff(const APowerForce: TPowerForce);
begin
  SetPrivilege;
	if ExitWindowsEx(EWX_POWEROFF or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TWindowsOperatingSystem.ShutDown(const APowerForce: TPowerForce);
begin
	if ExitWindowsEx(EWX_SHUTDOWN or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TWindowsOperatingSystem.Sleep;
begin
  SetPrivilege;
	if SetSystemPowerState(True, True) = False then
		ErrorMsg(GetLastError);
end;

class procedure TWindowsOperatingSystem.RepaintScreen;
begin
  SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, 0);
end;

class procedure TWindowsOperatingSystem.Restart(const APowerForce: TPowerForce);
begin
  SetPrivilege;
	if ExitWindowsEx(EWX_REBOOT or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TWindowsOperatingSystem.SetPrivilege;
var
  hToken, hProcess: THandle;
  tp, prev_tp: TTokenPrivileges;
  Len: DWORD;
begin
  if GetIsNT then
  begin
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, True, GetCurrentProcessID);
    try
      if not OpenProcessToken(hProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,hToken) then
        Exit;
    finally
      CloseHandle(hProcess);
    end;

    try
      if not LookupPrivilegeValue('', 'SeShutdownPrivilege',tp.Privileges[0].Luid) then
        Exit;
      tp.PrivilegeCount := 1;
      tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if not AdjustTokenPrivileges(hToken, False, tp, SizeOf(prev_tp),prev_tp, Len) then
        Exit;
    finally
      CloseHandle(hToken);
    end;
  end;
end;

class procedure TWindowsOperatingSystem.SystemProperties;
begin
	APIOpen('CONTROL.EXE', 'Sysdm.cpl, System,1');
end;


end.
