unit uOperatingSystem;

interface

uses
  uTypes;

type
  TPowerForce = (pfNone, pfForceIfHang, pfForce);

  TOperatingSystem = class
  private
    class function GetFlag(const APowerForce: TPowerForce): SG;
  public
    class procedure Hibernate;
    class procedure Sleep;

    class procedure LogOff(const APowerForce: TPowerForce);
    class procedure PowerOff(const APowerForce: TPowerForce);
    class procedure Restart(const APowerForce: TPowerForce);
    class procedure ShutDown(const APowerForce: TPowerForce);

    class procedure HibernateConfirmation;
    class procedure SleepConfirmation;

    class procedure LogOffConfirmation(const APowerForce: TPowerForce);
    class procedure PowerOffConfirmation(const APowerForce: TPowerForce);
    class procedure RestartConfirmation(const APowerForce: TPowerForce);
    class procedure ShutDownConfirmation(const APowerForce: TPowerForce);

    class procedure SystemProperties;
    class procedure DisplayProperties;

    class procedure RepaintScreen;
  end;

implementation

uses
  Windows,
  Messages,
  uMsg,
  uAPI;

resourcestring
  rsHibernate = 'Really hibernate?';
  rsLogOff = 'Really log off?';
  rsPowerOff = 'Really power off?';
  rsRestart = 'Really restart?';
  rsShutDown = 'Really shut down?';
  rsSleep = 'Really sleep?';

{ TOperatingSystem }

class procedure TOperatingSystem.DisplayProperties;
begin
	APIOpen('CONTROL.EXE', 'Desk.cpl, Display,3');
end;

class function TOperatingSystem.GetFlag(
  const APowerForce: TPowerForce): SG;
begin
  case APowerForce of
  pfForceIfHang: Result := EWX_FORCEIFHUNG;
  pfForce: Result := EWX_FORCE;
  else
    Result := 0;
  end;
end;

class procedure TOperatingSystem.Hibernate;
begin
		if SetSystemPowerState(False, True) = False then
			ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.HibernateConfirmation;
begin
	if Confirmation(rsHibernate, [mbYes, mbNo]) = mbYes then
    Hibernate;
end;

class procedure TOperatingSystem.LogOff(const APowerForce: TPowerForce);
begin
	if ExitWindowsEx(EWX_LOGOFF or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.LogOffConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsLogOff, [mbYes, mbNo]) = mbYes then
    LogOff(APowerForce);
end;

class procedure TOperatingSystem.PowerOff(const APowerForce: TPowerForce);
begin
	if ExitWindowsEx(EWX_POWEROFF or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.PowerOffConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsPowerOff, [mbYes, mbNo]) = mbYes then
    PowerOff(APowerForce);
end;

class procedure TOperatingSystem.RepaintScreen;
begin
  SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, 0);
end;

class procedure TOperatingSystem.Restart(const APowerForce: TPowerForce);
begin
	if ExitWindowsEx(EWX_REBOOT or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.RestartConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsRestart, [mbYes, mbNo]) = mbYes then
    Restart(APowerForce);
end;

class procedure TOperatingSystem.ShutDown(const APowerForce: TPowerForce);
begin
	if ExitWindowsEx(EWX_SHUTDOWN or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.ShutDownConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsShutDown, [mbYes, mbNo]) = mbYes then
    ShutDown(APowerForce);
end;

class procedure TOperatingSystem.Sleep;
begin
	if SetSystemPowerState(True, True) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.SleepConfirmation;
begin
	if Confirmation(rsSleep, [mbYes, mbNo]) = mbYes then
    Sleep;
end;

class procedure TOperatingSystem.SystemProperties;
begin
	APIOpen('CONTROL.EXE', 'Sysdm.cpl, System,1');
end;

end.
