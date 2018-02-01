unit uOperatingSystem;

interface

type
  TOperatingSystem = class

  public
    class procedure Hibernate;
    class procedure LogOff;
    class procedure Restart;
    class procedure ShutDown;
    class procedure Sleep;

    class procedure HibernateConfirmation;
    class procedure LogOffConfirmation;
    class procedure RestartConfirmation;
    class procedure ShutDownConfirmation;
    class procedure SleepConfirmation;

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
  rsRestart = 'Really restart?';
  rsShutDown = 'Really shut down?';
  rsSleep = 'Really sleep?';

{ TOperatingSystem }

class procedure TOperatingSystem.DisplayProperties;
begin
	APIOpen('CONTROL.EXE', 'Desk.cpl, Display,3');
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

class procedure TOperatingSystem.LogOff;
begin
	if ExitWindowsEx(EWX_LOGOFF, 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.LogOffConfirmation;
begin
	if Confirmation(rsLogOff, [mbYes, mbNo]) = mbYes then
    LogOff;
end;

class procedure TOperatingSystem.RepaintScreen;
begin
  SendMessage(HWND_BROADCAST, WM_WININICHANGE, 0, 0);
end;

class procedure TOperatingSystem.Restart;
begin
	if ExitWindowsEx(EWX_REBOOT, 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.RestartConfirmation;
begin
	if Confirmation(rsRestart, [mbYes, mbNo]) = mbYes then
    Restart;
end;

class procedure TOperatingSystem.ShutDown;
begin
	if ExitWindowsEx(EWX_POWEROFF, 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.ShutDownConfirmation;
begin
	if Confirmation(rsShutDown, [mbYes, mbNo]) = mbYes then
    ShutDown;
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
