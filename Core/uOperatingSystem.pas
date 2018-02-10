unit uOperatingSystem;

interface

uses
  uTypes,
  uProjectVersion;

type
  TPowerForce = (pfNone, pfForceIfHang, pfForce);

  TOperatingSystem = class
  private
    FName: string;

    function GetIsNT: BG;
    function GetIsAero: BG;
    function GetIsRegionCompatible: BG;
    procedure SetName(const Value: string);
    function GetName: string;
    function GetUptimeInMs: U8;

    class function GetFlag(const APowerForce: TPowerForce): SG;
    class function GetNameInternall: string;
    function GetVersion: TProjectVersion;
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
    class procedure SetPrivilege;

    class function ComputerName: string;

    property IsNT: BG read GetIsNT;
    property IsAero: BG read GetIsAero;
    property IsRegionCompatible: BG read GetIsRegionCompatible;
    property Name: string read GetName write SetName;
    property Version: TProjectVersion read GetVersion;
    property UptimeInMs: U8 read GetUptimeInMs;
  end;

function OperatingSystem: TOperatingSystem;

implementation

uses
  Windows,
  Messages,
  ActiveX,
  ComObj,
  uMsg,
  uAPI, SysUtils;

resourcestring
  rsHibernate = 'Really hibernate?';
  rsLogOff = 'Really log off?';
  rsPowerOff = 'Really power off?';
  rsRestart = 'Really restart?';
  rsShutDown = 'Really shut down?';
  rsSleep = 'Really sleep?';

var
  GOperatingSystem: TOperatingSystem;

function OperatingSystem: TOperatingSystem;
begin
  Result := GOperatingSystem;
end;

{ TOperatingSystem }

class function TOperatingSystem.ComputerName: string;
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName( ComputerName, Size );
  Result := ComputerName;
end;

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

function TOperatingSystem.GetIsAero: BG;
begin
  // Windows Vista and newer
  Result := Win32MajorVersion >= 6;
end;

function TOperatingSystem.GetIsNT: BG;
begin
  // Windows 2000, XP and newer
	Result := Win32Platform >= VER_PLATFORM_WIN32_NT; // GSysInfo.OS.dwMajorVersion >= 5
end;

function TOperatingSystem.GetIsRegionCompatible: BG;
begin
  // Windows Me and newer
  Result := not ((Win32MajorVersion < 4) or ((Win32MajorVersion = 4) and (Win32MinorVersion < 10)));
end;

function TOperatingSystem.GetName: string;
begin
  if FName = '' then
  begin
    try
      FName := GetNameInternall;
      if FName = '' then
        FName := '?';
    except
      FName := '?';
    end;
  end;
  Result := FName;
end;

class procedure TOperatingSystem.Hibernate;
begin
  SetPrivilege;
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
  SetPrivilege;
	if ExitWindowsEx(EWX_LOGOFF or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.LogOffConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsLogOff, [mbYes, mbNo]) = mbYes then
    LogOff(APowerForce);
end;

class function TOperatingSystem.GetNameInternall: string;
var
  objWMIService : OLEVariant;
  colItems      : OLEVariant;
  colItem       : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;

  function GetWMIObject(const objectName: String): IDispatch;
  var
    chEaten: Integer;
    BindCtx: IBindCtx;
    Moniker: IMoniker;
  begin
    OleCheck(CreateBindCtx(0, bindCtx));
    OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
    OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
  end;

begin
  objWMIService := GetWMIObject('winmgmts:\\localhost\root\cimv2');
  colItems      := objWMIService.ExecQuery('SELECT Caption FROM Win32_OperatingSystem','WQL',0);
  oEnum         := IUnknown(colItems._NewEnum) as IEnumVariant;
  if oEnum.Next(1, colItem, iValue) = 0 then
  Result:=colItem.Caption; //The caption property return an  string  wich includes the operating system version. For example, "Microsoft Windows XP Professional Version = 5.1.2500".
end;

function TOperatingSystem.GetUptimeInMs: U8;
begin
  Result := GetTickCount;
end;

class procedure TOperatingSystem.PowerOff(const APowerForce: TPowerForce);
begin
  SetPrivilege;
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
  SetPrivilege;
	if ExitWindowsEx(EWX_REBOOT or GetFlag(APowerForce), 0) = False then
		ErrorMsg(GetLastError);
end;

class procedure TOperatingSystem.RestartConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsRestart, [mbYes, mbNo]) = mbYes then
    Restart(APowerForce);
end;

procedure TOperatingSystem.SetName(const Value: string);
begin
  FName := Value;
end;

class procedure TOperatingSystem.SetPrivilege;
var
  hToken, hProcess: THandle;
  tp, prev_tp: TTokenPrivileges;
  Len: DWORD;
begin
  if OperatingSystem.IsNT then
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
  SetPrivilege;
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

function TOperatingSystem.GetVersion: TProjectVersion;
begin
  Result.Major := Win32MajorVersion;
  Result.Minor := Win32MinorVersion;
  Result.Release := Win32BuildNumber;
  Result.Build := 0;
end;

initialization
{$IFNDEF NoInitialization}
  GOperatingSystem := TOperatingSystem.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GOperatingSystem);
{$ENDIF NoFinalization}
end.
