unit uCustomOperatingSystem;

interface

uses
  uTypes,
  uProjectVersion;

type
  TPowerForce = (pfNone, pfForceIfHang, pfForce);

  TCustomOperatingSystem = class
  private
    FComputerName: string;

    FName: string;

    function GetComputerName: string;
    function GetName: string;
    function GetNameAndVersion: string;
  protected
    class function GetComputerNameInternal: string; virtual; abstract;

    class function GetNameInternal: string; virtual; abstract;
    class function GetVersion: TProjectVersion; virtual; abstract;

    class function GetUptimeInMs: U8; virtual; abstract;
  public
    class procedure Hibernate; virtual; abstract;
    class procedure Sleep; virtual; abstract;

    class procedure LogOff(const APowerForce: TPowerForce); virtual; abstract;
    class procedure PowerOff(const APowerForce: TPowerForce); virtual; abstract;
    class procedure Restart(const APowerForce: TPowerForce); virtual; abstract;
    class procedure ShutDown(const APowerForce: TPowerForce); virtual; abstract;

    class procedure HibernateConfirmation;
    class procedure SleepConfirmation;

    class procedure LogOffConfirmation(const APowerForce: TPowerForce);
    class procedure PowerOffConfirmation(const APowerForce: TPowerForce);
    class procedure RestartConfirmation(const APowerForce: TPowerForce);
    class procedure ShutDownConfirmation(const APowerForce: TPowerForce);

    class function GetCurrentProcessId: U4; virtual; abstract;
    class function GetCurrentThreadId: U4; virtual; abstract;

    /// <returns> Operating system name and version.
    /// <para>For example, "Microsoft Windows XP Professional Version = 5.1.2500".</para></returns>
    property ComputerName: string read GetComputerName;

    property Name: string read GetName;
    property Version: TProjectVersion read GetVersion;
    function VersionAsString: string;
    property NameAndVersion: string read GetNameAndVersion;
    property UptimeInMs: U8 read GetUptimeInMs;
  end;

implementation

uses
  SysUtils,
  uOutputInfo,
  uMsg;

resourcestring
  rsHibernate = 'Really hibernate?';
  rsLogOff = 'Really log off?';
  rsPowerOff = 'Really power off?';
  rsRestart = 'Really restart?';
  rsShutDown = 'Really shut down?';
  rsSleep = 'Really sleep?';

{ TCustomOperatingSystem }

function TCustomOperatingSystem.GetComputerName: string;
begin
  if FComputerName = '' then
  begin
    try
      FComputerName := GetComputerNameInternal;
      if FComputerName = '' then
        FComputerName := '?';
    except
      on E: Exception do
      begin
        FComputerName := '?';
        Fatal(E);
      end;
    end;
  end;
  Result := FComputerName;
end;

function TCustomOperatingSystem.GetName: string;
begin
  if FName = '' then
  begin
    try
      FName := GetNameInternal;
      if FName = '' then
        FName := '?';
    except
      on E: Exception do
      begin
        FName := '?';
        Fatal(E);
      end;
    end;
  end;
  Result := FName;
end;

function TCustomOperatingSystem.GetNameAndVersion: string;
begin
  Result := Name + ' [' + VersionAsString + ']';
end;

class procedure TCustomOperatingSystem.HibernateConfirmation;
begin
	if Confirmation(rsHibernate, [mbYes, mbNo]) = mbYes then
    Hibernate;
end;

class procedure TCustomOperatingSystem.LogOffConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsLogOff, [mbYes, mbNo]) = mbYes then
    LogOff(APowerForce);
end;

class procedure TCustomOperatingSystem.PowerOffConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsPowerOff, [mbYes, mbNo]) = mbYes then
    PowerOff(APowerForce);
end;

class procedure TCustomOperatingSystem.RestartConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsRestart, [mbYes, mbNo]) = mbYes then
    Restart(APowerForce);
end;

class procedure TCustomOperatingSystem.ShutDownConfirmation(const APowerForce: TPowerForce);
begin
	if Confirmation(rsShutDown, [mbYes, mbNo]) = mbYes then
    ShutDown(APowerForce);
end;

class procedure TCustomOperatingSystem.SleepConfirmation;
begin
	if Confirmation(rsSleep, [mbYes, mbNo]) = mbYes then
    Sleep;
end;

function TCustomOperatingSystem.VersionAsString: string;
begin
  Result :=
    IntToStr(Version.Major) + '.' +
    IntToStr(Version.Minor) + '.' +
    IntToStr(Version.Release);
end;

end.
