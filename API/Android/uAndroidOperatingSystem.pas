unit uAndroidOperatingSystem;

interface

uses
  uTypes,
  uProjectVersion,
  uCustomOperatingSystem;

type
  TAndroidOperatingSystem = class(TCustomOperatingSystem)
  protected
    class function GetComputerNameInternal: string; override;

    class function GetNameInternal: string; override;
    class function GetVersion: TProjectVersion; override;

  public
    class procedure Hibernate; override;
    class procedure Sleep; override;

    class procedure LogOff(const APowerForce: TPowerForce); override;
    class procedure PowerOff(const APowerForce: TPowerForce); override;
    class procedure Restart(const APowerForce: TPowerForce); override;
    class procedure ShutDown(const APowerForce: TPowerForce); override;

    class function GetCurrentProcessId: U4; override;
    class function GetCurrentThreadId: U4; override;

    class function GetUptimeInMs: U8; override;

    class procedure SystemProperties;
    class procedure DisplayProperties;
  end;

implementation

uses
  SysUtils,
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes;

{ TAndroidOperatingSystem }

class function TAndroidOperatingSystem.GetComputerNameInternal: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);
  if Result = '' then
    Result := JStringToString(TJBuild.JavaClass.MANUFACTURER) + ' ' + JStringToString(TJBuild.JavaClass.PRODUCT);
end;

class function TAndroidOperatingSystem.GetCurrentProcessId: U4;
begin
  Result := 0;
end;

class function TAndroidOperatingSystem.GetCurrentThreadId: U4;
begin
  Result := 0;
end;

class function TAndroidOperatingSystem.GetVersion: TProjectVersion;
begin
  Result.Major := TOSVersion.Major;
  Result.Minor := TOSVersion.Minor;
  Result.Release := TOSVersion.Build;
  Result.Build := 0;
end;

class procedure TAndroidOperatingSystem.DisplayProperties;
begin

end;

class procedure TAndroidOperatingSystem.Hibernate;
begin

end;

class procedure TAndroidOperatingSystem.LogOff(const APowerForce: TPowerForce);
begin

end;

class function TAndroidOperatingSystem.GetNameInternal: string;
begin
  Result := TOSVersion.Name;
end;

class function TAndroidOperatingSystem.GetUptimeInMs: U8;
begin
  Result := 0;
end;

class procedure TAndroidOperatingSystem.PowerOff(const APowerForce: TPowerForce);
begin

end;

class procedure TAndroidOperatingSystem.ShutDown(const APowerForce: TPowerForce);
begin

end;

class procedure TAndroidOperatingSystem.Sleep;
begin

end;

class procedure TAndroidOperatingSystem.Restart(const APowerForce: TPowerForce);
begin

end;

class procedure TAndroidOperatingSystem.SystemProperties;
begin

end;

end.
