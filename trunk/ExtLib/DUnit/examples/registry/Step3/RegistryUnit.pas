unit RegistryUnit;

interface

uses SysUtils, Classes;

type
  ERegistrySampleError = class(Exception);

  procedure GetRegData(out InstallPath, ModData: string);
  function GetRegInstallPath: string;
  function GetRegModData: string;

const
  ABaseKey = 'Software\Sample Co.\SampleApp';

implementation

uses Registry;

procedure GetRegData(out InstallPath, ModData: string);
begin
  InstallPath := GetRegInstallPath;
  ModData := GetRegModData;
end;

function GetRegInstallPath: string;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    if not r.KeyExists(ABaseKey) then
      raise ERegistrySampleError.Create('App key does not exist.');

    if not r.OpenKey(ABaseKey, false) then
      raise ERegistrySampleError.Create('Cannot open app key.');

    Result := r.ReadString('InstallPath');

    r.CloseKey;
  finally
    r.Free;
  end;
end;

function GetRegModData: string;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    if not r.KeyExists(ABaseKey + '\ModuleAData') then
      raise ERegistrySampleError.Create('Module key does not exist.');

    if not r.OpenKey(ABaseKey + '\ModuleAData', false) then
      raise ERegistrySampleError.Create('Cannot open module key.');

    Result := r.ReadString('Data');

    r.CloseKey;
  finally
    r.Free;
  end;
end;

end.
