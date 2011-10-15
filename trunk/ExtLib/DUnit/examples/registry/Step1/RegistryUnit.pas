unit RegistryUnit;

interface

uses SysUtils, Classes;

type
  ERegistrySampleError = class(Exception);

  procedure GetRegData(out InstallPath, ModData: string);

const
  ABaseKey = 'Software\Sample Co.\SampleApp';

implementation

uses Registry;

procedure GetRegData(out InstallPath, ModData: string);
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    if not r.KeyExists(ABaseKey) then
      raise ERegistrySampleError.Create('App key does not exist.');

    if not r.OpenKey(ABaseKey, false) then
      raise ERegistrySampleError.Create('Cannot open app key.');

    InstallPath := r.ReadString('InstallPath');

    if not r.KeyExists('ModuleAData') then
      raise ERegistrySampleError.Create('Module key does not exist.');

    if not r.OpenKey('ModuleAData', false) then
      raise ERegistrySampleError.Create('Cannot open module key.');

    ModData := r.ReadString('Data');

    r.CloseKey;
  finally
    r.Free;
  end;
end;

end.
