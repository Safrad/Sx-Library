unit uSxService;

interface

uses
  SvcMgr;

type
  TSxService = class(TService)
  private
    FDescription: string;
    procedure SetDescription(const Value: string);
  public
    property Description: string read FDescription write SetDescription; // Must be called in AfterInstall method
  end;

procedure SetServiceDescriptionInRegistry(const AServiceName: string; const ADescription: string);

implementation

uses
  Windows, Registry;

procedure SetServiceDescriptionInRegistry(const AServiceName: string; const ADescription: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_SET_VALUE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + AServiceName, False) then
    begin
      Reg.WriteString('Description', ADescription);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ TSxService }

procedure TSxService.SetDescription(const Value: string);
begin
  FDescription := Value;
  SetServiceDescriptionInRegistry(Name, Value);
end;

end.
