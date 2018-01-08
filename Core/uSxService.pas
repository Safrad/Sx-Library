unit uSxService;

interface

uses
  SvcMgr;

type
  TSxService = class(TService)
  private
    FDescription: string;
    procedure SetDescriptionInRegistry;
    procedure SetDescription(const Value: string);
  public
    property Description: string read FDescription write SetDescription; // Must be called in AfterInstall method
  end;

implementation

uses
  Windows, Registry;

{ TSxService }

procedure TSxService.SetDescription(const Value: string);
begin
  FDescription := Value;
  SetDescriptionInRegistry;
end;

procedure TSxService.SetDescriptionInRegistry;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_SET_VALUE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      Reg.WriteString('Description', FDescription);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

end.
