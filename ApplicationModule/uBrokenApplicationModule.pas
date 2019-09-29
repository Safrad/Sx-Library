unit uBrokenApplicationModule;

interface

uses
  uApplicationModule;

type
  TBrokenApplicationModule = class(TApplicationModule)
  protected
    procedure OnLoad; override;
    procedure OnUnload; override;
  end;

implementation

uses
  SysUtils;

{ TBrokenApplicationModule }

procedure TBrokenApplicationModule.OnLoad;
begin
  inherited;

  raise Exception.Create('Unspecified load problem for testing.');
end;

procedure TBrokenApplicationModule.OnUnload;
begin
  inherited;

  raise Exception.Create('Unspecified unload problem for testing.');
end;

end.
