unit uCorrectApplicationModule;

interface

uses
  uApplicationModule;

type
  TCorrectApplicationModule = class(TApplicationModule)
  protected
    procedure OnLoad; override;
    procedure OnUnload; override;
  end;

implementation

{ TCorrectApplicationModule }

procedure TCorrectApplicationModule.OnLoad;
begin
  inherited;

end;

procedure TCorrectApplicationModule.OnUnload;
begin
  inherited;

end;

end.
