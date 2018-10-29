unit uUIApplication;

interface

uses
  uCommonApplication,
  uSwitchArgument;

type
  TUIApplication = class(TCommonApplication)
  protected
    FMinimizedArgument: TSwitchArgument;
    procedure AddArguments; override;
    procedure Finalize; override;
  end;

implementation

uses
  uCustomArgument;

{ TUIApplication }

procedure TUIApplication.AddArguments;
begin
  inherited;

  FMinimizedArgument := TSwitchArgument.Create;
  FMinimizedArgument.Shortcut := 'minimized';
  FMinimizedArgument.Description := 'Minimizes application';
  FMinimizedArgument.RequireCheck := rcOptional;
  Arguments.Add(FMinimizedArgument);
end;

procedure TUIApplication.Finalize;
begin
  FMinimizedArgument.Free;

  inherited;
end;

end.
