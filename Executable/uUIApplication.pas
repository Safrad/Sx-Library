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
  Windows,
  uCustomArgument;

// Warning this flag can be used only in exe, not in dll/bpl
// in some cases it is possible to use it with correctly set ImageBase of dll
// Executable file is smaller
{$SETPEFLAGS IMAGE_FILE_RELOCS_STRIPPED}

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
