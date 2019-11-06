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
  public
    class procedure Activate;
  end;

implementation

uses
{$IF defined(MSWINDOWS)}
  Winapi.Windows,
  VCL.Forms,
{$ENDIF}
  uCustomArgument;

{$IF defined(MSWINDOWS)}
// Warning this flag can be used only in exe, not in dll/bpl
// in some cases it is possible to use it with correctly set ImageBase of dll
// Executable file is smaller
{$SETPEFLAGS IMAGE_FILE_RELOCS_STRIPPED}
{$ENDIF}

{ TUIApplication }

class procedure TUIApplication.Activate;
begin
{$IF defined(MSWINDOWS)}
  if Assigned(Application) and Assigned(Application.MainForm) and Application.ShowMainForm then
    if Application.MainForm.Visible = False then
      Application.MainForm.Show;
  if IsIconic(Application.Handle) then
    Application.Restore
  else
    Application.BringToFront;
{$ENDIF}
end;

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
