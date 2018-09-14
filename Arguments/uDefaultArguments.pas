unit uDefaultArguments;

interface

uses
  uArguments,
  uSwitchArgument;

type
  TDefaultArguments = class(TArguments)
  private
    FHelpArgument: TSwitchArgument;
  public
    constructor Create;

    procedure Parse(const ACommandLine: string); override;

    property HelpArgunment: TSwitchArgument read FHelpArgument;
  end;

implementation

uses
  uMsg;

{ TDefaultArguments }

constructor TDefaultArguments.Create;
begin
  inherited;

  FHelpArgument := TSwitchArgument.Create;
  FHelpArgument.Shortcut := 'h';
  FHelpArgument.Description := 'Display this help.';
  Add(FHelpArgument);
end;

procedure TDefaultArguments.Parse(const ACommandLine: string);
begin
  inherited;

  if HelpArgunment.Exists or ((Count = 0) and (RequiredArgumentCount > 0)) then
  begin
    if IsConsole then
      PreviewToConsole
    else
      Information(PreviewAsString);
  end;
end;       

end.
