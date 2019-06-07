unit uUnsupportedCommand;

interface

uses
  uCustomCommand;

type
  TUnsupportedCommand = class(TCustomCommand)
  public
    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils;

{ TUnsupportedCommand }

procedure TUnsupportedCommand.Execute(const AParameters: string);
begin
  inherited;

  raise ENotSupportedException.Create(Shortcut + ' command is not supported.');
end;

end.
