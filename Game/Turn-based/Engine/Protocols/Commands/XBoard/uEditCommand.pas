unit uEditCommand;

interface

uses
  uUnsupportedCommand;

type
  TEditCommand = class(TUnsupportedCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
  end;

implementation

{ TEditCommand }

constructor TEditCommand.Create;
begin
  inherited;

  Description := 'The edit command puts the chess engine into a special mode, where it accepts subcommands.';
end;

function TEditCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

