unit uCoresCommand;

interface

uses
  uUnsupportedCommand;

type
  TCoresCommand = class(TUnsupportedCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
  end;

implementation

{ TCoresCommand }

constructor TCoresCommand.Create;
begin
  inherited;

  Description := 'How many CPU cores it is allowed to use maximally.';
end;

function TCoresCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

