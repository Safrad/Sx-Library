unit uMemoryCommand;

interface

uses
  uUnsupportedCommand;

type
  TMemoryCommand = class(TUnsupportedCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils;

{ TMemoryCommand }

constructor TMemoryCommand.Create;
begin
  inherited;

  Description := 'THow much memory it is allowed to use maximally, in MegaBytes.';
end;

function TMemoryCommand.GetSyntax: string;
begin
  Result := '[MegaBytes]';
end;

end.

