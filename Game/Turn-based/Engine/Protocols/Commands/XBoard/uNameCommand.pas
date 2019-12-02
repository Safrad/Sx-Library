unit uNameCommand;

interface

uses
  uEngineCommand;

type
  TNameCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TNameCommand }

constructor TNameCommand.Create;
begin
  inherited;

  Description := 'Informs about engine opponent''s name.';
end;

procedure TNameCommand.Execute(const AParameters: string);
begin
  inherited;

end;

function TNameCommand.GetSyntax: string;
begin
  Result := '[UserName]';
end;

end.

