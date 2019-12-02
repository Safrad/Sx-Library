unit uRegisterCommand;

interface

uses
  uEngineCommand;

type
  TRegisterCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TRegisterCommand }

constructor TRegisterCommand.Create;
begin
  inherited;

  Description := 'Try to register an engine or to tell the engine that registration will be done later.';
end;

procedure TRegisterCommand.Execute(const AParameters: string);
begin
  inherited;

end;

function TRegisterCommand.GetSyntax: string;
begin
  Result := '[later | name {registration name} code {registration code}]';
end;

end.
