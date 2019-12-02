unit uAcceptedCommand;

interface

uses
  uEngineCommand;

type
  TAcceptedCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTypes;

{ TAcceptedCommand }

constructor TAcceptedCommand.Create;
begin
  inherited;

  Description := 'May be sent in reply to the "feature" command.';
end;

procedure TAcceptedCommand.Execute(const AParameters: string);
begin
  inherited;

  // TODO : GUI capability of feature
end;

function TAcceptedCommand.GetSyntax: string;
begin
  Result := '';
end;

end.
