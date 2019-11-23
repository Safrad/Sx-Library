unit uRejectedCommand;

interface

uses
  uEngineCommand;

type
  TRejectedCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTypes;

{ TRejectedCommand }

constructor TRejectedCommand.Create;
begin
  inherited;

  Description := 'May be sent in reply to the "feature" command.';
end;

procedure TRejectedCommand.Execute(const AParameters: string);
begin
  inherited;

  // TODO : GUI capability of feature
end;

function TRejectedCommand.GetSyntax: string;
begin
  Result := '';
end;

end.
