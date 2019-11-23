unit uIncludeCommand;

interface

uses
  uEngineCommand;

type
  TIncludeCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TIncludeCommand }

constructor TIncludeCommand.Create;
begin
  inherited;

  Description := 'Include "Move" for search.';
end;

procedure TIncludeCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.RootMoves.Include(AParameters);
end;

function TIncludeCommand.GetSyntax: string;
begin
  Result := '[Move]';
end;

end.

