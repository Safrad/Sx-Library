unit uExcludeCommand;

interface

uses
  uEngineCommand;

type
  TExcludeCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TExcludeCommand }

constructor TExcludeCommand.Create;
begin
  inherited;

  Description := 'Exclude "Move" for search.';
end;

procedure TExcludeCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.RootMoves.Exclude(AParameters);
end;

function TExcludeCommand.GetSyntax: string;
begin
  Result := '[Move]';
end;

end.

