unit uNpsCommand;

interface

uses
  uEngineCommand;

type
  TNpsCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,

  uEParseError,
  uStrings;

{ TNpsCommand }

constructor TNpsCommand.Create;
begin
  inherited;

  Description := 'The engine should not use wall-clock time to make its timing decisions, but an own internal time measure based on the number of nodes it has searched.';
end;

procedure TNpsCommand.Execute(const AParameters: string);
begin
  inherited;

  if AParameters = '' then
  begin
    raise EParseError.Create(['exact time per move in seconds'], '');
  end;

  InternalEngine.StopManager.LimitNodesPerSecond := ReadS8Fast(AParameters);
end;

function TNpsCommand.GetSyntax: string;
begin
  Result := '[NodeRate]';
end;

end.
