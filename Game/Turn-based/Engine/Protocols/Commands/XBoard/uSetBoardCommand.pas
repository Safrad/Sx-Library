unit uSetBoardCommand;

interface

uses
  uEngineCommand;

type
  TSetBoardCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TSetBoardCommand }

constructor TSetBoardCommand.Create;
begin
  inherited;

  Description := 'Set up position.';
end;

procedure TSetBoardCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.SetPositionFromString(AParameters);
end;

function TSetBoardCommand.GetSyntax: string;
begin
  Result := '[FEN]';
end;

end.

