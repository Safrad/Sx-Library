unit uNewCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TNewCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TNewCommand }

constructor TNewCommand.Create;
begin
  inherited;

  Description := 'Reset the board to the standard chess starting position. Leave force mode.';
end;

procedure TNewCommand.Execute(const AParameters: string);
begin
  inherited;

  InternalEngine.LevelManager.ResetLevels;
  InternalEngine.SetStartPos;
  XBoardProtocol.AutoPlay := True;
end;

function TNewCommand.GetSyntax: string;
begin
  Result := '';
end;

end.
