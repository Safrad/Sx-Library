unit uXBoardExitCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TXBoardExitCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TXBoardExitCommand }

constructor TXBoardExitCommand.Create;
begin
  inherited;

  Description := 'Leave analyze mode.';
end;

procedure TXBoardExitCommand.Execute(const AParameters: string);
begin
  inherited;

  XBoardProtocol.AnalyzeMode := False;

  InternalEngine.Stop;
  InternalEngine.LevelManager.InfiniteAnalysis := False;
end;

function TXBoardExitCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

