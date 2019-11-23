unit uAnalyzeCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TAnalyzeCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TAnalyzeCommand }

constructor TAnalyzeCommand.Create;
begin
  inherited;

  Description := 'Enter analyze mode.';
end;

procedure TAnalyzeCommand.Execute(const AParameters: string);
begin
  inherited;

  XBoardProtocol.AutoPlay := False;
  XBoardProtocol.AnalyzeMode := True;

  InternalEngine.DoMoveAfterStop := False;
  InternalEngine.LevelManager.InfiniteAnalysis := True;
  InternalEngine.Start;
end;

function TAnalyzeCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

