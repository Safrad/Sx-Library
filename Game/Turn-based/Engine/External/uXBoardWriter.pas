unit uXBoardWriter;

interface

uses
  uTypes,
  uStartWriter;

type
  TXBoardWriter = class(TStartWriter)
  private
    FInAnalysis: BG;
    FUserMovePrefix: BG;
    procedure SetUserMovePrefix(const Value: BG);
  public
    procedure SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG); override;
    procedure SetStartPos; override;
    procedure IsReady; override;
    procedure DoMove(const AMove: string); override;
    procedure NewGame; override;
    procedure Start; override;
    procedure Stop; override;

    procedure Force;
    procedure Post;

    property UserMovePrefix: BG read FUserMovePrefix write SetUserMovePrefix;
  end;

implementation

uses
  uLevelManagerToXBoardString;

{ TXBoardWriter }

procedure TXBoardWriter.DoMove(const AMove: string);
begin
  inherited;

  Force;
  if FUserMovePrefix then
    SendCommand('usermove ' + AMove)
  else
    SendCommand(AMove);
end;

procedure TXBoardWriter.Force;
begin
  SendCommand('force');
end;

procedure TXBoardWriter.IsReady;
begin
  inherited;

end;

procedure TXBoardWriter.NewGame;
begin
  inherited;

  Engine.AnalysisInfo.StartPosition := '';
  SendCommand('new');
end;

procedure TXBoardWriter.Post;
begin
  SendCommand('post');
end;

procedure TXBoardWriter.SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG);
begin
  inherited;

  SendCommand('setboard ' + AString);
end;

procedure TXBoardWriter.SetStartPos;
begin
  inherited;

  Engine.AnalysisInfo.StartPosition := '';
  SendCommand('new');
end;

procedure TXBoardWriter.SetUserMovePrefix(const Value: BG);
begin
  FUserMovePrefix := Value;
end;

procedure TXBoardWriter.Start;
var
  Command: string;
begin
  inherited;

  Post;

  Command := LevelManagerToXBoardString(Engine.LevelManager);
  SendCommand(Command);
  if Command = 'analyze' then
  begin
    FInAnalysis := True;
  end
  else
  	SendCommand('go');
end;

procedure TXBoardWriter.Stop;
begin
  inherited;

  if FInAnalysis then
  begin
  	SendCommand('exit');
    FInAnalysis := False;
  end
  else
  begin
  	SendCommand('?');
  end;
end;

end.
