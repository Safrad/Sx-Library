unit uUCIWriter;

interface

uses
  uTypes,
  uStartWriter;

type
  TUCIWriter = class(TStartWriter)
  private
    FMoves: string;

    procedure SendPosition;
  public
    procedure SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG); override;
    procedure SetStartPos; override;
    procedure IsReady; override;
    procedure DoMove(const AMove: string); override;
    procedure NewGame; override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

uses
  uLevelManagerToUCIString,
  uStrings;

{ TUCIWriter }

procedure TUCIWriter.DoMove(const AMove: string);
begin
  inherited;

  FMoves := FMoves + AMove + CharSpace;
end;

procedure TUCIWriter.IsReady;
begin
  inherited;

  SendCommand('isready');
end;

procedure TUCIWriter.NewGame;
begin
  inherited;

  Engine.AnalysisInfo.StartPosition := '';
  FMoves := '';

  SendCommand('ucinewgame');
  SendCommand('position startpos');
end;

procedure TUCIWriter.SendPosition;
var
  Command: string;
begin
  Command := 'position ';
  if Engine.AnalysisInfo.StartPosition = '' then
    AppendStr(Command, 'startpos')
  else
    AppendStr(Command, 'fen ' + Engine.AnalysisInfo.StartPosition);

  if FMoves <> '' then
    Command := Command + ' moves ' + FMoves;

  SendCommand(Command);
end;

procedure TUCIWriter.SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG);
begin
  inherited;

  Engine.AnalysisInfo.StartPosition := AString;
end;

procedure TUCIWriter.SetStartPos;
begin
  inherited;

  Engine.AnalysisInfo.StartPosition := '';
  SendCommand('position startpos');
end;

procedure TUCIWriter.Start;
var
  Command: string;
begin
	inherited;

  SendPosition;

  Command := 'go ';
  AppendStr(Command, LevelManagerToUCIString(Engine.LevelManager, Engine.SideToMove));
  if not Engine.RootMoves.AcceptAll then
    AppendStr(Command, 'searchmoves ' + Engine.RootMoves.GetIncludedMoves);
  SendCommand(Command);
end;

procedure TUCIWriter.Stop;
begin
  inherited;

  SendCommand('stop');
end;

end.
