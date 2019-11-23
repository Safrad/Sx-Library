unit uXBoardEngineOutput;

interface

uses
  uTypes,

  uScore,
  uProtocolEngineOutput;

type
  TXBoardEngineOutput = class(TProtocolEngineOutput)
  private
    FEnabled: BG;
    procedure SetEnabled(const Value: BG);
  public
    constructor Create;

    procedure TellGUIInfo(const AMessage: string); override;
    procedure TellGUIError(const AMessage: string); override;
    procedure TellGUIDebug(const AMessage: string); override;

    procedure Start; override;
    procedure DrawDepth; override;
    procedure DrawSelDepth; override;
    procedure DrawNodes; override;
    procedure OneSecond; override;
    procedure ShowBestMove(const APonder: BG); override;
    procedure DoImportMove; override;
    procedure DrawMove1; override;
    procedure DrawRefutation; override;
    procedure DrawAMoves; override;
    procedure DrawCurrLine(const ALine: string); override;

    procedure AcceptDraw; override;
    procedure OfferDraw; override;
    procedure Resign; override;

    function ScoreToStr(const AScore: TScore; const ABoundScore: TScoreBound): string; override;

    property Enabled: BG read FEnabled write SetEnabled;
  end;

implementation

uses
  SysUtils,
  uMath,
  uStrings,
  uTextType,

  uAnalysis;

{ TXBoardEngineOutput }

procedure TXBoardEngineOutput.AcceptDraw;
begin
  inherited;

  StartWrite;
  try
    WriteLine('offer draw', ccKeyword);
  finally
    StopWrite;
  end;
end;

constructor TXBoardEngineOutput.Create;
begin
  inherited;

  FEnabled := True;
  FNullMoveStr := '@@@@';
end;

procedure TXBoardEngineOutput.DoImportMove;
var
  Analysis: PAnalysis;
begin
  StartWrite;
  try
    Write('move ', ccKeyword);
    if AnalysisInfo.BestMove <> '' then
      WriteLine(AnalysisInfo.BestMove, ccValue)
    else
      WriteLine(NullMoveStr, ccValue);

    Analysis := AnalysisInfo.LastAnalysis;
    if (Analysis <> nil) then
    begin
      if Analysis.Status.Score = scWin then
      begin
        Write('result ', ccKeyword);
        WriteLine('1-0 {White mates}', ccValue);
      end
      else if Analysis.Status.Score = -scWin then
      begin
        Write('result ', ccKeyword);
        WriteLine('0-1 {Black mates}', ccValue);
      end;
    end;
  finally
    StopWrite;
  end;
end;

procedure TXBoardEngineOutput.DrawAMoves;
var
	OutText: string;
	Analysis: PAnalysis;
begin
  inherited;

  if not Enabled then
    Exit;

	Analysis := AnalysisInfo.ActualAnalysis;
	if Analysis <> nil then
	begin
    StartWrite;
    try
      OutText :=
        IntToStr(Analysis.Depth) + CharSpace +
        ScoreToStr(Analysis.Status.Score, Analysis.Status.ScoreBound) + CharSpace +
        IntToStr(RoundDiv(Analysis.ElapsedTime.Milliseconds, 10)) + CharSpace + // centi-second
        IntToStr(Analysis.Nodes) + CharSpace;
{     if ExtendedVersion then
        OutText := OutText +
          IntToStr(Analysis.SelDepth) + CharSpace +
          IntToStr(AnalysisInfo.AvgNodes) + CharSpace +
          '0' + CharSpace + // Reserved
          IntToStr(AnalysisInfo.EndgameTableBasesHits) + CharTab; }
      OutText := OutText +
        AnalysisInfo.ActualAnalysisToString(NullMoveStr);
      if Analysis.Status.ScoreBound = sbLower then
        OutText := OutText + ' ?'
      else if Analysis.Status.ScoreBound = sbUpper then
        OutText := OutText + ' !';

      WriteLine(OutText, ccValue);
    finally
      StopWrite;
    end;
	end;
end;

procedure TXBoardEngineOutput.DrawCurrLine(const ALine: string);
begin
  inherited;

	TellGUIInfo('Current line: ' + ALine);
end;

procedure TXBoardEngineOutput.DrawDepth;
begin
  inherited;

  // Not supported by protocol
end;

procedure TXBoardEngineOutput.DrawMove1;
begin
  inherited;

  // Not supported by protocol
end;

procedure TXBoardEngineOutput.DrawNodes;
begin
  // Reply to . command

  inherited;

  if AnalysisInfo <> nil then
  begin
    StartWrite;
    try
      Write('stat01: ', ccKeyword);
      WriteLine(
        IntToStr(Round(AnalysisInfo.ElapsedTime.Elapsed.Milliseconds / 10)) + CharSpace + // centi-second
        IntToStr(AnalysisInfo.Nodes) + CharSpace +
        IntToStr(AnalysisInfo.Depth) + CharSpace +
        IntToStr(AnalysisInfo.MoveCount - AnalysisInfo.CurrentMoveIndex - 1) + CharSpace + // left to consider moves
        IntToStr(AnalysisInfo.MoveCount) + CharSpace +
        AnalysisInfo.CurrentMove, ccValue);
    finally
      StopWrite;
    end;
  end
  else
    TellGUIError('No analysis available.');
end;

procedure TXBoardEngineOutput.DrawRefutation;
begin
  inherited;

  // Not supported by protocol
end;

procedure TXBoardEngineOutput.DrawSelDepth;
begin
  inherited;

  // Not supported by protocol
end;

procedure TXBoardEngineOutput.OfferDraw;
begin
  inherited;

  StartWrite;
  try
    WriteLine('offer draw', ccKeyword);
  finally
    StopWrite;
  end;
end;

procedure TXBoardEngineOutput.OneSecond;
begin
  inherited;

  // No code
end;

procedure TXBoardEngineOutput.Resign;
begin
  inherited;

  StartWrite;
  try
    WriteLine('resign', ccKeyword);
  finally
    StopWrite;
  end;
end;

function TXBoardEngineOutput.ScoreToStr(const AScore: TScore; const ABoundScore: TScoreBound): string;
{const
  MateOffset = 100000; // DNW in Arena
var
  Score: SG;
begin
	if Abs(AScore) < scWin0 then
    Score := AScore;
  else
  begin
    if AScore > 0 then
      Score := MateOffset + scWin + 1 - AScore
    else
      Score := -MateOffset - (scWin + 1 + AScore);
  end;}
begin
  Result := IntToStr(AScore);
end;

procedure TXBoardEngineOutput.SetEnabled(const Value: BG);
begin
  FEnabled := Value;
end;

procedure TXBoardEngineOutput.ShowBestMove(const APonder: BG);
begin
  inherited;

end;

procedure TXBoardEngineOutput.Start;
begin
  inherited;

end;

procedure TXBoardEngineOutput.TellGUIError(const AMessage: string);
begin
  inherited;

  StartWrite;
  try
//	WriteLine('tellusererror ' + AMessage); creates message dialog box in GUI
    WriteLine('Error (' + AMessage + ')', ccError);
  finally
    StopWrite;
  end;
end;

procedure TXBoardEngineOutput.TellGUIInfo(const AMessage: string);
begin
  inherited;

  StartWrite;
  try
//	WriteLine('telluser ' + AMessage); creates message dialog box in GUI
//	WriteLine('tellall ' + AMessage);  creates message dialog box in GUI
    WriteLine('# ' + AMessage, ccInfo);
  finally
    StopWrite;
  end;
end;

procedure TXBoardEngineOutput.TellGUIDebug(const AMessage: string);
begin
  Assert(DebugMode = True);
  inherited;

  StartWrite;
  try
    WriteLine('# ' + AMessage, ccDebug);
  finally
    StopWrite;
  end;
end;


end.
