unit uUCIEngineOutput;

interface

uses
  uTypes,
  uTimeSpan,

  uProtocolEngineOutput,
  uScore;

type
  TUCIEngineOutput = class(TProtocolEngineOutput)
  private
    FShowCurrentMoveOnChange: BG;
    FShowCurrentMoveEverySecond: BG;

    FLastDrawSelDepthTime: TTimeSpan;
    FLastDrawSelDepth: SG;

    FLastDrawMove1Time: TTimeSpan;
    FLastCurrentMoveIndex: SG;
    FLastCurrentMove: string;
    FShowSelectiveDepthOnChange: BG;
    FShowSelectiveDepthEverySecond: BG;

    procedure PeriodicWrite;
    procedure DrawMove1Internal;
    procedure DrawSelDepthInternal;
    procedure SetShowCurrentMoveEverySecond(const Value: BG);
    procedure SetShowCurrentMoveOnChange(const Value: BG);
    procedure SetShowSelectiveDepthEverySecond(const Value: BG);
    procedure SetShowSelectiveDepthOnChange(const Value: BG);
  public
    constructor Create;

    property ShowCurrentMoveEverySecond: BG read FShowCurrentMoveEverySecond write SetShowCurrentMoveEverySecond;
    property ShowCurrentMoveOnChange: BG read FShowCurrentMoveOnChange write SetShowCurrentMoveOnChange;

    property ShowSelectiveDepthEverySecond: BG read FShowSelectiveDepthEverySecond write SetShowSelectiveDepthEverySecond;
    property ShowSelectiveDepthOnChange: BG read FShowSelectiveDepthOnChange write SetShowSelectiveDepthOnChange;

    procedure TellGUIError(const AMessage: string); override;
    procedure TellGUIInfo(const AMessage: string); override;
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

    function ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string; override;
  end;

implementation

uses
  SysUtils,

  uMath,
  uLog,
  uTextType,

  uAnalysis;

{ TUCIEngineOutput }

procedure TUCIEngineOutput.AcceptDraw;
begin
  inherited;

  // UCI engine can not accept draw
  TellGUIInfo('I accept a draw!');
end;

constructor TUCIEngineOutput.Create;
begin
  inherited;

  FNullMoveStr := '0000';
end;

procedure TUCIEngineOutput.DoImportMove;
begin
  inherited;

  raise Exception.Create('Do not call DoImpportMove in UCI protocol.');
end;

function TUCIEngineOutput.ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string;
const
	BoundToUCIStr: array[TScoreBound] of string = ('', '', 'lowerbound', 'upperbound');
const
	Minus = #$96; // #$97
	Plus = '+';
	Inf = 'inf';
begin
	if Abs(AScore) < scWin0 then
	begin
		Result := 'cp ' + IntToStr(AScore)
	end
	else if AScore = scoNone then
	begin
		Result := 'none'
	end
	else if Abs(AScore) = scMax then
	begin
		if AScore < 0 then
      Result := Minus + Inf
    else
      Result := Result + Plus + Inf;
	end
	else if AScore < 0 then
	begin
		Result := 'mate -' + IntToStr((scWin + AScore) div 2 + 1)
	end
	else
	begin
		Result := 'mate ' + IntToStr((scWin - AScore) div 2 + 1)
	end;
  if AScoreBound <> sbExact then
    if BoundToUCIStr[AScoreBound] <> '' then
      Result := Result + ' ' + BoundToUCIStr[AScoreBound];
end;

procedure TUCIEngineOutput.SetShowCurrentMoveEverySecond(const Value: BG);
begin
  FShowCurrentMoveEverySecond := Value;
end;

procedure TUCIEngineOutput.SetShowCurrentMoveOnChange(const Value: BG);
begin
  FShowCurrentMoveOnChange := Value;
end;

procedure TUCIEngineOutput.SetShowSelectiveDepthEverySecond(const Value: BG);
begin
  FShowSelectiveDepthEverySecond := Value;
end;

procedure TUCIEngineOutput.SetShowSelectiveDepthOnChange(const Value: BG);
begin
  FShowSelectiveDepthOnChange := Value;
end;

procedure TUCIEngineOutput.ShowBestMove(const APonder: BG);
begin
  inherited;

  DrawNodes;

  StartWrite;
  try
    Write('bestmove ', ccKeyword);
    if (AnalysisInfo.BestMove <> '') then
    begin
      if APonder and (Length(AnalysisInfo.LastAnalysis.Moves) >= 2) then
      begin
        Write(AnalysisInfo.BestMove, ccValue);
        Write(' ponder ', ccKeyword);
        WriteLine(AnalysisInfo.LastAnalysis.Moves[1], ccValue);
      end
      else
        WriteLine(AnalysisInfo.BestMove, ccValue);
    end
    else
    begin
      WriteLine(NullMoveStr, ccValue);
    end;
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.Start;
begin
  inherited;

  FLastDrawSelDepthTime.Ticks := 0;
  FLastDrawSelDepth := 0;

  FLastDrawMove1Time.Ticks := 0;
  FLastCurrentMoveIndex := 0;
  FLastCurrentMove := '';
end;

procedure TUCIEngineOutput.DrawAMoves;
var
	Analysis: PAnalysis;
begin
  inherited;

	Analysis := AnalysisInfo.ActualAnalysis;
	if Analysis <> nil then
	begin
    StartWrite;
    try
      Write('info depth ', ccKeyword);
      Write(IntToStr(Analysis.Depth), ccValue);
      Write(' seldepth ', ccKeyword);
      Write(IntToStr(Analysis.SelDepth), ccValue);
      Write(' multipv ', ccKeyword);
      Write('1', ccValue);
{ if eoMultiPV.Value > 1 then
      OutText := OutText + ' multipv ' + IntToStr(); TODO : Implement }
      Write(' time ', ccKeyword);
      Write(IntToStr(Analysis.ElapsedTime.Milliseconds), ccValue);
      Write(' nodes ', ccKeyword);
      Write(IntToStr(Analysis.Nodes), ccValue);
      Write(' nps ', ccKeyword);
      Write(IntToStr(Analysis.GetAvg), ccValue);
      Write(' score ', ccKeyword);
      Write(ScoreToStr(Analysis.Status.Score, Analysis.Status.ScoreBound), ccValue);
      Write(' pv ', ccKeyword);
      WriteLine(AnalysisInfo.ActualAnalysisToString(NullMoveStr), ccValue);
    finally
      StopWrite;
    end;
	end;
end;

procedure TUCIEngineOutput.DrawCurrLine(const ALine: string);
begin
  inherited;

  StartWrite;
  try
  	Write('info currline ', ccKeyword);
    WriteLine(ALine, ccValue);
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.DrawDepth;
begin
  inherited;

  StartWrite;
  try
    Write('info depth ', ccKeyword);
    Write(IntToStr(AnalysisInfo.Depth), ccValue);
    PeriodicWrite;
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.DrawMove1;
begin
  inherited;

  if FShowCurrentMoveOnChange or
    (FShowCurrentMoveEverySecond and (AnalysisInfo.ElapsedTime.Elapsed.Difference(FLastDrawMove1Time).Seconds > 1)) then
    DrawMove1Internal;
end;

procedure TUCIEngineOutput.DrawMove1Internal;
begin
  if (AnalysisInfo.CurrentMoveIndex = FLastCurrentMoveIndex) and (AnalysisInfo.CurrentMove = FLastCurrentMove) then
    Exit;

  FLastDrawMove1Time := AnalysisInfo.ElapsedTime.Elapsed;
  FLastCurrentMoveIndex := AnalysisInfo.CurrentMoveIndex;
  FLastCurrentMove := AnalysisInfo.CurrentMove;
  StartWrite;
  try
    Write('info currmove ', ccKeyword);

    if AnalysisInfo.CurrentMoveIndex = -1 then
      Write(NullMoveStr, ccValue)
    else
      Write(AnalysisInfo.CurrentMove, ccValue);

    Write(' currmovenumber ', ccKeyword);
    WriteLine(IntToStr(AnalysisInfo.CurrentMoveIndex + 1), ccValue);
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.DrawNodes;
begin
  // Called every second or at the end

  inherited;

  StartWrite;
  try
    Write('info', ccKeyword);
    PeriodicWrite;
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.DrawRefutation;
begin;
  inherited;

end;

procedure TUCIEngineOutput.DrawSelDepth;
begin
  inherited;

  if FShowSelectiveDepthOnChange or
    (FShowSelectiveDepthEverySecond and (AnalysisInfo.ElapsedTime.Elapsed.Difference(FLastDrawSelDepthTime).Seconds > 1)) then
    DrawSelDepthInternal;
end;

procedure TUCIEngineOutput.DrawSelDepthInternal;
begin
  if (AnalysisInfo.SelDepth = FLastDrawSelDepth) then
    Exit;

  FLastDrawSelDepth := AnalysisInfo.SelDepth;
  FLastDrawSelDepthTime := AnalysisInfo.ElapsedTime.Elapsed;

  StartWrite;
  try
    // Duplicite but required by protocol specification
    Write('info depth ', ccKeyword);
  	Write(IntToStr(AnalysisInfo.Depth), ccValue);

    Write(' seldepth ', ccKeyword);
  	WriteLine(IntToStr(AnalysisInfo.SelDepth), ccValue);
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.OfferDraw;
begin
  inherited;

  // UCI engine can not offer draw
  TellGUIInfo('I offer a draw!');
end;

procedure TUCIEngineOutput.OneSecond;
begin
  inherited;

  DrawNodes;
  if FShowCurrentMoveEverySecond then
    DrawMove1Internal;

  if FShowSelectiveDepthEverySecond then
    DrawSelDepthInternal;
end;

procedure TUCIEngineOutput.PeriodicWrite;
var
  NodesPerSec: U8;
  Elapsed: TTimeSpan;
begin
  inherited;

  Elapsed := AnalysisInfo.ElapsedTime.Elapsed;
  Write(' time ', ccKeyword);
  Write(IntToStr(Elapsed.Milliseconds), ccValue);
  Write(' nodes ', ccKeyword);
  Write(IntToStr(AnalysisInfo.Nodes), ccValue);
  try
    if Elapsed.Ticks > 0 then
    begin
      NodesPerSec := RoundU8(AnalysisInfo.Nodes / Elapsed.SecondsAsF);
      Write(' nps ', ccKeyword);
      Write(IntToStr(NodesPerSec), ccValue);
    end;
  except
    on E: Exception do
    begin
      LogException(E);
    end;
  end;

  if AnalysisInfo.HashPos > 0 then
  begin
    Write(' hashfull ', ccKeyword);
    Write(IntToStr(RoundDiv(1000 * AnalysisInfo.HashNew, AnalysisInfo.HashPos)), ccValue);
  end;

  if AnalysisInfo.EndgameTableBasesHits > 0 then
  begin
    Write(' tbhits ', ccKeyword);
    Write(IntToStr(AnalysisInfo.EndgameTableBasesHits), ccValue);
  end;

  WriteLine('', ccValue)
end;

procedure TUCIEngineOutput.Resign;
begin
  inherited;

  // UCI engine can not resign
end;

procedure TUCIEngineOutput.TellGUIDebug(const AMessage: string);
begin
  Assert(DebugMode = True);

  inherited;

  StartWrite;
  try
    WriteLine('info string Debug: ' + AMessage, ccDebug);
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.TellGUIError(const AMessage: string);
begin
  inherited;

  StartWrite;
  try
    WriteLine('info string Error: ' + AMessage, ccError);
  finally
    StopWrite;
  end;
end;

procedure TUCIEngineOutput.TellGUIInfo(const AMessage: string);
begin
  inherited;

  StartWrite;
  try
    WriteLine('info string ' + AMessage, ccInfo);
  finally
    StopWrite;
  end;
end;

end.
