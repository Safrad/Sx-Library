unit uUCIParser;

interface

uses
  uTypes,
  uExternalEngineParser;

type
  TUCIParser = class(TExternalEngineParser)
  private
    procedure ParseInfo(const Line: string; var InLineIndex: SG);
  public
    procedure Parse(const AText: string); override;
  end;

implementation

uses
  SysUtils,

  uSubtreeStatus,
  uScore,
  uChar,
  uStrings,
  uLog;

{ TUCIParser }

procedure TUCIParser.Parse(const AText: string);
var
	s: string;
	InLineIndex: SG;
begin
  inherited;

	InLineIndex := 1;
	while InLineIndex < Length(AText) do
	begin
		if CharInSet(AText[InLineIndex], [CharCR, CharLF]) then
		begin
			Inc(InLineIndex);
			Continue;
		end;
		s := ReadToChar(AText, InLineIndex, CharSpace);

		if s = 'readyok' then
    begin
      // Skipped
    end
		else if s = 'Experimental' { '...'} then // Chess engine "Toga" message
			ReadToChar(AText, InLineIndex, CharCR) // Skipped
		else if s = 'EgbbProbe' { ' not Loaded!'} then // Chess engine "Toga" message
			ReadToChar(AText, InLineIndex, CharCR) // Skipped
		else if s = 'bestmove' then
		begin
      Engine.AnalysisInfo.ElapsedTime.Stop;
		  Engine.AnalysisInfo.BestMove := ReadToChar(AText, InLineIndex, CharSpace);
      Engine.CalculationDone(Self);
      s := ReadToChar(AText, InLineIndex, CharSpace);
      if s = 'ponder' then
        ReadToChar(AText, InLineIndex, CharSpace); // Skipped
		end
		else if s = 'ponder' then
		begin
      // Skipped
		end
		else if s = 'info' then
		begin
      ParseInfo(AText, InLineIndex);
		end
		else
			MainLogAdd('Unknown command ''' + s + '''', mlWarning);
	end;
end;

procedure TUCIParser.ParseInfo(const Line: string; var InLineIndex: SG);
var
  BestStatus: TSubtreeStatus;
  Moves: TArrayOfString;
  s: string;

  NewDepth: SG;
  CurrMoveIndex: SG;
  CurrMove: string;
begin
  BestStatus := Default(TSubtreeStatus);

  NewDepth := -1;
  CurrMoveIndex := -1;
  CurrMove := '';

  while InLineIndex < Length(Line) do
  begin
    if CharInSet(Line[InLineIndex], [CharCR, CharLF]) then
    begin
      Break;
    end;
    s := ReadToChar(Line, InLineIndex, CharSpace);
    if s = '' then
      // Nothing
    else if s = 'depth' then
    begin
      NewDepth := ReadSGFast(Line, InLineIndex);
      Engine.AnalysisInfo.Depth := NewDepth;
    end
    else if s = 'seldepth' then
    begin
      Engine.AnalysisInfo.SelDepth := ReadSGFast(Line, InLineIndex);
    end
    else if s = 'upperbound' then
      BestStatus.ScoreBound := sbUpper
    else if s = 'lowerbound' then
      BestStatus.ScoreBound := sbLower
    else if s = 'score' then
    begin
      s := ReadToChar(Line, InLineIndex, CharSpace);
      if s = 'cp' then
        BestStatus.Score := ReadSGFast(Line, InLineIndex)
      else if s = 'mate' then
      begin
        if BestStatus.Score >= 0 then
          BestStatus.Score := scWin - BestStatus.Score + 1
        else
          BestStatus.Score := -scWin - BestStatus.Score - 1;
      end;
      BestStatus.ScoreBound := sbExact;
    end
    else if s = 'time' then
    begin
      ReadSGFast(Line, InLineIndex); // Skipped, used AnalysisInfo timer
    end
    else if s = 'nodes' then
    begin
      Engine.AnalysisInfo.Nodes := ReadS8Fast(Line, InLineIndex);
    end
    else if s = 'nps' then
    begin
      ReadS8Fast(Line, InLineIndex); // Skipped, used AnalysisInfo timer
    end
    else if s = 'hashfull' then
    begin
      Engine.AnalysisInfo.HashNew := ReadSGFast(Line, InLineIndex);
      Engine.AnalysisInfo.HashPos := 1000;
    end
    else if s = 'cpuload' then
    begin
      ReadSGFast(Line, InLineIndex); // Not used
    end
    else if s = 'tbhits' then
    begin
      Engine.AnalysisInfo.EndgameTableBasesHits := ReadSGFast(Line, InLineIndex);
    end
    else if s = 'currmovenumber' then
    begin
      CurrMoveIndex := ReadSGFast(Line, InLineIndex);
    end
    else if s = 'refutation' then
      ReadToNewLine(Line, InLineIndex)
    else if s = 'currline' then
      ReadToNewLine(Line, InLineIndex)
    else if s = 'currmove' then
    begin
      CurrMove := ReadToChar(Line, InLineIndex, CharSpace);
    end
    else if s = 'pv' then
    begin
      Moves := SplitStringEx(ReadToNewLine(Line, InLineIndex), CharSpace);
      Engine.AnalysisInfo.AddAnalysis(BestStatus, Moves);
    end
    else if s = 'multipv' then
    begin
      ReadSGFast(Line, InLineIndex);
    end
    else if s = 'string' then
    begin
      ReadToNewLine(Line, InLineIndex)
    end
    else
      MainLogAdd('Unknown info ''' + s + '''', mlWarning);
  end;

  if NewDepth <> -1 then
  begin
    Engine.AnalysisInfo.Depth := NewDepth;
  end;

  if (CurrMove <> '') or (CurrMoveIndex <> -1) then
  begin
    Engine.AnalysisInfo.SetCurrentMove(CurrMoveIndex, CurrMove);
  end;
end;

end.
