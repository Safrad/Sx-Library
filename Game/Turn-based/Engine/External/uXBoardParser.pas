unit uXBoardParser;

interface

uses
  uExternalEngineParser;

type
  TXBoardParser = class(TExternalEngineParser)
  public
    procedure Parse(const Line: string); override;
  end;

implementation

uses
  SysUtils,

  uTypes,
  uChar,
  uStrings,
  uMainLog,

  uSubtreeStatus,
  uAnalysisInfo;

{ TXBoardParser }

procedure TXBoardParser.Parse(const Line: string);
var
	s: string;
	InLineIndex: SG;
  BestStatus: TSubtreeStatus;
  Moves: TArrayOfString;
begin
  inherited;

	InLineIndex := 1;
	while InLineIndex < Length(Line) do
	begin
		if CharInSet(Line[InLineIndex], [CharCR, CharLF]) then
		begin
			Inc(InLineIndex);
			Continue;
		end;
    if Line[InLineIndex] = '#' then
    begin
      // Comment
      ReadToChar(Line, InLineIndex, CharCR);
      Continue;
    end;

    SkipSpace(Line, InLineIndex);
		s := ReadToChar(Line, InLineIndex, CharSpace);
		if s = 'offer' then
    begin
		  s := ReadToChar(Line, InLineIndex, CharSpace);
      Assert(s = 'draw');
    end
		else if s = 'move' then
		begin
      Engine.AnalysisInfo.ElapsedTime.Stop;
		  Engine.AnalysisInfo.BestMove := ReadToChar(Line, InLineIndex, CharSpace);
      Engine.CalculationDone(Self);
		end
		else if CharInSet(FirstChar(s), ['0'..'9']) then
		begin
      BestStatus := Default(TSubtreeStatus);

			Engine.AnalysisInfo.Depth := ReadSGFast(s);
      BestStatus.Score := ReadSGFast(Line, InLineIndex);
      ReadSGFast(Line, InLineIndex); // Skipped, used from AnalysisInfo timer
			Engine.AnalysisInfo.Nodes := ReadS8Fast(Line, InLineIndex);
      SkipSpace(Line, InLineIndex);
      Moves := SplitStringEx(ReadToNewLine(Line, InLineIndex), CharSpace);
      Engine.AnalysisInfo.AddAnalysis(BestStatus, Moves);
		end
		else
    begin
      if MainLog.IsLoggerFor(mlWarning) then
  			MainLog.Add('Unknown command ''' + s + '''', mlWarning);
			ReadToNewLine(Line, InLineIndex);
    end;
	end;
end;

end.
