unit uNoProtocolEngineOutput;

interface

uses
  uTypes,
  uTimeSpan,

  uProtocolEngineOutput,
  uScore,
  uAnalysisInfo;

type
  TNoProtocolEngineOutput = class(TProtocolEngineOutput)
  public
    constructor Create;

    procedure TellGUIError(const AMessage: string); override;
    procedure TellGUIInfo(const AMessage: string); override;
    procedure TellGUIDebug(const AMessage: string); override;

    function ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string; override;
  end;

implementation

uses
  SysUtils,
  uMath,
  uTextType;

{ TNoProtocolEngineOutput }

constructor TNoProtocolEngineOutput.Create;
begin
  inherited;

  FNullMoveStr := '--';
end;

function TNoProtocolEngineOutput.ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string;
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

procedure TNoProtocolEngineOutput.TellGUIDebug(const AMessage: string);
begin
  Assert(DebugMode = True);

  inherited;

  StartWrite;
  try
    WriteLine(AMessage, ccDebug);
  finally
    StopWrite;
  end;
end;

procedure TNoProtocolEngineOutput.TellGUIError(const AMessage: string);
begin
  inherited;

  StartWrite;
  try
    WriteLine(AMessage, ccError);
  finally
    StopWrite;
  end;
end;

procedure TNoProtocolEngineOutput.TellGUIInfo(const AMessage: string);
begin
  inherited;

  StartWrite;
  try
    WriteLine(AMessage, ccInfo);
  finally
    StopWrite;
  end;
end;

end.
