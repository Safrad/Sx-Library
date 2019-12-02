unit uUSIEngineOutput;

interface

uses
  uScore,
  uUCIEngineOutput;

type
  TUSIEngineOutput = class(TUCIEngineOutput)
  public
    function ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string; override;
  end;

implementation

uses
  SysUtils;

{ TUSIEngineOutput }

function TUSIEngineOutput.ScoreToStr(const AScore: TScore; const AScoreBound: TScoreBound): string;
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
		Result := 'mate -' + IntToStr((scWin + AScore) + 1)
	end
	else
	begin
		Result := 'mate ' + IntToStr((scWin - AScore) + 1)
	end;
  if AScoreBound <> sbExact then
    if BoundToUCIStr[AScoreBound] <> '' then
      Result := Result + ' ' + BoundToUCIStr[AScoreBound];
end;

end.
