unit uScore;

interface

uses
  uTypes;

type
  // [centi-pawn]
  // from actual player view
  // > 0 means position for actual player is better
  // < 0 means position for actual player is worse
	TScore = S2;

	TScoreBound = (sbNone, sbExact, sbLower{vfFailHigh}, sbUpper{vfFailLow});

const
	ScoreBase = 100; // centipawn
	scMax = 32767;
	scMin = -32768;
	scWin = 32766; // #1
	scWin0 = 32001; // EPD specification; scWin - MaxDepth div 2; // 32734
	scoNone = -32768; // 32768, scNone is used in Classes
	{
	-scMax = -32767
	-scWin = -32766 -#1

	-scWin0 = -32734 -#33

	Fritz: -32001 -#766
	Normal Score -32733..32733

	scWin0 = 32734 #33

	scWin = 32766 #1
	scMax = 32767
	}


function InvertScore(const AScore: TScore; const APlayer: SG): TScore;
function InvertScoreBound(const AScoreBound: TScoreBound; const APlayer: SG): TScoreBound;

implementation

function InvertScore(const AScore: TScore; const APlayer: SG): TScore;
begin
  if (APlayer <> 0){50%} and (AScore <> scoNone){99%} then
  begin
    // Invert score
    Result := -AScore;
  end
  else
  begin
    Result := AScore;
  end;
end;

function InvertScoreBound(const AScoreBound: TScoreBound; const APlayer: SG): TScoreBound;
begin
  if (AScoreBound <> sbExact){10%} and (APlayer <> 0){50%} then
  begin
    // Invert score bound
    if AScoreBound = sbUpper then
      Result := sbLower
    else if AScoreBound = sbLower then
      Result := sbUpper
    else // sbNone
      Result := sbNone;
  end
  else
  begin
    Result := AScoreBound;
  end;
end;

end.
