unit uPortableGameNotation;

interface

type
	TPGNTag = (
		// STR  = Seven Tag Roster
		ptEvent,
		ptSite,
		ptDate,
		ptRound,
		ptWhite,
		ptBlack,
		ptResult,

		ptGameType, // Non PGN standard but is PDN standard
		ptVariant, // Not PGN standard but is PGN standard
		// Supplemental // 9.1
		ptWhiteTitle,
		ptBlackTitle,
		ptWhiteElo,
		ptBlackElo,
		ptWhiteUSCF,
		ptBlackUSCF,
		ptWhiteNA,
		ptBlackNA,
		ptWhiteType,
		ptBlackType,
		// 9.2
		ptEventDate,
		ptEventSponsor,
		ptSection,
		ptStage,
		ptBoard,
		// 9.3
		ptOpening,
		ptVariation,
		ptSubVariation,
		// 9.4
		ptECO,
		ptNIC,
		// 9.5
		ptTime,
		ptUTCTime,
		ptUTCDate,
		// 9.6
		ptTimeControl,
		ptWhiteTimeControl, // Non-standard (SDG)
		ptBlackTimeControl, // Non-standard (SDG)
		// 9.7
		ptSetUp,
		ptFEN,
		// 9.8
		ptTermination,
		// 9.9
		ptAnnotator,
		ptMode,
		ptPlyCount, // Fritz
		ptComment,
		ptSource,
		ptSourceDate,
		ptWhiteTeam,
		ptWhiteTeamCountry,
		ptBlackTeam,
		ptBlackTeamCountry,
		ptWhiteClock,
		ptBlackClock,
		ptRemark,
		ptTitle,
		ptCurrentMove // Non-standard (Virtual Chess)
		);

type
	TGameResult = (
		grUnknown, // Game in progress, result unknown, game abadoned ...
		grWhiteWin,
		grDraw,
		grBlackWin
		);

  TResultCause = (
		rcMates,
		rcNoPiece,
		rcLowMaterial,
		rcStalemate,
		rcRepetition,
		rcFiftyMoves, // MovesRule,
		rcTimeUp, // "time forfeit": loss due to losing player's failure to meet time control requirements.
		rcOfferDraw,
		rcResign,
    rcAbadoned, // abandoned game.
    rcAdjudication, // result due to third party adjudication process.
    rcDeath, // losing player called to greater things, one hopes.
    rcEmergency, // game concluded due to unforeseen circumstances.
    rcRulesInfraction // administrative forfeit due to losing player's failure to observe either the Laws of Chess or the event regulations.
  );

var
  ResultCauses: array[TResultCause] of string;

type
	TGameTermination = record
    Result: TGameResult;
    Cause: TResultCause;
  end;

	TPlayerType = (ptHuman, ptProgram);

const
	PlayerTypeStr: array[TPlayerType] of string = (
		'human',
		'program');

function GameResultToStr(const AGameResult: TGameResult): string;
function GameTerminationToStr(const AGameTermination: TGameTermination): string;

implementation

uses
  uStrings;

function GameResultToStr(const AGameResult: TGameResult): string;
begin
  case AGameResult of
  grUnknown: Result := '*';
  grWhiteWin: Result := '1-0';
  grBlackWin: Result := '0-1';
  grDraw: Result := '1/2-1/2';
  end;
end;

function GameTerminationToStr(const AGameTermination: TGameTermination): string;
begin
  Result := GameResultToStr(AGameTermination.Result);
  AppendStr(Result, '{');
  if AGameTermination.Result = grWhiteWin then
    AppendStr(Result, 'White ')
  else if AGameTermination.Result = grBlackWin then
    AppendStr(Result, 'Black ');
  AppendStr(Result, ResultCauses[AGameTermination.Cause]);
  AppendStr(Result, '}');
end;

initialization
  EnumToStr(TypeInfo(TResultCause), ResultCauses);
end.
