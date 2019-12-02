unit uStopCause;

interface

type
	TStopCause = (
    scNone,

    scUserAbort,
    scNoChoise,
    scNoIncrement,
    scWinScore,
    scLoseScore,

    scLevelOnNode,
    scLevelOnMove,
    scLevelOnAnalysis,
    scLevelOnDepth
  );

var
	StopCauseStrings: array[TStopCause] of string;

implementation

uses
  uStrings;

initialization
	EnumToStr(TypeInfo(TStopCause), StopCauseStrings);
end.
