unit uVariationCut;

interface

type
	TVariationCut = (
		vcNone, // "unterminated": game not terminated.
								// "normal": game terminated in a normal fashion.
		// Engine only
    vcGameFinished,
		vcSpecific,
		vcAlpha, // FailLow
		vcBeta, // FailHigh
		vcLimit,
		vcCalm,
		vcDepth,
		vcSelectivity,
		vcFutility,
		vcMaxDepth,
		vcHashAlpha,
		vcHashBeta,
    vcMovesRule,
    vcRepetition
  );

implementation

end.
