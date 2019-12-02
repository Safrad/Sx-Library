unit uSubtreeStatus;

interface

uses
  uTypes,

  uScore,
  uVariationCut;

type
	TSubtreeStatus = record
		Score: TScore; // 2
		ScoreBound: TScoreBound; // 1
		VariationCut: TVariationCut; // 1
		MoveCount: U1; // 1
	end;

implementation

end.
