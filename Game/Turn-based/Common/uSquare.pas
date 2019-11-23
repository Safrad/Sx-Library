unit uSquare;

interface

uses
  uTypes;

type
  TSide = U1; // 0..PlayerMax

  TSquare = S1; // S1 is fastest, but S4 is similar
  TPiece = S1; // S1 is fastest, but S4 is similar

  TSquareIndex = S1; // S1 or U1 is the fastest
  TSquares = array of TSquare;
const
  PlayerMax = 1;

  sqEmpty = 0;
  sqOut = 127; // sqNone

function SquareToSide(const ASquare: TSquare): TSide; inline;

implementation

function SquareToSide(const ASquare: TSquare): TSide;
begin
  Assert(ASquare <> sqEmpty);
  Assert(ASquare <> sqOut);
  Result := U1(ASquare) shr 7;
end;

end.
