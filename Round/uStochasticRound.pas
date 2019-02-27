unit uStochasticRound;

interface

uses
  uTypes,
  uRandomRound;

type
  TStochasticRound = class(TRandomRound)
  public
    function RoundF8(const AF8: F8): F8;
  end;

implementation

uses
  Math;

{ TStochasticRound }

function TStochasticRound.RoundF8(const AF8: F8): F8;
var
  FractionPart: F8;
begin
  FractionPart := Frac(AF8);

  if FSxRandomGenerator.RandomU4 >= FractionPart * (High(U4) + 1) then
    Result := Trunc(AF8)
  else
    Result := AwayFromZero(AF8);
end;

end.
