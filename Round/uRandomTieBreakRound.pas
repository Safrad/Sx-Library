unit uRandomTieBreakRound;

interface

uses
  uTypes,
  uRandomRound;

type
  TRandomTieBreakRound = class(TRandomRound)
  private
    FTowardsZero: BG;
  public
    function RoundF8(const AF8: F8): F8;
  end;

implementation

{ TRandomTieBreakRound }

function TRandomTieBreakRound.RoundF8(const AF8: F8): F8;
begin
  if Frac(AF8) = 0.5 then
  begin
    if FSxRandomGenerator.RandomU1 <= 127 then
      Result := Trunc(AF8)
    else
      Result := AwayFromZero(AF8);
  end
  else
    Result := Round(AF8);
end;

end.

