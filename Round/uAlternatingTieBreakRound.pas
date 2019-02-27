unit uAlternatingTieBreakRound;

interface

uses
  uTypes,
  uRound;

type
  TAlternatingTieBreakRound = class(TRound)
  private
    FTowardsZero: BG;
  public
    function RoundF8(const AF8: F8): F8;
  end;

implementation

{ TAlternatingTieBreakRound }

function TAlternatingTieBreakRound.RoundF8(const AF8: F8): F8;
begin
  if FTowardsZero then
    Result := Trunc(AF8)
  else
    Result := AwayFromZero(AF8);

  FTowardsZero := not FTowardsZero;
end;

end.
