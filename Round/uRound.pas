unit uRound;

interface

uses
  uTypes;

type
  TRound = class
  public
    class function AwayFromZero(AF8: F8): F8;
  end;

implementation

{ TRound }

class function TRound.AwayFromZero(AF8: F8): F8;
begin
  if Frac(AF8) = 0 then
    Result := AF8
  else if AF8 > 0 then
    Result := Trunc(AF8) + 1
  else
    Result := Trunc(AF8) - 1;
end;

end.
