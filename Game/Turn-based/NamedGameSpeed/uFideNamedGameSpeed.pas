unit uFideNamedGameSpeed;

interface

uses
  uNamedGameSpeed;

type
  TFideNamedGameSpeed = class(TNamedGameSpeed)
  protected
    function GetGameSpeed: TGameSpeed; override;
  end;

implementation

uses
  uTypes;

{ TFideNamedGameSpeed }

function TFideNamedGameSpeed.GetGameSpeed: TGameSpeed;
var
  AverageSecondsPerGame40: UG;
  AverageSecondsPerGame60: UG;
begin
  AverageSecondsPerGame40 := AverageGameTime(40).Seconds;
  AverageSecondsPerGame60 := AverageGameTime(60).Seconds;
  if AverageSecondsPerGame40 <= 15 then
    Result := gsUltraBullet
  else if AverageSecondsPerGame40 < 30 then
    Result := gsHyperBullet
  else if AverageSecondsPerGame40 < 3 * 60 then
    Result := gsBullet
  else if AverageSecondsPerGame60 < 10 * 60 then
    Result := gsBlitz
  else if AverageSecondsPerGame60 < 60 * 60 then
    Result := gsRapid
  else if AverageSecondsPerGame60 < 24 * 60 * 60 then
    Result := gsClassical
  else
    Result := gsCorrespondence;
end;

end.
