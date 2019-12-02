unit uLichessNamedGameSpeed;

interface

uses
  uNamedGameSpeed;

type
  TLichessNamedGameSpeed = class(TNamedGameSpeed)
  protected
    function GetGameSpeed: TGameSpeed; override;
  end;

implementation

uses
  uTypes;

{ TLichessNamedGameSpeed }

function TLichessNamedGameSpeed.GetGameSpeed: TGameSpeed;
var
  AverageSecondsPerGame: UG;
begin
  AverageSecondsPerGame := AverageGameTime.Seconds;
  if AverageSecondsPerGame < 30 then
    Result := gsUltraBullet
  else if AverageSecondsPerGame < 3 * 60 then
    Result := gsBullet
  else if AverageSecondsPerGame < 8 * 60 then
    Result := gsBlitz
  else if AverageSecondsPerGame < 25 * 60 then
    Result := gsRapid
  else if AverageSecondsPerGame < 24 * 60 * 60 then
    Result := gsClassical
  else
    Result := gsCorrespondence;
end;

end.
