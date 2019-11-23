unit uChessDotComNamedGameSpeed;

interface

uses
  uNamedGameSpeed;

type
  TChessDotComNamedGameSpeed = class(TNamedGameSpeed)
  protected
    function GetGameSpeed: TGameSpeed; override;
  end;

implementation

uses
  uTypes;

{ TChessDotComNamedGameSpeed }

function TChessDotComNamedGameSpeed.GetGameSpeed: TGameSpeed;
var
  AverageSecondsPerGame: UG;
begin
  AverageSecondsPerGame := AverageGameTime.Seconds;
  if AverageSecondsPerGame < 3 * 60 then
    Result := gsBullet
  else if AverageSecondsPerGame < 15 * 60 then
    Result := gsBlitz
  else if AverageSecondsPerGame < 24 * 60 * 60 then
    Result := gsRapid
  else
    Result := gsCorrespondence;
end;

end.
