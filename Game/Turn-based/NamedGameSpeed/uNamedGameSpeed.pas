unit uNamedGameSpeed;

interface

uses
  uTypes,
  uTimeSpan;

type
  TGameSpeed = (gsUltraBullet, gsHyperBullet, gsBullet, gsBlitz, gsRapid, gsClassical, gsCorrespondence);

  TNamedGameSpeed = class
  private
    FName: string;
    FTimePerMove: TTimeSpan;
    FTimePerGame: TTimeSpan;
    procedure SetMoveCount(const Value: UG);
    procedure SetName(const Value: string);
    procedure SetTimePerGame(const Value: TTimeSpan);
    procedure SetTimePerMove(const Value: TTimeSpan);
    function GetAverageGameTime(const AMoveCount: UG): TTimeSpan;
  public
    function GetGameSpeed: TGameSpeed; virtual; abstract;
  public
    // Input
    property TimePerMove: TTimeSpan read FTimePerMove write SetTimePerMove;
    property TimePerGame: TTimeSpan read FTimePerGame write SetTimePerGame;

    // Output
    property AverageGameTime: TTimeSpan read GetAverageGameTime;
    property Name: string read FName write SetName;
  end;

implementation

{ TNamedGameSpeed }

function TNamedGameSpeed.GetAverageGameTime(const AMoveCount: UG): TTimeSpan;
begin
  Result.Ticks := FTimePerGame.Ticks * FTimePerMove.Ticks * AMoveCount;
end;

procedure TNamedGameSpeed.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TNamedGameSpeed.SetTimePerGame(const Value: TTimeSpan);
begin
  FTimePerGame := Value;
end;

procedure TNamedGameSpeed.SetTimePerMove(const Value: TTimeSpan);
begin
  FTimePerMove := Value;
end;

end.
