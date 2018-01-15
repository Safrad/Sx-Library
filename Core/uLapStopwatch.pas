unit uLapStopwatch;

interface

uses
  uTypes,
  uStopwatch;

type
  TLapStopwatch = class(TStopwatch)
  private
    FLapTicks: TArrayOfU8;
    FLapCount: UG;
    procedure Clear;
  public
    procedure StoreLap;
    function LapTicks(const ALapNumber: SG): U8;

    procedure Reset; reintroduce;
    procedure Restart; reintroduce;
    property LapCount: UG read FLapCount;
  end;

implementation

{ TRoundStopwatch }

procedure TLapStopwatch.Clear;
begin
  FLapCount := 0;
  SetLength(FLapTicks, 0);
end;

function TLapStopwatch.LapTicks(const ALapNumber: SG): U8;
begin
  if ALapNumber < Length(FLapTicks) then
    Result := FLapTicks[ALapNumber]
  else
    Result := 0;
end;

procedure TLapStopwatch.StoreLap;
begin
  Inc(FLapCount);
  SetLength(FLapTicks, FLapCount);
  FLapTicks[FLapCount - 1] := Elapsed.Ticks;
  if IsRunning then
    inherited Restart
  else
    inherited Reset;
end;

procedure TLapStopwatch.Reset;
begin
  inherited Reset;

  Clear;
end;

procedure TLapStopwatch.Restart;
begin
  inherited Restart;

  Clear;
end;

end.
