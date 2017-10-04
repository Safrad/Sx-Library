unit uContinuousStatistics;

interface

uses
  uTypes;

type
  TContinuousStatistics = class
  private
    FSuma: FG;
    FCount: U8;
    FMinimum: FG;
    FMaximum: FG;

    function GetAverage: FG;
 public
    procedure Add(const ANumber: FG);
    procedure Reset;

    property Count: U8 read FCount;
    property Minimum: FG read FMinimum;
    property Maximum: FG read FMaximum;
    property Average: FG read GetAverage;
  end;

implementation

uses
  Math;

{ TStatistics }

procedure TContinuousStatistics.Add(const ANumber: FG);
begin
  if FCount = 0 then
  begin
    FMinimum := ANumber;
    FMaximum := ANumber;
  end
  else
  begin
    if FMinimum > ANumber then
      FMinimum := ANumber;
    if FMaximum < ANumber then
      FMaximum := ANumber;
  end;

  FSuma := FSuma + ANumber;
  Inc(FCount);
end;

function TContinuousStatistics.GetAverage: FG;
begin
  if FCount > 0 then
    Result := FSuma / FCount
  else
    Result := NaN;
end;

procedure TContinuousStatistics.Reset;
begin
  FCount := 0;
  FSuma := 0;
  FMinimum := 0;
  FMaximum := 0;
end;

end.
 