unit uContinuousStatistics;

interface

uses
  uTypes;

type
  TContinuousStatistics = class
  private
    FSum: FG;
    FCount: U8;
    FMin: FG;
    FMax: FG;

    function GetAvg: FG;
 public
    procedure Add(const ANumber: FG);
    procedure Reset;
    function ResultAsLines(const AUnit: string): string;

    property Min: FG read FMin;
    property Max: FG read FMax;
    property Avg: FG read GetAvg;
  end;

implementation

uses
  Math,
  uStrings,
  uOutputFormat;

{ TStatistics }

procedure TContinuousStatistics.Add(const ANumber: FG);
begin
  if FCount = 0 then
  begin
    FMin := ANumber;
    FMax := ANumber;
  end
  else
  begin
    if FMin > ANumber then
      FMin := ANumber;
    if FMax < ANumber then
      FMax := ANumber;
  end;

  FSum := FSum + ANumber;
  Inc(FCount);
end;

function TContinuousStatistics.GetAvg: FG;
begin
  if FCount > 0 then
    Result := FSum / FCount
  else
    Result := NaN;
end;

procedure TContinuousStatistics.Reset;
begin
  FCount := 0;
  FSum := 0;
  FMin := 0;
  FMax := 0;
end;

function TContinuousStatistics.ResultAsLines(const AUnit: string): string;
begin
  if FCount = 0 then
  begin
    Result := NAStr;
  end
  else
  begin
    Result :=
      'Minimum: ' + FToS(Min) + CharSpace + AUnit + LineSep +
      'Maximum: ' + FToS(Max) + CharSpace + AUnit + LineSep +
      'Average: ' + FToS(Avg) + CharSpace + AUnit + LineSep;
  end;
end;

end.
 