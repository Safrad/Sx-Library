unit uTimeSpan;

interface

uses
  uTypes;

type
  TTimeSpan = class
  private
    FTicks: U8;
    procedure SetMicroseconds(const Value: FG);
    procedure SetMilliseconds(const Value: FG);
    procedure SetTicks(const Value: U8);
    procedure SetTime(const Value: TDateTime);
    function GetMicroseconds: FG;
    function GetMilliseconds: FG;
    function GetTime: TDateTime;
    procedure SetFrequency(const Value: FG);
    function GetFrequency: FG;
    function GetSeconds: FG;
    procedure SetSeconds(const Value: FG);
  public
    // The total elapsed time measured by the current instance, in timer ticks.
    property Ticks: U8 read FTicks write SetTicks;

    // The total elapsed time measured by the current instance, in milliseconds.
    property Seconds: FG read GetSeconds write SetSeconds;

    // The total elapsed time measured by the current instance, in milliseconds.
    property Milliseconds: FG read GetMilliseconds write SetMilliseconds;

    // The total elapsed time measured by the current instance, in microseconds.
    property Microseconds: FG read GetMicroseconds write SetMicroseconds;

    // The total elapsed time measured by the current instance.
    property Time: TDateTime read GetTime write SetTime;

    property Frequency: FG read GetFrequency write SetFrequency;
  end;

implementation

uses
  uMath;

{ TTimeSpan }

function TTimeSpan.GetFrequency: FG;
begin
  Result := PerformanceFrequency / FTicks;
end;

function TTimeSpan.GetMicroseconds: FG;
begin
  Result := 1000 * Second * FTicks / PerformanceFrequency;
end;

function TTimeSpan.GetMilliseconds: FG;
begin
  Result := Second * FTicks / PerformanceFrequency;
end;

function TTimeSpan.GetSeconds: FG;
begin
  Result := FTicks / PerformanceFrequency;
end;

function TTimeSpan.GetTime: TDateTime;
begin
  Result := FTicks / (PerformanceFrequency * (Day div Second));
end;

procedure TTimeSpan.SetFrequency(const Value: FG);
begin
  SetTicks(Round(PerformanceFrequency / Value));
end;

procedure TTimeSpan.SetMicroseconds(const Value: FG);
begin
  SetTicks(Round(PerformanceFrequency * Value / (1000 * Second)));
end;

procedure TTimeSpan.SetMilliseconds(const Value: FG);
begin
  SetMicroseconds(Second * Value);
end;

procedure TTimeSpan.SetSeconds(const Value: FG);
begin
  SetMicroseconds(1000 * Second * Value);
end;

procedure TTimeSpan.SetTicks(const Value: U8);
begin
  FTicks := Value;
end;

procedure TTimeSpan.SetTime(const Value: TDateTime);
begin
  SetTicks(Round(Value * (Day / Second) * PerformanceFrequency));
end;

end.
