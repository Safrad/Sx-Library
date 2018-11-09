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
    function GetDays: FG;
    function GetHours: FG;
    function GetMinutes: FG;
    procedure SetDays(const Value: FG);
    procedure SetHours(const Value: FG);
    procedure SetMinutes(const Value: FG);
  public
    // The total elapsed time measured by the current instance, in timer ticks.
    property Ticks: U8 read FTicks write SetTicks;

    // The total elapsed time measured by the current instance, in microseconds.
    property Microseconds: FG read GetMicroseconds write SetMicroseconds;

    // The total elapsed time measured by the current instance, in milliseconds.
    property Milliseconds: FG read GetMilliseconds write SetMilliseconds;

    // The total elapsed time measured by the current instance, in seconds.
    property Seconds: FG read GetSeconds write SetSeconds;

    // The total elapsed time measured by the current instance, in minutes.
    property Minutes: FG read GetMinutes write SetMinutes;

    // The total elapsed time measured by the current instance, in hours.
    property Hours: FG read GetHours write SetHours;

    // The total elapsed time measured by the current instance, in days (the same as property Time but better precision).
    property Days: FG read GetDays write SetDays;

    // The total elapsed time measured by the current instance, in days (the same as property Days but lower precision).
    property Time: TDateTime read GetTime write SetTime;

    property Frequency: FG read GetFrequency write SetFrequency;

    function ToStringInSeconds: string;
  end;

implementation

uses
  uMainTimer,
  uOutputFormat;

{ TTimeSpan }

function TTimeSpan.GetDays: FG;
begin
  Result := FTicks / (MainTimer.Frequency * (Day div Second));
end;

function TTimeSpan.GetFrequency: FG;
begin
  Result := MainTimer.Frequency / FTicks;
end;

function TTimeSpan.GetHours: FG;
begin
  Result := FTicks / (MainTimer.Frequency * (Hour div Second));
end;

function TTimeSpan.GetMicroseconds: FG;
begin
  Result := 1000 * Second * FTicks / MainTimer.Frequency;
end;

function TTimeSpan.GetMilliseconds: FG;
begin
  Result := Second * FTicks / MainTimer.Frequency;
end;

function TTimeSpan.GetMinutes: FG;
begin
  Result := FTicks / (MainTimer.Frequency * (Minute div Second));
end;

function TTimeSpan.GetSeconds: FG;
begin
  Result := FTicks / MainTimer.Frequency;
end;

function TTimeSpan.GetTime: TDateTime;
begin
  Result := FTicks / (MainTimer.Frequency * (Day div Second));
end;

procedure TTimeSpan.SetDays(const Value: FG);
begin
  SetTicks(Round(Value * (Day / Second) * MainTimer.Frequency));
end;

procedure TTimeSpan.SetFrequency(const Value: FG);
begin
  SetTicks(Round(MainTimer.Frequency / Value));
end;

procedure TTimeSpan.SetHours(const Value: FG);
begin
  SetTicks(Round(Value * (Hour / Second) * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMicroseconds(const Value: FG);
begin
  SetTicks(Round(MainTimer.Frequency * Value / (1000 * Second)));
end;

procedure TTimeSpan.SetMilliseconds(const Value: FG);
begin
  SetMicroseconds(Second * Value);
end;

procedure TTimeSpan.SetMinutes(const Value: FG);
begin
  SetTicks(Round(Value * (Minute / Second) * MainTimer.Frequency));
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
  SetTicks(Round(Value * (Day / Second) * MainTimer.Frequency));
end;

function TTimeSpan.ToStringInSeconds: string;
begin
  Result := FloatToDecimalString(Seconds, 16, MainTimer.PrecisionDigits);
end;

end.
