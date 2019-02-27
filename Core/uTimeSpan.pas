unit uTimeSpan;

interface

uses
  uTypes;

type
  TTimeSpan = record
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

    // The total elapsed time measured by the current instance, in days.
    // The same as property Time but better precision in 32 bit.
    property Days: FG read GetDays write SetDays;

    // The total elapsed time measured by the current instance, in days.
    // The same as property Days but lower precision in 32 bit.
    property Time: TDateTime read GetTime write SetTime;

    property Frequency: FG read GetFrequency write SetFrequency;

    function ToStringInSeconds: string;

    // -- Comparison operators --

    /// <summary>Returns True if Left is mathematically less than or equal to Right.</summary>
    /// <param name="Left">The first operand</param>
    /// <param name="Right">The second operand</param>
    /// <returns><code>Result := Left &lt;= Right;</code></returns>
    class operator LessThanOrEqual(const Left, Right: TTimeSpan): Boolean;

    /// <summary>Returns True if Left is mathematically less than Right.</summary>
    /// <param name="Left">The first operand</param>
    /// <param name="Right">The second operand</param>
    /// <returns><code>Result := Left &lt; Right;</code></returns>
    class operator LessThan(const left, Right: TTimeSpan): Boolean;

    /// <summary>Returns True if Left is mathematically greater than or equal to Right.</summary>
    /// <param name="Left">The first operand</param>
    /// <param name="Right">The second operand</param>
    /// <returns><code>Result := Left &gt;= Right;</code></returns>
    class operator GreaterThanOrEqual(const Left, Right: TTimeSpan): Boolean;

    /// <summary>Returns True if Left is mathematically greater than Right.</summary>
    /// <param name="Left">The first operand</param>
    /// <param name="Right">The second operand</param>
    /// <returns><code>Result := Left &gt; Right;</code></returns>
    class operator GreaterThan(const Left, Right: TTimeSpan): Boolean;

    /// <summary>Returns True if Left is mathematically equal to Right.</summary>
    /// <param name="Left">The first operand</param>
    /// <param name="Right">The second operand</param>
    /// <returns><code>Result := Left = Right;</code></returns>
    class operator Equal(const left, Right: TTimeSpan): Boolean;

    /// <summary>Returns True if Left is mathematically not equal to Right.</summary>
    /// <param name="Left">The first operand</param>
    /// <param name="Right">The second operand</param>
    /// <returns><code>Result := Left &lt;&gt; Right;</code></returns>
    class operator NotEqual(const Left, Right: TTimeSpan): Boolean;
  end;

implementation

uses
  uTimePrefix,
  uMainTimer,
  uOutputFormat;

{ TTimeSpan }

class operator TTimeSpan.Equal(const Left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks = Right.Ticks;
end;

function TTimeSpan.GetDays: FG;
begin
  Result := FTicks / (TTimePrefix.Day * MainTimer.Frequency);
end;

function TTimeSpan.GetFrequency: FG;
begin
  Result := MainTimer.Frequency / FTicks;
end;

function TTimeSpan.GetHours: FG;
begin
  Result := FTicks / (TTimePrefix.Hour * MainTimer.Frequency);
end;

function TTimeSpan.GetMicroseconds: FG;
begin
  Result :=  FTicks / (TTimePrefix.MicroSecond * MainTimer.Frequency);
end;

function TTimeSpan.GetMilliseconds: FG;
begin
  Result := FTicks / (TTimePrefix.MilliSecond * MainTimer.Frequency);
end;

function TTimeSpan.GetMinutes: FG;
begin
  Result := FTicks / (TTimePrefix.Minute * MainTimer.Frequency);
end;

function TTimeSpan.GetSeconds: FG;
begin
  Result := FTicks / (TTimePrefix.Second * MainTimer.Frequency);
end;

function TTimeSpan.GetTime: TDateTime;
begin
  Result := FTicks / (TTimePrefix.Day * MainTimer.Frequency);
end;

class operator TTimeSpan.GreaterThan(const Left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks > Right.Ticks;
end;

class operator TTimeSpan.GreaterThanOrEqual(const Left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks >= Right.Ticks;
end;

class operator TTimeSpan.LessThan(const left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks < Right.Ticks;
end;

class operator TTimeSpan.LessThanOrEqual(const Left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks <= Right.Ticks;
end;

class operator TTimeSpan.NotEqual(const Left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks <> Right.Ticks;
end;

procedure TTimeSpan.SetDays(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.Day * MainTimer.Frequency));
end;

procedure TTimeSpan.SetFrequency(const Value: FG);
begin
  SetTicks(Round(MainTimer.Frequency / Value));
end;

procedure TTimeSpan.SetHours(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.Hour * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMicroseconds(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.MicroSecond * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMilliseconds(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.MilliSecond * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMinutes(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.Minute * MainTimer.Frequency));
end;

procedure TTimeSpan.SetSeconds(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.Second * MainTimer.Frequency));
end;

procedure TTimeSpan.SetTicks(const Value: U8);
begin
  FTicks := Value;
end;

procedure TTimeSpan.SetTime(const Value: TDateTime);
begin
  SetTicks(Round(Value * TTimePrefix.Day * MainTimer.Frequency));
end;

function TTimeSpan.ToStringInSeconds: string;
begin
  Result := FloatToDecimalString(Seconds, 16, MainTimer.PrecisionDigits);
end;

end.
