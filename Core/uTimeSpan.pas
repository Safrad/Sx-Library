unit uTimeSpan;

interface

uses
  uTypes,
  Velthuis.BigDecimals;

type
  TTimeSpan = record
  private
    FTicks: U8;
    procedure SetMicrosecondsAsF(const Value: FG);
    procedure SetMillisecondsAsF(const Value: FG);
    procedure SetTicks(const Value: U8);
    procedure SetTime(const Value: TDateTime);
    function GetMicrosecondsAsF: FG;
    function GetMillisecondsAsF: FG;
    function GetTime: TDateTime;
    procedure SetFrequency(const Value: FG);
    function GetFrequency: FG;
    function GetSecondsAsF: FG;
    procedure SetSeconds(const Value: FG);
    function GetDaysAsF: FG;
    function GetHoursAsF: FG;
    function GetMinutesAsF: FG;
    procedure SetDaysAsF(const Value: FG);
    procedure SetHoursAsF(const Value: FG);
    procedure SetMinutesAsF(const Value: FG);
    function GetMillisecondsAsI: U8;
    procedure SetMillisecondsAsI(const Value: U8);
    function GetDaysAsI: U8;
    function GetHoursAsI: U8;
    function GetMicrosecondsAsI: U8;
    function GetMinutesAsI: U8;
    function GetSecondsAsI: U8;
    procedure SetDaysAsI(const Value: U8);
    procedure SetHoursAsI(const Value: U8);
    procedure SetMicrosecondsAsI(const Value: U8);
    procedure SetMinutesAsI(const Value: U8);
    procedure SetSecondsAsI(const Value: U8);
    function GetMicrosecondsAsBD: BigDecimal;
    procedure SetMicrosecondsAsBD(const Value: BigDecimal);
    function GetMillisecondsAsBD: BigDecimal;
    procedure SetMillisecondsAsBD(const Value: BigDecimal);
    procedure SetDaysAsBD(const Value: BigDecimal);
    procedure SetHoursAsBD(const Value: BigDecimal);
    procedure SetMinutesAsBD(const Value: BigDecimal);
    procedure SetSecondsAsBD(const Value: BigDecimal);
    function GetSecondsAsBD: BigDecimal;
    function GetDaysAsBD: BigDecimal;
    function GetHoursAsBD: BigDecimal;
    function GetMinutesAsBD: BigDecimal;
  public
    // The total elapsed time measured by the current instance, in timer ticks.
    property Ticks: U8 read FTicks write SetTicks;

    // The total elapsed time measured by the current instance, in microseconds.
    property Microseconds: U8 read GetMicrosecondsAsI write SetMicrosecondsAsI;
    property MicrosecondsAsF: FG read GetMicrosecondsAsF write SetMicrosecondsAsF;
    property MicrosecondsAsBD: BigDecimal read GetMicrosecondsAsBD write SetMicrosecondsAsBD;

    // The total elapsed time measured by the current instance, in milliseconds.
    property Milliseconds: U8 read GetMillisecondsAsI write SetMillisecondsAsI;
    property MillisecondsAsF: FG read GetMillisecondsAsF write SetMillisecondsAsF;
    property MillisecondsAsBD: BigDecimal read GetMillisecondsAsBD write SetMillisecondsAsBD;

    // The total elapsed time measured by the current instance, in seconds.
    property Seconds: U8 read GetSecondsAsI write SetSecondsAsI;
    property SecondsAsF: FG read GetSecondsAsF write SetSeconds;
    property SecondsAsBD: BigDecimal read GetSecondsAsBD write SetSecondsAsBD;

    // The total elapsed time measured by the current instance, in minutes.
    property Minutes: U8 read GetMinutesAsI write SetMinutesAsI;
    property MinutesAsF: FG read GetMinutesAsF write SetMinutesAsF;
    property MinutesAsBD: BigDecimal read GetMinutesAsBD write SetMinutesAsBD;

    // The total elapsed time measured by the current instance, in hours.
    property Hours: U8 read GetHoursAsI write SetHoursAsI;
    property HoursAsF: FG read GetHoursAsF write SetHoursAsF;
    property HoursAsBD: BigDecimal read GetHoursAsBD write SetHoursAsBD;

    // The total elapsed time measured by the current instance, in days.
    // DaysAsF is the same as property Time.
    property Days: U8 read GetDaysAsI write SetDaysAsI;
    property DaysAsF: FG read GetDaysAsF write SetDaysAsF;
    property DaysAsBD: BigDecimal read GetDaysAsBD write SetDaysAsBD;

    // The total elapsed time measured by the current instance, in days.
    // The same as property DaysAsF.
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

    function Difference(const AValue: TTimeSpan): TTimeSpan;
  end;

implementation

uses
  uTimePrefix,
  uMath,
  uMainTimer,
  uOutputFormat,
  uMetricPrefix;

{ TTimeSpan }

function TTimeSpan.Difference(const AValue: TTimeSpan): TTimeSpan;
begin
  Result.Ticks := TimeDifference(FTicks, AValue.Ticks);
end;

class operator TTimeSpan.Equal(const Left, Right: TTimeSpan): Boolean;
begin
  Result := Left.Ticks = Right.Ticks;
end;

function TTimeSpan.GetDaysAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, BigDecimal(TTimePrefix.DayAsF) * BigDecimal(MainTimer.Frequency));
end;

function TTimeSpan.GetDaysAsF: FG;
begin
  Result := FTicks / (TTimePrefix.DayAsF * MainTimer.Frequency);
end;

function TTimeSpan.GetDaysAsI: U8;
begin
  Result := RoundDivU8(FTicks, TTimePrefix.DayAsI * MainTimer.Frequency);
end;

function TTimeSpan.GetFrequency: FG;
begin
  Result := MainTimer.Frequency / FTicks;
end;

function TTimeSpan.GetHoursAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, BigDecimal(TTimePrefix.HourAsI) * BigDecimal(MainTimer.Frequency));
end;

function TTimeSpan.GetHoursAsF: FG;
begin
  Result := FTicks / (TTimePrefix.HourAsF * MainTimer.Frequency);
end;

function TTimeSpan.GetHoursAsI: U8;
begin
  Result := RoundDivU8(FTicks, TTimePrefix.HourAsI * MainTimer.Frequency);
end;

function TTimeSpan.GetMicrosecondsAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(BigDecimal(FTicks) * TMetricPrefix.MicroDivisorAsI, MainTimer.Frequency);
end;

function TTimeSpan.GetMicrosecondsAsF: FG;
begin
  Result :=  FTicks / (TMetricPrefix.MicroAsF * MainTimer.Frequency);
end;

function TTimeSpan.GetMicrosecondsAsI: U8;
begin
  Result :=  RoundDiv(FTicks * TMetricPrefix.MicroDivisorAsI, MainTimer.Frequency);
end;

function TTimeSpan.GetMillisecondsAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(BigDecimal(FTicks) * TMetricPrefix.MilliDivisorAsI, MainTimer.Frequency);
end;

function TTimeSpan.GetMillisecondsAsF: FG;
begin
  Result := TMetricPrefix.MilliDivisorAsF * FTicks / MainTimer.Frequency;
end;

function TTimeSpan.GetMillisecondsAsI: U8;
begin
  Result := RoundDivU8(FTicks * TMetricPrefix.MilliDivisorAsI, MainTimer.Frequency);
end;

function TTimeSpan.GetMinutesAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, BigDecimal(TTimePrefix.MinuteAsI) * BigDecimal(MainTimer.Frequency));
end;

function TTimeSpan.GetMinutesAsF: FG;
begin
  Result := FTicks / (TTimePrefix.MinuteAsF * MainTimer.Frequency);
end;

function TTimeSpan.GetMinutesAsI: U8;
begin
  Result := RoundDivU8(FTicks, TTimePrefix.MinuteAsI * MainTimer.Frequency);
end;

function TTimeSpan.GetSecondsAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, MainTimer.Frequency);
end;

function TTimeSpan.GetSecondsAsF: FG;
begin
  Result := FTicks / MainTimer.Frequency;
end;

function TTimeSpan.GetSecondsAsI: U8;
begin
  Result := RoundDivU8(FTicks, MainTimer.Frequency);
end;

function TTimeSpan.GetTime: TDateTime;
begin
  Result := FTicks / (TTimePrefix.DayAsF * MainTimer.Frequency);
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

procedure TTimeSpan.SetDaysAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * TTimePrefix.DayAsI * MainTimer.Frequency));
end;

procedure TTimeSpan.SetDaysAsF(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.DayAsF * MainTimer.Frequency));
end;

procedure TTimeSpan.SetDaysAsI(const Value: U8);
begin
  SetTicks(Value * TTimePrefix.DayAsI * MainTimer.Frequency);
end;

procedure TTimeSpan.SetFrequency(const Value: FG);
begin
  SetTicks(Round(MainTimer.Frequency / Value));
end;

procedure TTimeSpan.SetHoursAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * TTimePrefix.HourAsI * MainTimer.Frequency));
end;

procedure TTimeSpan.SetHoursAsF(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.HourAsF * MainTimer.Frequency));
end;

procedure TTimeSpan.SetHoursAsI(const Value: U8);
begin
  SetTicks(Value * TTimePrefix.HourAsI * MainTimer.Frequency);
end;

procedure TTimeSpan.SetMicrosecondsAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(BigDecimal.Divide(Value * MainTimer.Frequency, TMetricPrefix.MicroDivisorAsI)));
end;

procedure TTimeSpan.SetMicrosecondsAsF(const Value: FG);
begin
  SetTicks(Round(Value * TMetricPrefix.MicroAsF * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMicrosecondsAsI(const Value: U8);
begin
  SetTicks(RoundDivU8(Value * MainTimer.Frequency, TMetricPrefix.MicroDivisorAsI));
end;

procedure TTimeSpan.SetMillisecondsAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(BigDecimal.Divide(Value * MainTimer.Frequency, TMetricPrefix.MilliDivisorAsI)));
end;

procedure TTimeSpan.SetMillisecondsAsF(const Value: FG);
begin
  SetTicks(Round(Value * TMetricPrefix.MilliAsF * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMillisecondsAsI(const Value: U8);
begin
  SetTicks(RoundDivU8(Value * MainTimer.Frequency, TMetricPrefix.MilliDivisorAsI));
end;

procedure TTimeSpan.SetMinutesAsF(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.MinuteAsF * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMinutesAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * TTimePrefix.MinuteAsI * MainTimer.Frequency));
end;

procedure TTimeSpan.SetMinutesAsI(const Value: U8);
begin
  SetTicks(Value * TTimePrefix.MinuteAsI * MainTimer.Frequency);
end;

procedure TTimeSpan.SetSeconds(const Value: FG);
begin
  SetTicks(Round(Value * MainTimer.Frequency));
end;

procedure TTimeSpan.SetSecondsAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * MainTimer.Frequency));
end;

procedure TTimeSpan.SetSecondsAsI(const Value: U8);
begin
  SetTicks(Value * MainTimer.Frequency);
end;

procedure TTimeSpan.SetTicks(const Value: U8);
begin
  FTicks := Value;
end;

procedure TTimeSpan.SetTime(const Value: TDateTime);
begin
  SetTicks(Round(Value * TTimePrefix.DayAsF * MainTimer.Frequency));
end;

function TTimeSpan.ToStringInSeconds: string;
begin
  Result := FloatToDecimalString(SecondsAsF, 16, MainTimer.PrecisionDigits);
end;

end.
