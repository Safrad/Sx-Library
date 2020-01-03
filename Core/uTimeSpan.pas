unit uTimeSpan;

interface

uses
  uTypes,
  Velthuis.BigDecimals;

type
  TTimeSpan = record
  private
    FTicks: U8;
    class var FTicksPerSecond: U8;
    class var FPrecisionDigits: U1;
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
    class procedure SetTicksPerSecond(const Value: U8); static;
    class procedure SetPrecisionDigits(const Value: U1); static;
  public
    // The total elapsed time measured by the current instance, in timer ticks.
    property Ticks: U8 read FTicks write SetTicks;
    class property TicksPerSecond: U8 read FTicksPerSecond write SetTicksPerSecond;
    class property PrecisionDigits: U1 read FPrecisionDigits write SetPrecisionDigits;

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

    /// <summary>Returns difference between actual time span and time span in AValue</summary>
    /// <param name="AValue">The first operand</param>
    /// <returns><code>Result := Self - AValue</code></returns>
    function Difference(const AValue: TTimeSpan): TTimeSpan;
  end;

implementation

uses
  uTimePrefix,
  uMath,
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
  Result := BigDecimal.Divide(FTicks, BigDecimal(TTimePrefix.DayAsF) * BigDecimal(FTicksPerSecond));
end;

function TTimeSpan.GetDaysAsF: FG;
begin
  Result := FTicks / (TTimePrefix.DayAsF * FTicksPerSecond);
end;

function TTimeSpan.GetDaysAsI: U8;
begin
  Result := RoundDivU8(FTicks, TTimePrefix.DayAsI * FTicksPerSecond);
end;

function TTimeSpan.GetFrequency: FG;
begin
  Result := FTicksPerSecond / FTicks;
end;

function TTimeSpan.GetHoursAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, BigDecimal(TTimePrefix.HourAsI) * BigDecimal(FTicksPerSecond));
end;

function TTimeSpan.GetHoursAsF: FG;
begin
  Result := FTicks / (TTimePrefix.HourAsF * FTicksPerSecond);
end;

function TTimeSpan.GetHoursAsI: U8;
begin
  Result := RoundDivU8(FTicks, TTimePrefix.HourAsI * FTicksPerSecond);
end;

function TTimeSpan.GetMicrosecondsAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(BigDecimal(FTicks) * TMetricPrefix.MicroDivisorAsI, FTicksPerSecond);
end;

function TTimeSpan.GetMicrosecondsAsF: FG;
begin
  Result :=  FTicks / (TMetricPrefix.MicroAsF * FTicksPerSecond);
end;

function TTimeSpan.GetMicrosecondsAsI: U8;
begin
  Result :=  RoundDivU8(FTicks * TMetricPrefix.MicroDivisorAsI, FTicksPerSecond);
end;

function TTimeSpan.GetMillisecondsAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(BigDecimal(FTicks) * TMetricPrefix.MilliDivisorAsI, FTicksPerSecond);
end;

function TTimeSpan.GetMillisecondsAsF: FG;
begin
  Result := TMetricPrefix.MilliDivisorAsF * FTicks / FTicksPerSecond;
end;

function TTimeSpan.GetMillisecondsAsI: U8;
begin
  Result := RoundDivU8(FTicks * TMetricPrefix.MilliDivisorAsI, FTicksPerSecond);
end;

function TTimeSpan.GetMinutesAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, BigDecimal(TTimePrefix.MinuteAsI) * BigDecimal(FTicksPerSecond));
end;

function TTimeSpan.GetMinutesAsF: FG;
begin
  Result := FTicks / (TTimePrefix.MinuteAsF * FTicksPerSecond);
end;

function TTimeSpan.GetMinutesAsI: U8;
begin
  Result := RoundDivU8(FTicks, TTimePrefix.MinuteAsI * FTicksPerSecond);
end;

function TTimeSpan.GetSecondsAsBD: BigDecimal;
begin
  Result := BigDecimal.Divide(FTicks, FTicksPerSecond);
end;

function TTimeSpan.GetSecondsAsF: FG;
begin
  Result := FTicks / FTicksPerSecond;
end;

function TTimeSpan.GetSecondsAsI: U8;
begin
  Result := RoundDivU8(FTicks, FTicksPerSecond);
end;

function TTimeSpan.GetTime: TDateTime;
begin
  Result := FTicks / (TTimePrefix.DayAsF * FTicksPerSecond);
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
  SetTicks(BigDecimal.Round(Value * TTimePrefix.DayAsI * FTicksPerSecond));
end;

procedure TTimeSpan.SetDaysAsF(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.DayAsF * FTicksPerSecond));
end;

procedure TTimeSpan.SetDaysAsI(const Value: U8);
begin
  SetTicks(Value * TTimePrefix.DayAsI * FTicksPerSecond);
end;

procedure TTimeSpan.SetFrequency(const Value: FG);
begin
  SetTicks(Round(FTicksPerSecond / Value));
end;

procedure TTimeSpan.SetHoursAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * TTimePrefix.HourAsI * FTicksPerSecond));
end;

procedure TTimeSpan.SetHoursAsF(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.HourAsF * FTicksPerSecond));
end;

procedure TTimeSpan.SetHoursAsI(const Value: U8);
begin
  SetTicks(Value * TTimePrefix.HourAsI * FTicksPerSecond);
end;

procedure TTimeSpan.SetMicrosecondsAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(BigDecimal.Divide(Value * FTicksPerSecond, TMetricPrefix.MicroDivisorAsI)));
end;

procedure TTimeSpan.SetMicrosecondsAsF(const Value: FG);
begin
  SetTicks(Round(Value * TMetricPrefix.MicroAsF * FTicksPerSecond));
end;

procedure TTimeSpan.SetMicrosecondsAsI(const Value: U8);
begin
  SetTicks(RoundDivU8(Value * FTicksPerSecond, TMetricPrefix.MicroDivisorAsI));
end;

procedure TTimeSpan.SetMillisecondsAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(BigDecimal.Divide(Value * FTicksPerSecond, TMetricPrefix.MilliDivisorAsI)));
end;

procedure TTimeSpan.SetMillisecondsAsF(const Value: FG);
begin
  SetTicks(Round(Value * TMetricPrefix.MilliAsF * FTicksPerSecond));
end;

procedure TTimeSpan.SetMillisecondsAsI(const Value: U8);
begin
  SetTicks(RoundDivU8(Value * FTicksPerSecond, TMetricPrefix.MilliDivisorAsI));
end;

procedure TTimeSpan.SetMinutesAsF(const Value: FG);
begin
  SetTicks(Round(Value * TTimePrefix.MinuteAsF * FTicksPerSecond));
end;

procedure TTimeSpan.SetMinutesAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * TTimePrefix.MinuteAsI * FTicksPerSecond));
end;

procedure TTimeSpan.SetMinutesAsI(const Value: U8);
begin
  SetTicks(Value * TTimePrefix.MinuteAsI * FTicksPerSecond);
end;

class procedure TTimeSpan.SetPrecisionDigits(const Value: U1);
begin
  FPrecisionDigits := Value;
end;

procedure TTimeSpan.SetSeconds(const Value: FG);
begin
  SetTicks(Round(Value * FTicksPerSecond));
end;

procedure TTimeSpan.SetSecondsAsBD(const Value: BigDecimal);
begin
  SetTicks(BigDecimal.Round(Value * FTicksPerSecond));
end;

procedure TTimeSpan.SetSecondsAsI(const Value: U8);
begin
  SetTicks(Value * FTicksPerSecond);
end;

class procedure TTimeSpan.SetTicksPerSecond(const Value: U8);
begin
  FTicksPerSecond := Value;
end;

procedure TTimeSpan.SetTicks(const Value: U8);
begin
  FTicks := Value;
end;

procedure TTimeSpan.SetTime(const Value: TDateTime);
begin
  SetTicks(Round(Value * TTimePrefix.DayAsF * FTicksPerSecond));
end;

function TTimeSpan.ToStringInSeconds: string;
begin
  Result := FloatToDecimalString(SecondsAsF, 16, FPrecisionDigits);
end;

end.

