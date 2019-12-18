unit uMainTimer;

interface

uses
  uTypes,
  uTimeSpan;

type
	TMeasureType = (
    mtNone,

    // [1000 Hz], (low precision, updated every 10-16 ms)
    // Based on RTC
    mtGetTickCount32,

    // [1000 Hz], (low precision, updated every 10-16 ms)
    // Based on RTC, 64 bit version of GetTickCount since Windows Vista
    mtGetTickCount64,

    // new systems: HPET (High Precision Event Timer) [~24,000,000 Hz]
    // old systems: PIT or RTC [~3,579,545 Hz]
    mtPerformanceCounter,

    // CPU instruction RDTSC
    mtTSC
  );

{$IF defined(MSWINDOWS)}
  TGetTickCount64 = function: U8; stdcall;
{$ENDIF}

  TMainTimer = class
  private
    FValue: TTimeSpan;
    FMeasureType: TMeasureType;
    FFrequency: U8;
    FPrecisionDigits: U1;
{$IF defined(MSWINDOWS)}
    FGetTickCount64: TGetTickCount64;
{$ENDIF}
    function GetTickValue: U8;
    function GetPerformanceCounterFrequecy: U8;
    procedure SetMeasureType(const Value: TMeasureType);
    function GetValue: TTimeSpan;
  public
    procedure Delay(const ATimeSpan: TTimeSpan);
    procedure PreciseSleep(const ATimeSpan: TTimeSpan);

    function IntervalFrom(const AStartTime: U8): U8; overload;
    function IntervalFrom(const AStartTime: TTimeSpan): TTimeSpan; overload;

    property Value: TTimeSpan read GetValue;
    property Frequency: U8 read FFrequency;
    property MeasureType: TMeasureType read FMeasureType write SetMeasureType;
    property PrecisionDigits: U1 read FPrecisionDigits;
  end;

var
  MainTimer: TMainTimer;

implementation

uses
{$IF defined(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  Math,
  Diagnostics,
  SysUtils,
  uMath;

{$IF defined(MSWINDOWS)}
function GetCPUCounter: TU8; register;
asm
{$ifdef CPUX64}
  push rcx
	mov ecx, 10h
  rdtsc
  pop rcx
  mov [Result.D0], eax
  mov [Result.D1], edx
{$else}
	push Result
	mov ecx, 10h
	dw 310fh // RDTSC 10 clocks
	pop ecx
	mov [ecx], eax
	mov [ecx + 4], edx
{$endif}
end;
{$ENDIF}

{ TMainTimer }

/// <summary>
/// CPU usage is 100% if used in loop
/// </summary>
procedure TMainTimer.Delay(const ATimeSpan: TTimeSpan);
var
	StartTickCount: U8;
begin
	StartTickCount := Value.Ticks;
  // busy-wait loop (spin-wait loop)
	while IntervalFrom(StartTickCount) < ATimeSpan.Ticks do
  begin
    Pause;
  end;
end;

function TMainTimer.GetPerformanceCounterFrequecy: U8;
{$IF defined(MSWINDOWS)}
var
  QueriedFrequency: TLargeInteger;
begin
  Result := 0;
  if QueryPerformanceFrequency(QueriedFrequency) then
    Result := QueriedFrequency;
{$ELSE}
begin
  Result := Diagnostics.TStopwatch.Frequency;
{$ENDIF}
end;

function TMainTimer.GetTickValue: U8;
begin
	case FMeasureType of
  {$IF defined(MSWINDOWS)}
	mtGetTickCount32: Result := GetTickCount;
  mtGetTickCount64: Result := FGetTickCount64;
	mtPerformanceCounter: QueryPerformanceCounter(TLargeInteger(Result));
	mtTSC: Result := GetCPUCounter.A;
  {$ELSE}
	mtGetTickCount32: Result := RoundDiv(1000 * Diagnostics.TStopwatch.GetTimeStamp, Diagnostics.TStopwatch.Frequency);
  mtGetTickCount64: Result := RoundDiv(1000 * Diagnostics.TStopwatch.GetTimeStamp, Diagnostics.TStopwatch.Frequency);
	mtPerformanceCounter: Result := Diagnostics.TStopwatch.GetTimeStamp;
	mtTSC: Result := 0;
  {$ENDIF}
  else
    raise EInvalidOpException.Create('Measure type is not initialized.');
	end;
end;

function TMainTimer.GetValue: TTimeSpan;
begin
  FValue.Ticks := GetTickValue;
  Result := FValue;
end;

function TMainTimer.IntervalFrom(const AStartTime: U8): U8;
begin
  Result := TimeDifference(GetTickValue, AStartTime);
end;

function TMainTimer.IntervalFrom(const AStartTime: TTimeSpan): TTimeSpan;
begin
  Result.Ticks := TimeDifference(GetTickValue, AStartTime.Ticks);
end;

/// <summary>
/// CPU usage if used in loop
/// Sleep Time [ms] CPU [%]
/// 0  100
/// 1  100
/// 2   25
/// 3   16
/// 4   12
/// 5    9
/// 10   5
/// </summary>
procedure TMainTimer.PreciseSleep(const ATimeSpan: TTimeSpan);
const
  MaximalSleepTime = 250; // ms

  // Thread swap time
  // Unix and new Windows: 1 ms
  // Old Windows: 15 ms!
  MaximalAddedSleepTimeInMs = 1; // TODO: Detect old Windows 15 ms
var
	StartTickCount: U8;
  RemainTimeInMs: FG;
begin
  StartTickCount := Value.Ticks;

  // sleep-wait loop
  while True do
  begin
    RemainTimeInMs := Second * (S8(ATimeSpan.Ticks) - S8(IntervalFrom(StartTickCount))) / MainTimer.Frequency;
    if RemainTimeInMs >= MaximalAddedSleepTimeInMs then
    begin
      // Method Sleep sleeps a bit longer then specified
      Sleep(Min(Trunc(RemainTimeInMs - MaximalAddedSleepTimeInMs), MaximalSleepTime));
    end
    else
      Break;
  end;

  // busy-wait loop (spin-wait loop)
  while IntervalFrom(StartTickCount) < ATimeSpan.Ticks do
  begin
    Pause;
  end;
end;

procedure TMainTimer.SetMeasureType(const Value: TMeasureType);
begin
  if FMeasureType <> Value then
  begin
    case Value of
    mtGetTickCount32:
    begin
      FFrequency := 1000;
      FPrecisionDigits := 3;
    end;
    mtGetTickCount64:
    begin
      {$IF defined(MSWINDOWS)}
      FGetTickCount64 := GetProcAddress(GetModuleHandle(kernel32), 'GetTickCount64');
      if not Assigned(FGetTickCount64) then
      begin
        SetMeasureType(mtGetTickCount32); // Downgrade
        Exit;
      end;
      {$ENDIF}
      FFrequency := 1000;
      FPrecisionDigits := 3;
    end;
    mtPerformanceCounter:
    begin
      FFrequency := GetPerformanceCounterFrequecy;
      if FFrequency >= 1000 then
      begin
        FPrecisionDigits := CountDigits(FFrequency - 1);
      end
      else
      begin
        SetMeasureType(mtGetTickCount64); // Downgrade
        Exit;
      end;
    end;
    mtTSC:
    begin
      FFrequency := 0; // N/A
      FPrecisionDigits := 0;
    end;
    end;
    FMeasureType := Value;
  end;
end;

initialization
{$IFNDEF NoInitialization}
  MainTimer := TMainTimer.Create;
  MainTimer.MeasureType := mtPerformanceCounter;
  TTimeSpan.TicksPerSecond := MainTimer.Frequency;
  TTimeSpan.PrecisionDigits := MainTimer.PrecisionDigits;
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
  FreeAndNil(MainTimer);
{$ENDIF NoFinalization}
end.
