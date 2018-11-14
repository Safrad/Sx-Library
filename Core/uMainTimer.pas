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

  TGetTickCount64 = function: U8; stdcall;

  TMainTimer = class
  private
    FValue: TTimeSpan;
    FMeasureType: TMeasureType;
    FFrequency: U8;
    FPrecisionDigits: U1;
    FGetTickCount64: TGetTickCount64;
    function GetTickValue: U8;
    procedure SetMeasureType(const Value: TMeasureType);
    function GetValue: TTimeSpan;
  public
    function IntervalFrom(const AStartTime: U8): U8;

    property Value: TTimeSpan read GetValue;
    property Frequency: U8 read FFrequency;
    property MeasureType: TMeasureType read FMeasureType write SetMeasureType;
    property PrecisionDigits: U1 read FPrecisionDigits;
  end;

var
  MainTimer: TMainTimer;

implementation

uses
  Windows,
  SysUtils,
  uMath;

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

{ TMainTimer }

function TMainTimer.GetTickValue: U8;
begin
	case FMeasureType of
	mtGetTickCount32: Result := Windows.GetTickCount;
  mtGetTickCount64: Result := FGetTickCount64;
	mtPerformanceCounter: QueryPerformanceCounter(TLargeInteger(Result));
	mtTSC: Result := GetCPUCounter.A;
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

procedure TMainTimer.SetMeasureType(const Value: TMeasureType);
var
  QueriedFrequency: TLargeInteger;
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
      FGetTickCount64 := GetProcAddress(GetModuleHandle(kernel32), 'GetTickCount64');
      if not Assigned(FGetTickCount64) then
      begin
        SetMeasureType(mtGetTickCount32);
        Exit;
      end;
      FFrequency := 1000;
      FPrecisionDigits := 3;
    end;
    mtPerformanceCounter:
    begin
      QueriedFrequency := 0;
      if QueryPerformanceFrequency(QueriedFrequency) then
      begin
        if QueriedFrequency >= 1000 then
        begin
          FFrequency := QueriedFrequency;
          FPrecisionDigits := CountDigits(QueriedFrequency - 1);
        end
        else
        begin
          SetMeasureType(mtGetTickCount64);
          Exit;
        end;
      end
      else
      begin
        SetMeasureType(mtGetTickCount64);
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
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
  FreeAndNil(MainTimer);
{$ENDIF NoFinalization}
end.
