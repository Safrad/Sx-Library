// Precise GUI Timer
unit uDTimer;

interface

uses
	uTypes,
  uTimeSpan,
  SysUtils, Classes, Winapi.Messages;

type
	TDTimerEvent = procedure(Sender: TObject) of object;

	TEventStep = (esInterval {in milliseconds}, esIntervalInSeconds, esFrequency, esCPU);

	TDTimer = class(TComponent)
	private
		FActiveOnly: BG;
		FEnabled: BG;
		FSuspended: BG;
		FFrameRate: Integer;
		FInitialized: BG;
		FEventStep: TEventStep;
		FInterval: UG;
		FPreciseInterval: TTimeSpan;
		FNowFrameRate: Integer;
		FOldTime: S8;
		FOldTime2: U8;
		FOnActivate: TNotifyEvent;
		FOnDeactivate: TNotifyEvent;
		FOnTimer: TDTimerEvent;
		FLastLeaveTime: U8;
		TotalLags: UG;
    FIntervalInSeconds: FG;

		function AppProc(var Message: TMessage): Boolean;
		procedure Finalize;
		procedure Initialize;
		procedure Resume;
		procedure SetActiveOnly(Value: BG);
		procedure SetEnabled(Value: BG);
		procedure SetEventStep(Value: TEventStep);
		procedure SetInterval(Value: UG);
		procedure Suspend;
    procedure SetIntervalInSeconds(const Value: FG);
	protected
		procedure DoActivate; virtual;
		procedure DoDeactivate; virtual;
		procedure DoTimer; virtual;
		procedure Loaded; override;
		procedure InitInterval;
	public
		TimerCount: UG;
		Clock,
		ElapsedTime: S8;
		LagCount, LagCount2: UG;
		TimLeave: U8;
		TimSleep, TimWork, CPUUsage, TimWork2, TimSleep2, CPUUsage2: U8;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property FrameRate: Integer read FFrameRate;
		procedure Reset;
		procedure Step;
	published
		property ActiveOnly: BG read FActiveOnly write SetActiveOnly default False;
		property Enabled: BG read FEnabled write SetEnabled default True;
		property Interval: UG read FInterval write SetInterval default 1000;
		property IntervalInSeconds: FG read FIntervalInSeconds write SetIntervalInSeconds;
		property EventStep: TEventStep read FEventStep write SetEventStep default esInterval;
		property OnTimer: TDTimerEvent read FOnTimer write FOnTimer;
		property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
		property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
	end;

implementation

uses
  Vcl.Controls,
  Vcl.Forms,
  Math,

  uDIdleTimer,
  uMainTimer,
  uMath,
  uMsg;

{ TDTimer }

constructor TDTimer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FActiveOnly := False;
	FEnabled := True;
	FInterval := 1000;
  FIntervalInSeconds := 1;
  InitInterval;
	if (not (csDesigning in ComponentState)) then
	begin
		DIdleTimer.AddTimer(Self);
		Application.HookMainWindow(AppProc);
		TimLeave := MainTimer.Value.Ticks;
	end;
end;

destructor TDTimer.Destroy;
begin
  try
    if (not (csDesigning in ComponentState)) then
    begin
      Application.UnHookMainWindow(AppProc);
      DIdleTimer.RemoveTimer(Self);
      Finalize;

      // Free DIdleTimers when contains no Timers
      if DIdleTimer.TimerCount = 0 then
      begin
        FreeDIdleTimer;
      end;
    end;
  finally
  	inherited Destroy;
  end;
end;

function TDTimer.AppProc(var Message: TMessage): Boolean;
begin
	Result := False;
	case Message.Msg of
	CM_ACTIVATE:
	begin
		DoActivate;
		if FInitialized and FActiveOnly then Resume;
	end;
	CM_DEACTIVATE:
	begin
		DoDeactivate;
		if FInitialized and FActiveOnly then Suspend;
	end;
	end;
end;

procedure TDTimer.DoActivate;
begin
	if Assigned(FOnActivate) then FOnActivate(Self);
end;

procedure TDTimer.DoDeactivate;
begin
	if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TDTimer.DoTimer;
begin
	try
		if Assigned(FOnTimer) then
			FOnTimer(Self);
	except
		on E: Exception do
			Fatal(E, Self);
	end;
end;

procedure TDTimer.Finalize;
begin
	if FInitialized then
	begin
		Suspend;
		FInitialized := False;
	end;
end;

procedure TDTimer.Initialize;
begin
	Finalize;
	Reset;

	if FActiveOnly then
	begin
		if Application.Active then
			Resume;
	end
	else
		Resume;
	FInitialized := True;
end;

procedure TDTimer.Loaded;
begin
	inherited Loaded;
	if (not (csDesigning in ComponentState)) and FEnabled then
		Initialize;
end;

procedure TDTimer.Reset;
begin
	FFrameRate := 0;
	FNowFrameRate := 0;
	FOldTime := 0;
	FOldTime2 := 0;
	FLastLeaveTime := MainTimer.Value.Ticks;
	TotalLags := 0;

	TimerCount := 0;
	Clock := 0;
	ElapsedTime := 0;
	LagCount := 0;
	LagCount2 := 0;
	TimLeave := 0;
	TimSleep := 0;
	TimWork := 0;
	CPUUsage := 0;
	TimWork2 := 0;
	TimSleep2 := 0;
	CPUUsage2 := 0;
end;

procedure TDTimer.Suspend;
begin
	FSuspended := True;
end;

procedure TDTimer.Resume;
begin
	FOldTime := MainTimer.Value.Ticks;
	FOldTime2 := FOldTime;
	FSuspended := False;
end;

procedure TDTimer.SetActiveOnly(Value: BG);
begin
	if FActiveOnly <> Value then
	begin
		FActiveOnly := Value;

		if Application.Active and FActiveOnly then
			if FInitialized and FActiveOnly then Suspend;
	end;
end;

procedure TDTimer.SetEnabled(Value: BG);
begin
	if FEnabled <> Value then
	begin
		FEnabled := Value;
		if ComponentState*[csReading, csLoading]=[] then
			if FEnabled then Initialize else Finalize;
	end;
end;

procedure TDTimer.InitInterval;
begin
	case FEventStep of
	esInterval:
    FPreciseInterval.Milliseconds := FInterval;
	esIntervalInSeconds:
    FPreciseInterval.SecondsAsF := FIntervalInSeconds;
	esFrequency:
    FPreciseInterval.Frequency := FInterval;
	else
    FPreciseInterval.Ticks := 0;
	end;
end;

procedure TDTimer.SetEventStep(Value: TEventStep);
begin
	if FEventStep <> Value then
	begin
		FEventStep := Value;
		InitInterval;
	end;
end;

procedure TDTimer.SetInterval(Value: UG);
begin
	if FInterval <> Value then
	begin
		FInterval := Max(Value, 1);
		InitInterval;
	end;
end;

procedure TDTimer.SetIntervalInSeconds(const Value: FG);
begin
  if FIntervalInSeconds <> Value then
  begin
    FIntervalInSeconds := Value;
    InitInterval;
  end;
end;

procedure TDTimer.Step;
var
	NowTime: S8; // Actual PerformanceCounter Value
	NTime: S8;
	t: S8;
begin
	if (FEnabled) and ((FSuspended = False) or (FActiveOnly = False)) then
	begin
		NowTime := MainTimer.Value.Ticks;
		TimSleep := NowTime - TimLeave;
		ElapsedTime := NowTime - FOldTime;
//				if MinTime > FInterval12 then MinTime := FInterval12;
		if FEventStep = esCPU then
		begin
			if FInterval = 0 then
        FInterval := 1;
			FPreciseInterval.Ticks := Max(RoundDivU8(100 * TimWork, FInterval), 1);
		end;
		if (ElapsedTime > 0) and (ElapsedTime + RoundDivS8(MainTimer.Frequency * LagTime, 2 * Second) >= FPreciseInterval.Ticks) then
		begin
			// Frame Rate
			Inc(FNowFrameRate);
			t := NowTime - FOldTime2;
			if t >= MainTimer.Frequency then
			begin
				if t = 0 then
					FFrameRate := High(FFrameRate)
				else
					FFrameRate := RoundDivS8(FNowFrameRate * MainTimer.Frequency * Second, t);
				FNowFrameRate := 0;
{						LagCount := ElapsedTime div MainTimer.Frequency;
				if LagCount < 1 then LagCount := 1;
				Inc(FOldTime2, LagCount * MainTimer.Frequency);}
				FOldTime2 := NowTime;
			end;

			// Lags
			LagCount := ElapsedTime div FPreciseInterval.Ticks;
			if LagCount < 1 then LagCount := 1;
			if LagCount > 1 then Inc(TotalLags, LagCount - 1);
			t := U8(LagCount) * FPreciseInterval.Ticks;
			Inc(FOldTime, t);

			Inc(Clock, ElapsedTime + t);

			ElapsedTime := NowTime - FLastLeaveTime;
			FLastLeaveTime := NowTime;

{			esCPU: SetPriorityClass(GetCurrentProcess, HIGH);
			else SetPriorityClass(GetCurrentProcess, NORMAL); TODO : }

			DoTimer;

			NTime := MainTimer.Value.Ticks;
			TimLeave := NTime;
			TimWork := TimLeave - NowTime;
			NowTime := NTime;

			t := TimWork + TimSleep;
			if t > 0 then
			begin
				CPUUsage := RoundDivS8(1000 * TimWork, t);
			end;
			Inc(TimWork2, TimWork);
			Inc(TimSleep2, TimSleep);
			t := TimWork2 + TimSleep2;
			if t >= MainTimer.Frequency then
			begin
				CPUUsage2 := RoundDivS8(1000 * TimWork2, t);
				LagCount2 := TotalLags;
				TotalLags := 0;
				TimWork2 := 0;
				TimSleep2 := 0;
			end;

			Inc(TimerCount);
		end;
    t := S8(FPreciseInterval.Ticks + FOldTime) - NowTime;
		if DIdleTimer.MinTime > t then
      DIdleTimer.MinTime := t;
	end;
end;

end.

