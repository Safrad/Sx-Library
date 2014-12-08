unit uDTimer;

interface

uses
	uTypes, uMath,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
	TDTimerEvent = procedure(Sender: TObject) of object;

	TEventStep = (esInterval, esFrequency, esCPU);

	TDTimer = class(TComponent)
	private
		FActiveOnly: BG;
		FEnabled: BG;
		FSuspended: BG;
		FFrameRate: Integer;
		FInitialized: BG;
		FEventStep: TEventStep;
		FInterval: UG;
		FInterval12: S8;
		FNowFrameRate: Integer;
		FOldTime: S8;
		FOldTime2: U8;
		FOnActivate: TNotifyEvent;
		FOnDeactivate: TNotifyEvent;
		FOnTimer: TDTimerEvent;
		FLastLeaveTime: U8;
		TotalLags: UG;

		function AppProc(var Message: TMessage): Boolean;
		procedure Finalize;
		procedure Initialize;
		procedure Resume;
		procedure SetActiveOnly(Value: BG);
		procedure SetEnabled(Value: BG);
		procedure SetEventStep(Value: TEventStep);
		procedure SetInterval(Value: UG);
		procedure Suspend;
		procedure Step;
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
	published
		property ActiveOnly: BG read FActiveOnly write SetActiveOnly default False;
		property Enabled: BG read FEnabled write SetEnabled default True;
		property Interval: UG read FInterval write SetInterval default 1000;
		property EventStep: TEventStep read FEventStep write SetEventStep default esInterval;
		property OnTimer: TDTimerEvent read FOnTimer write FOnTimer;
		property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
		property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
	end;

procedure TryTimer;

var
	NowTime: S8; // Actual PerformanceCounter Value
	// Statistic Values
	TimLeave, TimSleep, TimWork, CPUUsage, TimWork2, TimSleep2, CPUUsage2: U8;

implementation

uses
	Math,
	uMsg;

type
	TDIdleTimer = class(TComponent)
	private
		Timers: array of TDTimer;
		procedure AppIdle(Sender: TObject; var Done: Boolean);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;

const
	LagTime = LoopSleepTime;

var
	DIdleTimer: TDIdleTimer;

procedure TryTimer;
var Done: Boolean;
begin
	Done := False;
	if Assigned(DIdleTimer) then
		DIdleTimer.AppIdle(nil, Done);
end;

constructor TDIdleTimer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
//	Application.HookMainWindow(AppProc);
	Application.OnIdle := AppIdle;
	TimLeave := PerformanceCounter;
end;

destructor TDIdleTimer.Destroy;
begin
//	SetLength(Timers, 0);
//	Application.UnHookMainWindow(AppProc);
	Application.OnIdle := nil;
	inherited Destroy;
end;

var
	MinTime: S8;

procedure TDIdleTimer.AppIdle(Sender: TObject; var Done: Boolean);
var
	i: SG;
	t: S8;
	StartTime: U8;
begin
	MinTime := High(MinTime);

	StartTime := PerformanceCounter;
	TimSleep := StartTime - TimLeave;
	i := 0;
	while i < Length(Timers) do
	begin
		DIdleTimer.Timers[i].Step;
		Inc(i);
	end;

	TimLeave := PerformanceCounter;
	TimWork := TimLeave - StartTime;
	t := TimWork + TimSleep;
	if t > 0 then
	begin
		CPUUsage := RoundDivS8(1000 * TimWork, t);
	end;
	Inc(TimWork2, TimWork);
	Inc(TimSleep2, TimSleep);
	t := TimWork2 + TimSleep2;
	if t >= PerformanceFrequency then
	begin
		CPUUsage2 := RoundDivS8(1000 * TimWork2, t);
		TimWork2 := 0;
		TimSleep2 := 0;
	end;

	if MinTime <> High(MinTime) then
	begin
		Done := False;
		t := 1000 div 2 * MinTime div PerformanceFrequency;
		if t >= 2 then
			Sleep(Min(t, LagTime));
	end
	else
		Done := True;
end;

constructor TDTimer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FActiveOnly := False;
	FEnabled := True;
	Interval := 1000;
	if (not (csDesigning in ComponentState)) then
	begin
		if not Assigned(DIdleTimer) then DIdleTimer := TDIdleTimer.Create(nil);
		SetLength(DIdleTimer.Timers, Length(DIdleTimer.Timers) + 1);
		DIdleTimer.Timers[Length(DIdleTimer.Timers) - 1] := Self;
		Application.HookMainWindow(AppProc);
		TimLeave := PerformanceCounter;
	end;
end;

destructor TDTimer.Destroy;
var i, j: SG;
begin
	if (not (csDesigning in ComponentState)) then
	begin
		Application.UnHookMainWindow(AppProc);
		if Assigned(DIdleTimer) then
			for i := 0 to Length(DIdleTimer.Timers) - 1 do
			begin
				if DIdleTimer.Timers[i] = Self then
				begin
					for j := i to Length(DIdleTimer.Timers) - 2 do
						DIdleTimer.Timers[j] := DIdleTimer.Timers[j + 1];
					SetLength(DIdleTimer.Timers, Length(DIdleTimer.Timers) - 1);
					Break;
				end;
			end;
		Finalize;
	end;

	if (not (csDesigning in ComponentState)) then // Free DIdleTimers when contains no Timers
	begin
		if Assigned(DIdleTimer) then
		if Length(DIdleTimer.Timers) = 0 then
		begin
			FreeAndNil(DIdleTimer);
		end;
	end;
	inherited Destroy;
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
	FLastLeaveTime := PerformanceCounter;
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
	FOldTime := PerformanceCounter;
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
	esInterval: FInterval12 := RoundDivS8(U8(FInterval) * PerformanceFrequency, 1000);
	esFrequency: FInterval12 := RoundDivS8(PerformanceFrequency, FInterval);
	else FInterval12 := FInterval;
	end;
	if FInterval12 <= 0 then FInterval12 := 1;
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

procedure TDTimer.Step;
var
	NTime: S8;
	t: S8;
begin
	if (FEnabled) and ((FSuspended = False) or (FActiveOnly = False)) then
	begin
		NowTime := PerformanceCounter;
		TimSleep := NowTime - TimLeave;
		ElapsedTime := NowTime - FOldTime;
//				if MinTime > FInterval12 then MinTime := FInterval12;
		if FEventStep = esCPU then
		begin
			if FInterval = 0 then FInterval := 1;
			FInterval12 := Max(RoundDivU8(100 * TimWork, FInterval), 1);
		end;
		if (ElapsedTime > 0) and (ElapsedTime + RoundDivS8(PerformanceFrequency * LagTime, 2 * Second) >= FInterval12) then
		begin
			// Frame Rate
			Inc(FNowFrameRate);
			t := NowTime - FOldTime2;
			if t >= PerformanceFrequency then
			begin
				if t = 0 then
					FFrameRate := High(FFrameRate)
				else
					FFrameRate := RoundDivS8(FNowFrameRate * PerformanceFrequency * Second, t);
				FNowFrameRate := 0;
{						LagCount := ElapsedTime div PerformanceFrequency;
				if LagCount < 1 then LagCount := 1;
				Inc(FOldTime2, LagCount * PerformanceFrequency);}
				FOldTime2 := NowTime;
			end;

			// Lags
			LagCount := ElapsedTime div FInterval12;
			if LagCount < 1 then LagCount := 1;
			if LagCount > 1 then Inc(TotalLags, LagCount - 1);
			t := U8(LagCount) * FInterval12;
			Inc(FOldTime, t);

			Inc(Clock, ElapsedTime + t);

			ElapsedTime := NowTime - FLastLeaveTime;
			FLastLeaveTime := NowTime;

{			esCPU: SetPriorityClass(GetCurrentProcess, HIGH);
			else SetPriorityClass(GetCurrentProcess, NORMAL); TODO : }

			DoTimer;

			NTime := PerformanceCounter;
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
			if t >= PerformanceFrequency then
			begin
				CPUUsage2 := RoundDivS8(1000 * TimWork2, t);
				LagCount2 := TotalLags;
				TotalLags := 0;
				TimWork2 := 0;
				TimSleep2 := 0;
			end;

			Inc(TimerCount);
		end;
		if (MinTime = High(MinTime)) or (MinTime + NowTime > FInterval12 + FOldTime) then
      MinTime := FInterval12 - NowTime + FOldTime;
	end;
end;

end.
