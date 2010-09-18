//* File:     Lib\uDTimer.pas
//* Created:  2000-08-01
//* Modified: 2004-08-03
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDTimer;

interface

{$R *.RES}
uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
	TDTimerEvent = procedure(Sender: TObject) of object;

	TEventStep = (esInterval, esFrequency, esCPU);

	TDTimer = class(TComponent)
	private
		FActiveOnly: Boolean;
		FEnabled: Boolean;
		FSuspended: Boolean;
		FFrameRate: Integer;
		FInitialized: Boolean;
		FEventStep: TEventStep;
		FInterval: Cardinal;
		FInterval12: Cardinal;
		FNowFrameRate: Integer;
		FOldTime: Int64;
		FOldTime2: Int64;
		FOnActivate: TNotifyEvent;
		FOnDeactivate: TNotifyEvent;
		FOnTimer: TDTimerEvent;
		TotalLags: UG;

		function AppProc(var Message: TMessage): Boolean;
		procedure Finalize;
		procedure Initialize;
		procedure Resume;
		procedure SetActiveOnly(Value: Boolean);
		procedure SetEnabled(Value: Boolean);
		procedure SetEventStep(Value: TEventStep);
		procedure SetInterval(Value: Cardinal);
		procedure Suspend;
	protected
		procedure DoActivate; virtual;
		procedure DoDeactivate; virtual;
		procedure DoTimer; virtual;
		procedure Loaded; override;
		procedure InitInterval;
	public
		TimerCount: UG;
		Clock,
		ElapsedTime: Int64;
		LagCount, LagCount2: UG;
		TimLeave: U8;
		TimSleep, TimWork, CPUUsage, TimWork2, TimSleep2, CPUUsage2: U8;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property FrameRate: Integer read FFrameRate;
		procedure Reset;
	published
		property ActiveOnly: Boolean read FActiveOnly write SetActiveOnly;
		property Enabled: Boolean read FEnabled write SetEnabled;
		property Interval: Cardinal read FInterval write SetInterval;
		property EventStep: TEventStep read FEventStep write SetEventStep;
		property OnTimer: TDTimerEvent read FOnTimer write FOnTimer;
		property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
		property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
	end;

procedure TryTimer;
procedure Register;

var
	NowTime: U8; // Actual PerformanceCounter Value
	// Statistic Values
	TimLeave, TimSleep, TimWork, CPUUsage, TimWork2, TimSleep2, CPUUsage2: U8;

implementation

uses
	Math,
	uSysInfo, uError;

type
	TDIdleTimer = class(TComponent)
	private
		Timers: array of TDTimer;
		procedure AppIdle(Sender: TObject; var Done: Boolean);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;

var
	DIdleTimer: TDIdleTimer;

procedure TryTimer;
var Done: BG;
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

procedure TDIdleTimer.AppIdle(Sender: TObject; var Done: Boolean);
const LagTime = 40;
var
	i: SG;
	NTime, t: S8;
	MinTime: S8;
	StartTime: U8;
begin
//	NTime := 0;
	MinTime := High(MinTime);

	StartTime := PerformanceCounter;
	TimSleep := StartTime - TimLeave;
	i := 0;
	while i < Length(Timers) do
	begin
		if (DIdleTimer.Timers[i].FEnabled) and ((DIdleTimer.Timers[i].FSuspended = False) or (DIdleTimer.Timers[i].FActiveOnly = False)) then
		begin
//				if NTime = 0 then
			begin
				NTime := PerformanceCounter;
				DIdleTimer.Timers[i].TimSleep := NTime - DIdleTimer.Timers[i].TimLeave;
			end;
			NowTime := NTime;
			DIdleTimer.Timers[i].ElapsedTime := NowTime - DIdleTimer.Timers[i].FOldTime;
//				if MinTime > FInterval12 then MinTime := FInterval12;
			if DIdleTimer.Timers[i].FEventStep = esCPU then
			begin
				if DIdleTimer.Timers[i].FInterval = 0 then DIdleTimer.Timers[i].FInterval := 1;
				DIdleTimer.Timers[i].FInterval12 := Max(RoundDiv(100 * DIdleTimer.Timers[i].TimWork, DIdleTimer.Timers[i].FInterval), 1);
			end;
			if (DIdleTimer.Timers[i].ElapsedTime > 0) and (DIdleTimer.Timers[i].ElapsedTime + RoundDivS8(PerformanceFrequency * LagTime, 2 * 1000) >= DIdleTimer.Timers[i].FInterval12) then
			begin
				// Frame Rate
				Inc(DIdleTimer.Timers[i].FNowFrameRate);
				t := NowTime - DIdleTimer.Timers[i].FOldTime2;
				if t >= PerformanceFrequency then
				begin
					if t = 0 then
						DIdleTimer.Timers[i].FFrameRate := High(DIdleTimer.Timers[i].FFrameRate)
					else
						DIdleTimer.Timers[i].FFrameRate := RoundDivS8(DIdleTimer.Timers[i].FNowFrameRate * PerformanceFrequency * 1000, t);
					DIdleTimer.Timers[i].FNowFrameRate := 0;
{						LagCount := ElapsedTime div PerformanceFrequency;
					if LagCount < 1 then LagCount := 1;
					Inc(FOldTime2, LagCount * PerformanceFrequency);}
					DIdleTimer.Timers[i].FOldTime2 := NowTime;
				end;

				// Lags
				DIdleTimer.Timers[i].LagCount := DIdleTimer.Timers[i].ElapsedTime div DIdleTimer.Timers[i].FInterval12;
				if DIdleTimer.Timers[i].LagCount < 1 then DIdleTimer.Timers[i].LagCount := 1;
				if DIdleTimer.Timers[i].LagCount > 1 then Inc(DIdleTimer.Timers[i].TotalLags, DIdleTimer.Timers[i].LagCount - 1);
				t := Int64(DIdleTimer.Timers[i].LagCount) * DIdleTimer.Timers[i].FInterval12;
				Inc(DIdleTimer.Timers[i].FOldTime, t);// := NowTime;

				Inc(DIdleTimer.Timers[i].Clock, DIdleTimer.Timers[i].ElapsedTime + t);

				DIdleTimer.Timers[i].DoTimer;

				NTime := PerformanceCounter;
				DIdleTimer.Timers[i].TimLeave := NTime;
				DIdleTimer.Timers[i].TimWork := DIdleTimer.Timers[i].TimLeave - NowTime;
				NowTime := NTime;
				t := DIdleTimer.Timers[i].TimWork + DIdleTimer.Timers[i].TimSleep;
				if t > 0 then
				begin
					DIdleTimer.Timers[i].CPUUsage := RoundDivS8(1000 * DIdleTimer.Timers[i].TimWork, t);
				end;
				Inc(DIdleTimer.Timers[i].TimWork2, DIdleTimer.Timers[i].TimWork);
				Inc(DIdleTimer.Timers[i].TimSleep2, DIdleTimer.Timers[i].TimSleep);
				t := DIdleTimer.Timers[i].TimWork2 + DIdleTimer.Timers[i].TimSleep2;
				if t >= PerformanceFrequency then
				begin
					DIdleTimer.Timers[i].CPUUsage2 := RoundDivS8(1000 * DIdleTimer.Timers[i].TimWork2, t);
					DIdleTimer.Timers[i].LagCount2 := DIdleTimer.Timers[i].TotalLags;
					DIdleTimer.Timers[i].TotalLags := 0;
					DIdleTimer.Timers[i].TimWork2 := 0;
					DIdleTimer.Timers[i].TimSleep2 := 0;
				end;

				Inc(DIdleTimer.Timers[i].TimerCount);
			end;
			if MinTime > DIdleTimer.Timers[i].FInterval12 - NowTime + DIdleTimer.Timers[i].FOldTime then MinTime := DIdleTimer.Timers[i].FInterval12 - NowTime + DIdleTimer.Timers[i].FOldTime;
		end;
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
	FActiveOnly := True;
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
			DIdleTimer.Free; DIdleTimer := nil;
		end;
	end;
	inherited Destroy;
end;

function TDTimer.AppProc(var Message: TMessage): Boolean;
begin
	Result := False;
	if Message.Msg = 0 then Exit;
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
	// Application.ProcessMessages;
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
		if Assigned(FOnTimer) then FOnTimer(Self);
	except
		on E: Exception do MessageD(E.Message, mtError, [mbOk]);
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

	if FActiveOnly then
	begin
		if Application.Active then
			Resume;
	end else
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

procedure TDTimer.SetActiveOnly(Value: Boolean);
begin
	if FActiveOnly <> Value then
	begin
		FActiveOnly := Value;

		if Application.Active and FActiveOnly then
			if FInitialized and FActiveOnly then Suspend;
	end;
end;

procedure TDTimer.SetEnabled(Value: Boolean);
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
	esInterval: FInterval12 := RoundDivS8(FInterval * PerformanceFrequency, 1000);
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

procedure TDTimer.SetInterval(Value: Cardinal);
begin
	if FInterval <> Value then
	begin
		FInterval := Max(Value, 1);
		InitInterval;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDTimer]);
end;

end.
