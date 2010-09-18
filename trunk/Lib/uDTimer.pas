unit uDTimer;

interface

{$R *.RES}
uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
	TDTimerEvent = procedure(Sender: TObject) of object;

	TDTimer = class(TComponent)
	private
		FActiveOnly: Boolean;
		FEnabled: Boolean;
		FSuspended: Boolean;
		FFrameRate: Integer;
		FInitialized: Boolean;
		FInterval: Cardinal;
		FInterval12: Cardinal;
		FNowFrameRate: Integer;
		FOldTime: Int64;
		FOldTime2: Int64;
		FOnActivate: TNotifyEvent;
		FOnDeactivate: TNotifyEvent;
		FOnTimer: TDTimerEvent;
		function AppProc(var Message: TMessage): Boolean;
		procedure Finalize;
		procedure Initialize;
		procedure Resume;
		procedure SetActiveOnly(Value: Boolean);
		procedure SetEnabled(Value: Boolean);
		procedure SetInterval(Value: Cardinal);
		procedure Suspend;
	protected
		procedure DoActivate; virtual;
		procedure DoDeactivate; virtual;
		procedure DoTimer; virtual;
		procedure Loaded; override;
	public
		TimerCount: UG;
		Clock,
		NowTime,
		ElapsedTime: Int64;
		LagCount: UG;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property FrameRate: Integer read FFrameRate;
	published
		property ActiveOnly: Boolean read FActiveOnly write SetActiveOnly;
		property Enabled: Boolean read FEnabled write SetEnabled;
		property Interval: Cardinal read FInterval write SetInterval;
		property OnTimer: TDTimerEvent read FOnTimer write FOnTimer;
		property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
		property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
	end;

procedure Register;

implementation

uses
	Math,
	uSysInfo;

type
	TDIdleTimer = class(TComponent)
	private
		Timers: array of TDTimer;
		procedure AppIdle(Sender: TObject; var Done: Boolean);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;

var DIdleTimer: TDIdleTimer;

{
function Max(Val1, Val2: Integer): Integer;
begin
	if Val1>=Val2 then Result := Val1 else Result := Val2;
end;

function Min(Val1, Val2: Integer): Integer;
begin
	if Val1<=Val2 then Result := Val1 else Result := Val2;
end;
}

constructor TDIdleTimer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
//	Application.HookMainWindow(AppProc);
	Application.OnIdle := AppIdle;
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
	NTime, t: S64;
	MinTime: S64;
begin
	NTime := 0;
	MinTime := MaxInt64;

	for i := 0 to Length(Timers) - 1 do
	begin
		with DIdleTimer.Timers[i] do
		begin
			if (Enabled) and ((FSuspended = False) or (FActiveOnly = False)) then
			begin
				if NTime = 0 then NTime := PerformanceCounter;
				NowTime := NTime;
				ElapsedTime := NowTime - FOldTime;
//				if MinTime > FInterval12 then MinTime := FInterval12;
				if (ElapsedTime > 0) and (ElapsedTime {+ RoundDiv64(PerformanceCounter * LagTime, 2 * 1000)} >= FInterval12) then
				begin
					LagCount := ElapsedTime div (PerformanceFrequency);
					if LagCount < 1 then LagCount := 1;

					Inc(FNowFrameRate);
					t := NowTime - FOldTime2;
					if t >= PerformanceFrequency then
					begin
						if t = 0 then
							FFrameRate := MaxInt
						else
							FFrameRate := RoundDiv64(FNowFrameRate * (PerformanceFrequency) * 1000, t);
						FNowFrameRate := 0;
						Inc(FOldTime2, LagCount * PerformanceFrequency); //NowTime;
					end;


					LagCount := ElapsedTime div FInterval12;
					if LagCount < 1 then LagCount := 1;
					Inc(FOldTime, Int64(LagCount) * FInterval12);// := NowTime;


					Inc(Clock, ElapsedTime);
					DoTimer;
					Inc(TimerCount);
				end;
				if MinTime > FInterval12 - NowTime + FOldTime then MinTime := FInterval12 - NowTime + FOldTime;
			end;
		end;
	end;

	if MinTime <> MaxInt64 then
	begin
		Done := False;
//		Sleep(Min(MinTime div 2, LagTime));
		Sleep(Min(1000 div 2 * MinTime div PerformanceFrequency, LagTime));
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
	// D??? Error
	if (not (csDesigning in ComponentState)) then
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
	if Assigned(FOnTimer) then FOnTimer(Self);
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

procedure TDTimer.SetInterval(Value: Cardinal);
begin
	if FInterval <> Value then
	begin
		FInterval := Max(Value, 0);
		FInterval12 := RoundDiv64(FInterval * PerformanceFrequency, 1000);
		if FInterval12 <= 0 then FInterval12 := 1;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDTimer]);
end;

end.
