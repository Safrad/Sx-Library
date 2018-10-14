// Singleton
// Used in TDTimer
unit uDIdleTimer;

interface

uses
  uTypes,
  uDTimer;

const
	LagTime = LoopSleepTime;

type
	TDIdleTimer = class
	private
	 	FTimers: array of TDTimer;
    FMinTime: S8;
  	// Statistic Values
  	FTimLeave, FTimSleep, FTimWork, FCPUUsage, FTimWork2, FTimSleep2, FCPUUsage2: U8;
		procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure SetMinTime(const Value: S8);
    function GetTimerCount: UG;
	public
		constructor Create;
		destructor Destroy; override;

    function ExecuteTimers: BG;

    procedure AddTimer(const ADTimer: TDTimer);
    procedure RemoveTimer(const ADTimer: TDTimer);

    property MinTime: S8 read FMinTime write SetMinTime;
    property TimerCount: UG read GetTimerCount;
	end;

function DIdleTimer: TDIdleTimer;
procedure FreeDIdleTimer;

implementation

uses
  SysUtils,
  Windows,
  Forms,
  Math,
  uMath,
  uMainTimer;

var
	GIdleTimer: TDIdleTimer;

function DIdleTimer: TDIdleTimer;
begin
		if GIdleTimer = nil then
    begin
      GIdleTimer := TDIdleTimer.Create;
    end;
  Result := GIdleTimer;
end;

procedure FreeDIdleTimer;
begin
  FreeAndNil(GIdleTimer);
end;

{ TDIdleTimer }

constructor TDIdleTimer.Create;
begin
	inherited;

//	Application.HookMainWindow(AppProc);
	Application.OnIdle := AppIdle;
	FTimLeave := MainTimer.Value.Ticks;
end;

destructor TDIdleTimer.Destroy;
begin
//	SetLength(Timers, 0);
//	Application.UnHookMainWindow(AppProc);
	Application.OnIdle := nil;
	inherited Destroy;
end;

function TDIdleTimer.ExecuteTimers: BG;
var
	i: SG;
	t: S8;
	StartTime: U8;
begin
	FMinTime := High(FMinTime);

	StartTime := MainTimer.Value.Ticks;
	FTimSleep := StartTime - FTimLeave;
	i := 0;
	while i < Length(FTimers) do
	begin
		FTimers[i].Step;
		Inc(i);
	end;

	FTimLeave := MainTimer.Value.Ticks;
	FTimWork := FTimLeave - StartTime;
	t := FTimWork + FTimSleep;
	if t > 0 then
	begin
		FCPUUsage := RoundDivS8(1000 * FTimWork, t);
	end;
	Inc(FTimWork2, FTimWork);
	Inc(FTimSleep2, FTimSleep);
	t := FTimWork2 + FTimSleep2;
	if t >= MainTimer.Frequency then
	begin
		FCPUUsage2 := RoundDivS8(1000 * FTimWork2, t);
		FTimWork2 := 0;
		FTimSleep2 := 0;
	end;

	if FMinTime <> High(FMinTime) then
	begin
		Result := False;
		t := 1000 div 2 * FMinTime div MainTimer.Frequency;
		if t >= 2 then
			Sleep(Min(t, LagTime));
	end
	else
		Result := True;
end;

function TDIdleTimer.GetTimerCount: UG;
begin
  Result := Length(FTimers);
end;

procedure TDIdleTimer.RemoveTimer(const ADTimer: TDTimer);
var
  i, j: SG;
begin
  for i := 0 to Length(FTimers) - 1 do
  begin
    if FTimers[i] = ADTimer then
    begin
      for j := i to Length(FTimers) - 2 do
        FTimers[j] := FTimers[j + 1];
      SetLength(FTimers, Length(FTimers) - 1);
      Break;
    end;
  end;
end;

procedure TDIdleTimer.SetMinTime(const Value: S8);
begin
  FMinTime := Value;
end;

procedure TDIdleTimer.AddTimer(const ADTimer: TDTimer);
begin
  SetLength(FTimers, Length(FTimers) + 1);
		FTimers[Length(FTimers) - 1] := ADTimer;
end;

procedure TDIdleTimer.AppIdle(Sender: TObject; var Done: Boolean);
begin
  Done := ExecuteTimers;
end;

end.
