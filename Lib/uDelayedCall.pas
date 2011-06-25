unit uDelayedCall;

interface

uses
	uTypes;


type
//	TProcedure = procedure(Sender: TObject) of object;
	TProcedure = procedure;

procedure RegisterDelayedCall(const Index: SG; Address: TProcedure);
procedure DelayedCall(const Index: SG);
procedure DirectCall(const Index: SG);
procedure DelayedTimer;
function GetDelayedCallEnabled: BG;
procedure SetDelayedCallEnabled(const Value: BG);

const
	DefaultDelayedCallTime = 250; // ms
var
	DelayedCallTime: U4 = DefaultDelayedCallTime;

implementation

uses
	SysUtils, Windows,
	uSimulation,
	uData;

type
	PCall = ^TCall;
	TCall = record
		MissedCount: UG;
		NextDrawTime: U4;
		Address: TProcedure;
	end;
var
	Calls: TData;
	DelayedCallEnabled: BG = True;

procedure RegisterDelayedCall(const Index: SG; Address: TProcedure);
var
	Call: PCall;
begin
	Assert(Index >= 0);
	if Index >= Calls.Count then
		Calls.SetCount(Index + 1);
//	Call := Calls.Add;
	Call := Calls.Get(Index);
	Call.MissedCount := 0;
	Call.NextDrawTime := 0;
	Call.Address := Address;
end;

var
	FCriticalSection: TRTLCriticalSection;
	Crit: SG;

procedure CallProc(Call: PCall);
begin
	if Assigned(Call.Address) then
	begin
		if Crit = 0 then
			EnterCriticalSection(FCriticalSection);
		Inc(Crit);
		try
			Call.Address();
		finally
			Dec(Crit);
			if Crit = 0 then
				LeaveCriticalSection(FCriticalSection);
		end;
	end;
	Call.MissedCount := 0;
	GetGTime;
	Call.NextDrawTime := GTime;
end;

procedure DelayedCall(const Index: SG);
var
	Call: PCall;
begin
	Assert(Index >= 0);
	Assert(Index < Calls.Count);
	Call := Calls.Get(Index);
	if Call <> nil then
	begin
		GetGTime;
		{$ifdef Thread}
		Inc(Call.MissedCount);
		Exit;
		{$endif}
		if DelayedCallEnabled then
		begin
			if Call.NextDrawTime + DelayedCallTime > GTime then
			begin
				Inc(Call.MissedCount);
				Exit;
			end
		end;
		CallProc(Call);
	end
	else
		Assert(False);
end;

procedure DirectCall(const Index: SG);
var
	Call: PCall;
begin
	Call := Calls.Get(Index);
	if Call <> nil then
	begin
		CallProc(Call);
	end;
end;

procedure DelayedTimer;
var
	Call: PCall;
	HTime: U4;
begin
	GetGTime;

	if DelayedCallEnabled then
		HTime := GTime
	else
		HTime := High(HTime);

	Call := Calls.GetFirst;
	while Call <> nil do
	begin
		if (Call.MissedCount > 0) and (HTime >= Call.NextDrawTime + DelayedCallTime) then
		begin
			CallProc(Call);
		end;
		Calls.Next(Call);
	end;
end;

function GetDelayedCallEnabled: BG;
begin
	Result := DelayedCallEnabled;
end;

procedure SetDelayedCallEnabled(const Value: BG);
begin
	if Value <> DelayedCallEnabled then
	begin
		DelayedCallEnabled := Value;
		if not DelayedCallEnabled then
			DelayedTimer;
	end;
end;

initialization
	InitializeCriticalSection(FCriticalSection);
	Calls := TData.Create;
	Calls.ItemSize := SizeOf(TCall);
finalization
	FreeAndNil(Calls);
	DeleteCriticalSection(FCriticalSection);
end.
