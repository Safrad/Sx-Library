unit uDelayedCall;

interface

uses
	uTypes,
  uTimeSpan;

{$define Thread}

type
//	TProcedure = procedure(Sender: TObject) of object;
	TProcedure = procedure;

procedure RegisterDelayedCall(const Index: SG; Address: TProcedure);
procedure DelayedCall(const Index: SG);
procedure DirectCall(const Index: SG);
procedure DelayedTimer;
function GetDelayedCallEnabled: BG;
procedure SetDelayedCallEnabled(const Value: BG);

var
	DelayedCallTime: TTimeSpan;

implementation

uses
	SysUtils,
  SyncObjs,

	uData,
  uMainTimer;

type
	PCall = ^TCall;
	TCall = record
		MissedCount: U4;
		NextDrawTime: U8;
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
	FCriticalSection: TCriticalSection;
	Crit: SG;

procedure CallProc(Call: PCall);
begin
	if Assigned(Call.Address) then
	begin
		if Crit = 0 then
			FCriticalSection.Enter;
		Inc(Crit);
		try
			Call.Address();
		finally
			Dec(Crit);
			if Crit = 0 then
				FCriticalSection.Leave;
		end;
	end;
	Call.MissedCount := 0;
	Call.NextDrawTime := MainTimer.Value.Ticks;
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
		{$ifdef Thread}
		Inc(Call.MissedCount);
		{$else}
		if DelayedCallEnabled then
		begin
			if Call.NextDrawTime + DelayedCallTime.Ticks > MainTimer.Value.Ticks then
			begin
				Inc(Call.MissedCount);
				Exit;
			end
		end;
		CallProc(Call);
		{$endif}
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
	HTime: U8;
begin
	if DelayedCallEnabled then
		HTime := MainTimer.Value.Ticks
	else
		HTime := High(HTime);

	Call := Calls.GetFirst;
	while Call <> nil do
	begin
		if (Call.MissedCount > 0) and (HTime >= Call.NextDrawTime + DelayedCallTime.Ticks) then
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
{$IFNDEF NoInitialization}
	FCriticalSection := TCriticalSection.Create;
	DelayedCallTime.Milliseconds := 250;

	Calls := TData.Create;
	Calls.ItemSize := SizeOf(TCall);
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
	FreeAndNil(Calls);
	FreeAndNil(FCriticalSection);
{$ENDIF NoFinalization}
end.
