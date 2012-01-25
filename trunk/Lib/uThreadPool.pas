unit uThreadPool;

interface

uses
	uTypes,
	uQueue,
	Classes;

// TODO: Wait for command

type
	TCommand = class
	private
		// Name: string;
	protected
		Thread: TThread;
		procedure Execute; virtual; abstract;
	end;

	TThreadPool = class
	private
		FMaxThreads: SG;
		FRunThreads: SG;
		FWorking: SG;
		FThreads: array of TThread;
		FQueue: TQueue; // array of TCommand;
		procedure SetRunThreads(Value: SG);
		procedure SetMaxThreads(Value: SG);
		procedure QueueToThread;
	public
		constructor Create;
		destructor Destroy; override;
		procedure AddCommand(const Command: TCommand);
		procedure RandomizeCommands;
		procedure SortCommands(const A: TArrayOfSG);
		procedure Clear;
		procedure Pause;
		procedure Resume;
		procedure Stop;
		property MaxThreads: SG read FMaxThreads write SetMaxThreads;
	end;

implementation

uses
	uLog,
	uSysInfo,
	uSorts,
	uMath,
	SysUtils,
	Windows, uData;

type
	TOneThread = class(TThread)
	private
		FId: SG;
		Command: TCommand;
		FThreadPool: TThreadPool;
		procedure GetQueueCommand;
	public
		constructor Create;
	protected
		procedure Execute; override;
	end;

{ TThreadPool }

procedure TThreadPool.AddCommand(const Command: TCommand);
begin
	FQueue.Add(Command);
	QueueToThread;
end;

procedure TThreadPool.Clear;
begin
	FQueue.Clear;
end;

constructor TThreadPool.Create;
begin
	inherited;

	FQueue := TQueue.Create;

	FRunThreads := 0;
	SetMaxThreads(GSysInfo.LogicalProcessorCount);
end;

destructor TThreadPool.Destroy;
begin
	Pause;
//	Stop;
	FreeAndNil(FQueue);

	inherited;
end;

procedure TThreadPool.Pause;
var
	i: SG;
begin
	for i := 0 to Length(FThreads) - 1 do
		if FThreads[i] <> nil then
			FThreads[i].Suspend;
end;

procedure TThreadPool.QueueToThread;
begin
	if FRunThreads > FWorking then
		Resume;

	if FQueue.Count > (FRunThreads - FWorking) then
		SetRunThreads(FQueue.Count + FWorking);
end;

procedure TThreadPool.RandomizeCommands;
var
	Count: SG;
	i, X: SG;
begin
	Count := FQueue.Count;
	if Count <= 1 then Exit;
	for i := 0 to Count - 1 do
	begin
		X := Random(Count);
		FQueue.Swap(i, X);
{		T := FQueue[i];
		FQueue[i] := FQueue[X];
		FQueue[X] := T;}
	end;
end;

procedure TThreadPool.Resume;
var
	i: SG;
begin
	for i := 0 to Length(FThreads) - 1 do
		if FThreads[i] <> nil then
			FThreads[i].Resume;
end;

procedure TThreadPool.SetMaxThreads(Value: SG);
begin
	if Value <> FMaxThreads then
	begin
		FMaxThreads := Value;
		QueueToThread;
	end;
end;

procedure TThreadPool.SetRunThreads(Value: SG);
var
	i: SG;
	OneThread: TOneThread;
begin
	Value := Range(1, Value, FMaxThreads);
	if Value > FRunThreads then
	begin
		SetLength(FThreads, Value);
		for i := FRunThreads to Value - 1 do
		begin
			OneThread := TOneThread.Create;
			{$ifopt d+}
			{$ifdef UNICODE}
			RegisterExpectedMemoryLeak(OneThread);
			OneThread.NameThreadForDebugging(AnsiString('Process ' + IntToStr(i)));
			{$endif}
			{$endif}
			OneThread.Priority := tpLower;
			OneThread.FThreadPool := Self;
			OneThread.FId := i;
			Inc(FRunThreads);
			FThreads[i] := OneThread;
			OneThread.Resume;
		end;
	end;
end;

procedure TThreadPool.SortCommands(const A: TArrayOfSG);
var
	AIndex: TArrayOfSG;
	FQueue2: TQueue;
	i: SG;
	n: SG;
begin
	// Sort
	SetLength(AIndex, Length(A));
	FillOrderUG(AIndex[0], Length(AIndex));
	SortS4(False, False, PArraySG(AIndex), PArrayS4(A), Length(AIndex));

	// Add unsorted commands
	n := FQueue.Count - Length(A);
	FQueue2 := TQueue.Create;
	for i := 0 to n - 1 do
	begin
		FQueue2.Add(TCommand(FQueue[i]^));
	end;

	// Add sorted commands
	for i := 0 to Length(A) - 1 do
	begin
		FQueue2.Add(TCommand(FQueue[n + AIndex[i]]^));
	end;

	for i := 0 to FQueue.Count - 1 do
		FQueue.ReplaceObject(i, nil);

//	FQueue[i] := nil;
	FQueue.Free;
	FQueue := FQueue2;
end;

procedure TThreadPool.Stop;
begin
	FQueue.Clear;

//	Resume;

{	while FRunThreads > 0 do
	begin
		Sleep(LoopSleepTime);
	end; DeadLock }

(*	FMaxThreads := 0;
	for i := 0 to Length(FThreads) - 1 do
	begin
		if FThreads[i] <> nil then
			try
				FThreads[i].WaitFor;
			except
			end;
	end; *)
end;

{ TOneThread }

constructor TOneThread.Create;
begin
	FreeOnTerminate := True;
	inherited Create(True);
end;

procedure TOneThread.Execute;
begin
	try
		while FId < {FThreadPool.FRunThreads <=} FThreadPool.FMaxThreads do
		begin
			Synchronize(GetQueueCommand);
			if Command <> nil then
			begin
				{$ifopt d+}
				if LogDebug then LogAdd('Execute Command', mlDebug);
				{$endif}
				Inc(FThreadPool.FWorking);
				try
				  Command.Thread := Self;
					Command.Execute;
				finally
					Dec(FThreadPool.FWorking);
					Command.Free;
				end;
			end
			else
			begin
				Suspend;
//				Sleep(FThreadPool.FRunThreads * LoopSleepTime);
			end;
		end;
	finally
		FThreadPool.FThreads[FId] := nil;
		Dec(FThreadPool.FRunThreads);
	end;
end;

procedure TOneThread.GetQueueCommand;
begin
{	if FThreadPool.FQueue = nil then
		Command := nil
	else // TODO Maybe Not Needed}
		Command := FThreadPool.FQueue.GetAndDeleteFirst as TCommand;
end;

end.
