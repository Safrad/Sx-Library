unit uNewThread;

interface

uses SysUtils, Classes;

type
	TExecuteProcedure = procedure(ANewThread: TThread);

function RunInNewThread(AExecuteProcedure: TExecuteProcedure; ThreadPriority: TThreadPriority = tpNormal): TThread;

implementation

type
	TNewThread = class(TThread)
	private
		ExecuteProcedure: TExecuteProcedure;
	protected
		procedure Execute; override;
	public
		{ Public declarations }
		constructor Create;
	end;

{ TNewThread }

constructor TNewThread.Create;
begin
	FreeOnTerminate := True;
	inherited Create(True);
end;

procedure TNewThread.Execute;
begin
	ExecuteProcedure(Self);
end;

function RunInNewThread(AExecuteProcedure: TExecuteProcedure; ThreadPriority: TThreadPriority = tpNormal): TThread;
var
	NewThread: TNewThread;
begin
	NewThread := TNewThread.Create;
	NewThread.Priority := ThreadPriority;
	NewThread.ExecuteProcedure := AExecuteProcedure;
	{$ifdef VER150}
	NewThread.Resume;
	{$else}
	NewThread.Start;
	{$endif}
	Result := NewThread;
end;

end.
