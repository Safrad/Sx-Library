unit uNewThread;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses SysUtils, Classes;

type
	TExecuteProcedure = procedure(ANewThread: TThread);
	TExecuteProcedureOfObject = procedure(AInstance: TObject; ANewThread: TThread);

function RunInNewThread(AExecuteProcedure: TExecuteProcedure; ThreadPriority: TThreadPriority = tpNormal; const AFreeOnTerminate: Boolean = True): TThread; overload;
function RunInNewThread(AInstance: TObject; AExecuteProcedureOfObject: TExecuteProcedureOfObject; ThreadPriority: TThreadPriority = tpNormal; const AFreeOnTerminate: Boolean = True): TThread; overload;

implementation

type
	TNewThread = class(TThread)
	private
		ExecuteProcedure: TExecuteProcedure;
    Instance: TObject;
		ExecuteProcedureOfObject: TExecuteProcedureOfObject;
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
  inherited;

  if Assigned(ExecuteProcedure) then
  	ExecuteProcedure(Self);
  if Assigned(ExecuteProcedureOfObject) then
  	ExecuteProcedureOfObject(Instance, Self);
end;

function RunInNewThread(AExecuteProcedure: TExecuteProcedure; ThreadPriority: TThreadPriority = tpNormal; const AFreeOnTerminate: Boolean = True): TThread;
var
	NewThread: TNewThread;
begin
	NewThread := TNewThread.Create;
  NewThread.FreeOnTerminate := AFreeOnTerminate;
	NewThread.Priority := ThreadPriority;
	NewThread.ExecuteProcedure := AExecuteProcedure;
	{$if CompilerVersion >= 20}
	NewThread.Start;
	{$else}
	NewThread.Resume;
	{$ifend}
	Result := NewThread;
end;

function RunInNewThread(AInstance: TObject; AExecuteProcedureOfObject: TExecuteProcedureOfObject; ThreadPriority: TThreadPriority = tpNormal; const AFreeOnTerminate: Boolean = True): TThread; overload;
var
	NewThread: TNewThread;
begin
	NewThread := TNewThread.Create;
  NewThread.FreeOnTerminate := AFreeOnTerminate;
	NewThread.Priority := ThreadPriority;
  NewThread.Instance := AInstance;
	NewThread.ExecuteProcedureOfObject := AExecuteProcedureOfObject;
	{$if CompilerVersion >= 20}
	NewThread.Start;
	{$else}
	NewThread.Resume;
	{$ifend}
	Result := NewThread;
end;

end.
