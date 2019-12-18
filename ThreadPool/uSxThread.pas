unit uSxThread;

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes;

type
  TNotifyEvent = procedure(Sender: TObject) of object;

  TThreadMethod = procedure of object;

  TSxThread = class(TThread)
  private
    FName: string;
    procedure SetName(const Value: string);
  protected
    procedure Execute; override;
  public
    constructor Create;

    {$IFDEF MSWINDOWS}
    function WaitFor: LongWord; reintroduce; virtual;
    {$ENDIF}
    procedure Terminate; reintroduce; virtual;
    function TerminateAndWaitFor: LongWord;

    {$IF Defined(MSWINDOWS) AND (CompilerVersion < 20)}
    class procedure NameThreadForDebugging(const AName: AnsiString; const AThreadId: LongWord);
    {$IFEND}

    {$IF CompilerVersion < 20}
    procedure Start;
    {$IFEND}
    property Name: string read FName write SetName;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;
{$ENDIF}



{ TSxThread }

procedure TSxThread.Execute;
begin
  inherited;

  {$IF Defined(MSWINDOWS) AND (CompilerVersion < 20)}
  if FName <> '' then
    NameThreadForDebugging(FName, ThreadID);
  {$ifend}
end;

procedure TSxThread.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    {$if CompilerVersion >= 20}
    NameThreadForDebugging(FName, ThreadId);
    {$endif}
  end;
end;

{$IF Defined(MSWINDOWS) AND (CompilerVersion < 20)}
class procedure TSxThread.NameThreadForDebugging(const AName: AnsiString; const AThreadId: LongWord);
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PAnsiChar;    // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID ($FFFFFFFF indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PAnsiChar(AName);
  ThreadNameInfo.FThreadID := AThreadId;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(LongWord), Pointer(@ThreadNameInfo));
  except
    // No Code
  end;
end;
{$IFEND}

{$IF CompilerVersion < 20}
procedure TSxThread.Start;
begin
  Resume;
end;
{$IFEND}

constructor TSxThread.Create;
begin
  inherited Create(True); // Create suspended
  Name := ClassName; // Set thread name for debugging purposes
end;

procedure TSxThread.Terminate;
begin
  inherited Terminate;

  Resume; // if suspended
end;

function TSxThread.TerminateAndWaitFor: LongWord;
begin
  Terminate;
  Result := WaitFor;
end;

{$IFDEF MSWINDOWS}
function TSxThread.WaitFor: LongWord;
begin
  // Delphi WaitFor called from main thread consumes CPU cycles
  WaitForSingleObject(Handle, INFINITE);
  GetExitCodeThread(Handle, Result);
end;
{$ENDIF}

initialization
{$IFNDEF NoInitialization}
  if not IsLibrary then
    TSxThread.NameThreadForDebugging('Main', MainThreadID);
{$ENDIF NoInitialization}

end.

