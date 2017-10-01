unit uSxThread;

interface

uses
  Classes;

type
  TSxThread = class(TThread)
  private
    FName: string;
    procedure SetName(const Value: string);
  protected
    procedure Execute; override;
  public
    {$if CompilerVersion < 20}
    procedure Start;
    {$ifend}
    property Name: string read FName write SetName;
  end;

implementation

uses
  Windows;

{$if CompilerVersion < 20}
const
  ActualThread = $FFFFFFFF;

procedure SetThreadName(const Value: string; const ThreadId: LongWord = ActualThread);
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID ($FFFFFFFF indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar(Value);
  ThreadNameInfo.FThreadID := ThreadId;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(LongWord), Pointer(@ThreadNameInfo));
  except
    // No Code
  end;
end;
{$ifend}

{ TSxThread }

procedure TSxThread.Execute;
begin
  inherited;

  {$if CompilerVersion < 20}
  if FName <> '' then
    SetThreadName(FName);
  {$ifend}
end;

procedure TSxThread.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    {$if CompilerVersion >= 20}
    NameThreadForDebugging(FName);
    {$ifend}
  end;
end;

{$if CompilerVersion < 20}
procedure TSxThread.Start;
begin
  Resume;
end;
{$ifend}

initialization
{$IFNDEF NoInitialization}
  SetThreadName('Main');
{$ENDIF NoInitialization}

end.

