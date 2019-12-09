unit uTemporaryDirectory;

interface

uses
  uTypes,

  SyncObjs;

type
  TTemporaryDirectoryItem = record
    ThreadId: TThreadId;
    Handle: THandle;
  end;

  TTemporaryDirectory = class
  private
  	FCriticalSection: TCriticalSection;

    FApplicationTempDirHandle: THandle;
    FProcessTempDirHandle: THandle;
    FThreadTempDirHandles: array of TTemporaryDirectoryItem;

    FUserTempDir: string;
    FApplicationTempDir: string;
    FProcessTempDir: string;
    function GetDirectoryForThread(const AThreadId: TThreadId): string;
    function GetApplicationTempDir: string;
    function GetProcessTempDir: string;
    function GetThreadTempDir: string;
    function FindHandleForThreadId(const AThreadId: TThreadId): THandle;
  public
    constructor Create;
    destructor Destroy; override;

    property UserTempDir: string read FUserTempDir;
    property ApplicationTempDir: string read GetApplicationTempDir;
    property ProcessTempDir: string read GetProcessTempDir;
    property ThreadTempDir: string read GetThreadTempDir;
    procedure DeleteThread;
    procedure DeleteAll;
  end;

function TemporaryDirectory: TTemporaryDirectory;

implementation

uses
  SysUtils,
  {$ifdef MSWINDOWS}
  Winapi.Windows,
  {$endif}

  uProjectInfo,
  uStrings,
  uFiles,
  uOperatingSystem;

{ TTemporaryDirectory }

constructor TTemporaryDirectory.Create;
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;

	FUserTempDir := GetEnvironmentVariable('TEMP');
	CorrectDir(FUserTempDir);

	FApplicationTempDir := FUserTempDir + '_' + GetProjectInfo(piInternalName) + PathDelim;

	FProcessTempDir := FApplicationTempDir + IntToStr(OperatingSystem.GetCurrentProcessId) + PathDelim;
end;

procedure TTemporaryDirectory.DeleteThread;
var
  Handle: THandle;
  ThreadId: TThreadId;
begin
  ThreadId := OperatingSystem.GetCurrentThreadId;
  Handle := FindHandleForThreadId(ThreadId);
  if Handle <> 0 then
  begin
    CloseHandle(Handle);
    RemoveDirsEx(GetDirectoryForThread(ThreadId), True);
  end;
end;

procedure TTemporaryDirectory.DeleteAll;
var
  i: SG;
begin
  for i := 0 to Length(FThreadTempDirHandles) - 1 do
  begin
    CloseHandle(FThreadTempDirHandles[i].Handle);
  end;
  SetLength(FThreadTempDirHandles, 0);
  CloseHandle(FProcessTempDirHandle);
  RemoveDirsEx(FProcessTempDir, True);
end;

destructor TTemporaryDirectory.Destroy;
begin
  DeleteAll;

  CloseHandle(FApplicationTempDirHandle);

  FreeAndNil(FCriticalSection);

  inherited;
end;

function TTemporaryDirectory.GetThreadTempDir: string;
var
  Handle: THandle;
  ExistingHandle: THandle;
  ThreadId: TThreadId;
begin
  ThreadId := OperatingSystem.GetCurrentThreadId;
  Result := GetDirectoryForThread(ThreadId);
  Handle := FindHandleForThreadId(ThreadId);
  if Handle = 0 then
  begin
    ExistingHandle := CreateLockedDir(Result);
    if ExistingHandle <> INVALID_HANDLE_VALUE then
    begin
      FCriticalSection.Enter;
      try
        SetLength(FThreadTempDirHandles, Length(FThreadTempDirHandles) + 1);
        FThreadTempDirHandles[Length(FThreadTempDirHandles) - 1].ThreadId := ThreadId;
        FThreadTempDirHandles[Length(FThreadTempDirHandles) - 1].Handle := ExistingHandle;
      finally
        FCriticalSection.Leave;
      end;
    end;
  end;
end;

function TTemporaryDirectory.GetApplicationTempDir: string;
begin
  Result := FApplicationTempDir;
  if FApplicationTempDirHandle = 0 then
  begin
    FCriticalSection.Enter;
    try
      if FApplicationTempDirHandle = 0 then // Double-checked locking
        FApplicationTempDirHandle := CreateLockedDir(Result);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

function TTemporaryDirectory.GetDirectoryForThread(const AThreadId: TThreadId): string;
begin
  Result := FProcessTempDir + IntToStr(AThreadId) + PathDelim;
end;

function TTemporaryDirectory.FindHandleForThreadId(const AThreadId: TThreadId): THandle;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to Length(FThreadTempDirHandles) - 1 do
  begin
    if FThreadTempDirHandles[i].ThreadId = AThreadId then
    begin
      Result := FThreadTempDirHandles[i].Handle;
      Break;
    end;
  end;
end;

function TTemporaryDirectory.GetProcessTempDir: string;
begin
  Result := FProcessTempDir;
  if FProcessTempDirHandle = 0 then
  begin
    FCriticalSection.Enter;
    try
      if FProcessTempDirHandle = 0 then // Double-checked locking
        FProcessTempDirHandle := CreateLockedDir(Result);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

var
  GTemporaryDirectory: TTemporaryDirectory;

function TemporaryDirectory: TTemporaryDirectory;
begin
  Result := GTemporaryDirectory;
end;

initialization
{$IFNDEF NoInitialization}
  GTemporaryDirectory := TTemporaryDirectory.Create;
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GTemporaryDirectory);
{$ENDIF NoFinalization}

end.
