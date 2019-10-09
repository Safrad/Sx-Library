unit uProcessInfos;

interface

uses
  Generics.Collections,

  uTypes,
  uStopwatch;

type
  TProcessInfoItem = record
    ProcessId: U4;
    ParentProcessId: U4;
  end;

  TProcessInfoList = TList<TProcessInfoItem>;

  TProcessInfos = class
  private
    FStopwatch: TStopwatch;
    FCompleteList: TProcessInfoList;
    procedure TryUpdate;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ForceUpdate;

    // Output
    function CompleteList: TProcessInfoList;
    function CreateChildrenOneLevelList(const AParentProcessId: U4): TProcessInfoList;
  end;

function ProcessInfos: TProcessInfos;

implementation

uses
  SysUtils,

  WinApi.Windows,
  WinApi.TlHelp32;

var
  GProcessInfos: TProcessInfos;

function ProcessInfos: TProcessInfos;
begin
  if not Assigned(GProcessInfos) then
    GProcessInfos := TProcessInfos.Create;

  Result := GProcessInfos;
end;

{ TProcessInfos }

constructor TProcessInfos.Create;
begin
  FStopwatch := TStopwatch.Create;
  FStopwatch.Start;
  FCompleteList := TProcessInfoList.Create;
end;

function TProcessInfos.CreateChildrenOneLevelList(const AParentProcessId: U4): TProcessInfoList;
var
  ProcessInfo: TProcessInfoItem;
begin
  TryUpdate;

  Result := TProcessInfoList.Create;
  for ProcessInfo in FCompleteList do
  begin
    if ProcessInfo.ParentProcessId = AParentProcessId then
    begin
      Result.Add(ProcessInfo);
    end;
  end;
end;

destructor TProcessInfos.Destroy;
begin
  try
    FCompleteList.Free;
    FStopwatch.Free;
  finally
    inherited;
  end;
end;

procedure TProcessInfos.ForceUpdate;
var
  Snap: THandle;
  ProcessEntry: TProcessEntry32;
  Ok: BG;
  ProcessInfo: TProcessInfoItem;
begin
  FStopwatch.Stop;
  FStopwatch.Reset;
  try
    FCompleteList.Clear;
    Snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    try
      ProcessEntry.dwSize:=SizeOf(ProcessEntry);
      Ok := Process32First(Snap, ProcessEntry);
      while Ok do
      begin
        ProcessInfo.ProcessId := ProcessEntry.th32ProcessID;
        ProcessInfo.ParentProcessId := ProcessEntry.th32ParentProcessID;
        FCompleteList.Add(ProcessInfo);
        Ok := Process32Next(Snap, ProcessEntry);
      end;
    finally
      CloseHandle(Snap);
    end;
  finally
    FStopwatch.Start;
  end;
end;

function TProcessInfos.CompleteList: TProcessInfoList;
begin
  TryUpdate;
  Result := FCompleteList;
end;

procedure TProcessInfos.TryUpdate;
begin
  if FStopwatch.Elapsed.Seconds >= 1 then
    ForceUpdate;
end;

initialization

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GProcessInfos);
{$ENDIF NoFinalization}
end.
