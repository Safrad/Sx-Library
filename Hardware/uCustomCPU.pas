unit uCustomCPU;

interface

uses
  uTypes,
  uTimeSpan,
  uMainTimer;

type
  TCustomCPU = class
  private
    FLastTickCountForFrequency: U8;

    FCPUTimer: TMainTimer;
    FLastTickCountForCPUUsage: TTimeSpan;
    FLastCPUTick: U8;

     // Properties
    FFrequency: FG;

    procedure UpdateFrequency;
    procedure UpdateUsage;

    function GetFamily: SG;
    function GetModel: SG;
    function GetStepping: SG;
    function GetName: string;
    function GetLogicalProcessorCount: SG;
    function GetPageSize: SG;
    function GetAllocationGranularity: SG;
  protected
    // Properties
    FName: string;
    FLogicalProcessorCount: SG;
    FPageSize: SG;
    FAllocationGranularity: SG;
    FUsage: FG;

    function GetID: U4; virtual;
    procedure UpdateName; virtual;
    procedure UpdateSystemInfo; virtual;
    function GetCPUUsageForce: FG; virtual;
    function GetCPUUsage: FG; virtual;
    function GetDefaultFrequency: U8; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Update; // changing over time

    property Name: string read GetName;
    property ID: U4 read GetID;
    property Family: SG read GetFamily;
    property Model: SG read GetModel;
    property Stepping: SG read GetStepping;
    property LogicalProcessorCount: SG read GetLogicalProcessorCount;
    property PageSize: SG read GetPageSize;
    property AllocationGranularity: SG read GetAllocationGranularity;
    property Frequency: FG read FFrequency; // precision 0,00041666 (0.1s/4min, 1.5s/1hod. 36sec/24hod)
    property DefaultFrequency: U8 read GetDefaultFrequency;
    property Usage: FG read FUsage; // 0..1
  end;

implementation

uses
  uMainLog;

{ TCustomCPU }

constructor TCustomCPU.Create;
begin
  inherited;

  FCPUTimer := TMainTimer.Create;
  FCPUTimer.MeasureType := mtTSC;
end;

destructor TCustomCPU.Destroy;
begin

  FCPUTimer.Free;
  inherited;
end;

procedure TCustomCPU.UpdateFrequency;
var
	TickCount: U8;
	CPUTick: U8;
begin
  // alternative static in MHz: wmic cpu get CurrentClockSpeed
  if FLastCPUTick <> 0 then
  begin
    CPUTick := FCPUTimer.IntervalFrom(FLastCPUTick);
    TickCount := MainTimer.IntervalFrom(FLastTickCountForFrequency);
    if (TickCount > 0) and (CPUTick > 0) then
      FFrequency := MainTimer.Frequency * CPUTick / TickCount;
  end
  else
    FFrequency := DefaultFrequency;

  FLastCPUTick := FCPUTimer.Value.Ticks;
  FLastTickCountForFrequency := MainTimer.Value.Ticks;
end;

procedure TCustomCPU.Update;
begin
  UpdateFrequency;
  UpdateUsage;
end;

procedure TCustomCPU.UpdateName;
begin
  FName := '?';
end;

procedure TCustomCPU.UpdateSystemInfo;
begin
  FLogicalProcessorCount := System.CPUCount;
  FPageSize := 4 * KB;
  FAllocationGranularity := 64 * KB;
end;

procedure TCustomCPU.UpdateUsage;
begin
  FUsage := GetCPUUsage;
end;

function TCustomCPU.GetFamily: SG;
begin
  Result := (GetID shr 8 and $0000000f) or (GetID shr 16 and $000000f0);
end;

function TCustomCPU.GetID: U4;
begin
  Result := 0;
end;

function TCustomCPU.GetModel: SG;
begin
  Result := (GetID shr 4 and $0000000f) or (GetID shr 12 and $000000f0);
end;

function TCustomCPU.GetStepping: SG;
begin
  Result := GetID and $000000f;
end;

function TCustomCPU.GetAllocationGranularity: SG;
begin
  UpdateSystemInfo;
  Result := FAllocationGranularity;
end;

function TCustomCPU.GetCPUUsage: FG;
begin
  if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('GetCPUUsage', mlDebug);
  if MainTimer.IntervalFrom(FLastTickCountForCPUUsage).Milliseconds < 500 then
  begin
    // Use cached value
    Result := FUsage;
  end
  else
  begin
    // Calculate new value
    FUsage := GetCPUUsageForce;
    Result := FUsage;

    FLastTickCountForCPUUsage := MainTimer.Value;
  end;
end;

function TCustomCPU.GetCPUUsageForce: FG;
begin
  Result := 0;
end;

function TCustomCPU.GetDefaultFrequency: U8;
begin
  Result := 0;
end;

function TCustomCPU.GetName: string;
begin
  if FName = '' then
    UpdateName;
  Result := FName;
end;

function TCustomCPU.GetPageSize: SG;
begin
  UpdateSystemInfo;
  Result := FPageSize;
end;

function TCustomCPU.GetLogicalProcessorCount: SG;
begin
  UpdateSystemInfo;
  Result := FLogicalProcessorCount;
end;

end.
