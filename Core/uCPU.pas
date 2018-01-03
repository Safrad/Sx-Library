unit uCPU;

interface

uses
  Windows,
  Registry,
  uTypes;

const
  CPUUsageMul = 100; // TODO : remove

type
  TPDWord = ^DWORD;
  TNtQuerySystemInformation = function(infoClass: DWORD; buffer: Pointer; bufSize: DWORD; returnSize: TPDWord): DWORD; stdcall;

  TCPU = class
  private
    // Properties
    FName: string; // from CPUID
    FFrequency: FG;
    FLastTickCount: U8;
    FLastCPUTick: U8;
    FLogicalProcessorCount: SG;
    FUsage: FG;
    FNtQuerySystemInformation: TNtQuerySystemInformation;
    liOldIdleTime: LARGE_INTEGER;
    liOldSystemTime: LARGE_INTEGER;
    Reg: TRegistry;
    LastTickCount{, LastProcessorTime}: U8;

    function GetCPUUsage: FG;
    function GetCPUUsageForce: FG;

    procedure UpdateName;
    procedure UpdateSystemInfo;

    procedure UpdateFrequency;
    procedure UpdateUsage;

    function GetFamily: SG;
    function GetModel: SG;
    function GetStepping: SG;
    procedure SetLogicalProcessorCount(const Value: SG);
    function GetDefaultFrequency: U8;
  public
    constructor Create;
    destructor Destroy; override; 

    procedure Update; // changing over time

    property Name: string read FName;
    property Family: SG read GetFamily;
    property Model: SG read GetModel;
    property Stepping: SG read GetStepping;
    property LogicalProcessorCount: SG read FLogicalProcessorCount write SetLogicalProcessorCount;
    property Frequency: FG read FFrequency; // precision 0,00041666 (0.1s/4min, 1.5s/1hod. 36sec/24hod)
    property DefaultFrequency: U8 read GetDefaultFrequency;
    property Usage: FG read FUsage; // 4 (0..10000)
  end;

var
  GCPU: TCPU;

implementation

uses
  uMath,
  uSysInfo,
  SysUtils;

var
  // Result od CPUID instruction
  FCPUID1: U4;
  FCPUID2: U4;
  FCPUIDStr: string[12] = '            ';

procedure CallCPUID;
asm
{$ifdef CPUX64}
  push rax
  push rbx
  push rcx
  push rdx
  push rdi

  xor rax, rax
  xor rbx, rbx
  xor rcx, rcx
  xor rdx, rdx
  cpuid
  mov qword ptr [FCPUIDStr+1], ebx
  mov qword ptr [FCPUIDStr+5], edx
  mov qword ptr [FCPUIDStr+9], ecx

  mov eax, 1
  xor ebx, ebx
  xor ecx, ecx
  xor edx, edx
  cpuid
  mov rdx, FCPUID1
  mov [rdx], eax
  mov rdx, FCPUID2
  mov [rdx], ebx

  pop rdi
  pop rdx
  pop rcx
  pop rbx
  pop rax
{$else}
  pushad

  xor eax, eax
  xor ebx, ebx
  xor ecx, ecx
  xor edx, edx
  dw 0a20fh // cpuid
  mov dword ptr [FCPUIDStr+1], ebx
  mov dword ptr [FCPUIDStr+5], edx
  mov dword ptr [FCPUIDStr+9], ecx

  mov eax, 1
  xor ebx, ebx
  xor ecx, ecx
  xor edx, edx
  dw 0a20fh // cpuid
  mov FCPUID1, eax
  mov FCPUID2, ebx

  popad
{$endif}
end;


{ TCPU }

constructor TCPU.Create;
begin
  inherited;

  // Static
  UpdateSystemInfo;
  CallCPUID;
  UpdateName;
  if NTSystem then
    FNtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');

  // Dynamic
  Update;
end;

procedure TCPU.UpdateFrequency;
var
	TickCount: U8;
	CPUTick: U8;
begin
  if FLastCPUTick <> 0 then
  begin
    CPUTick := TimeDifference(GetCPUCounter.A, FLastCPUTick);
    TickCount := TimeDifference(PerformanceCounter, FLastTickCount);
    if (TickCount > 0) and (CPUTick > 0) then
      FFrequency := PerformanceFrequency * CPUTick / TickCount;
  end
  else
    FFrequency := DefaultFrequency;

  FLastCPUTick := GetCPUCounter.A;
  FLastTickCount := PerformanceCounter;

  {
      if (TickCount > 0) and (CPUTick < High(Int64) div (2 * PerformanceFrequency)) then
      begin
        SysInfo.CPUFrequency := RoundDivS8(CPUTick * PerformanceFrequency, TickCount);
        SysInfo.CPUPower := RoundDivS8(4 * Count * PerformanceFrequency, TickCount);
      end
      else
      begin
        SysInfo.CPUFrequency := 0;
        SysInfo.CPUPower := 0;
      end;}
end;

procedure TCPU.Update;
begin
  UpdateFrequency;
  UpdateUsage;
end;

procedure TCPU.UpdateName;
var
  s: string;
begin
	s := 'Unknown';
	if FCPUIDStr = 'AuthenticAMD' then
	begin
		case Family of
		5:
		begin
			case Model of
			0, 1, 2, 3: s := 'K5';
			6, 7: s := 'K6';
			8: s := 'K6-II';
			9: s := 'K6-III';
			end;
		end;
		6:
		begin
			case Model of
			0, 1, 2: s := 'Athlon';
			4, 5: s := 'Thunderbird';
			else {3, 6, 7:}s := 'Duron';
			end;
		end;
		end;
		s := 'AMD ' + s;
	end
	else if FCPUIDStr = 'GenuineIntel' then
	begin
		case Family of
		0..2: s := '';
    3: s := 'i386';
		4:
		begin
			case Model of
			0: s := 'i486DX';
			3: s := 'i486DX2';
			8: s := 'i486DX4';
			end;
		end;
		5:
		begin
			case Model of
			0, 1, 2, 7: s := 'Pentium';
			4, 8: s := 'Pentium MMX';
			end;
		end;
		6:
		begin
			case Model of
			0, 1: s := 'Pentium Pro';
			3: s := 'Pentium II';
			5: s := 'Core™ i3'; //'Pentium II';
			6: s := 'Celeron';
			7: s := 'Pentium III';
			8: s := 'Pentium III E';
			9..14: s := 'Pentium 4';
			else // 15
				s := 'Dual Core';
			end;
		end;
		15:
		begin
			case Model of
			0..5: s := 'Pentium 4';
			else // 6
				s := 'Pentium(R) D CPU';
			end;
		end
		end;
		s := 'Intel ' + s;
	end
	else if FCPUIDStr = 'CyrixInstead' then
		s := 'Cyrix '
	else if FCPUIDStr = 'NexGenDriven' then
		s := 'NexGen '
	else if FCPUIDStr = 'CentaurHauls' then
		s := 'Centaur '
	else if FCPUIDStr = 'RiseRiseRise' then
		s := 'Rise '
	else if FCPUIDStr = 'UMC UMC UMC ' then
		s := 'UMC ';

  FName := s;
end;

procedure TCPU.UpdateUsage;
begin
  FUsage := GetCPUUsage;
end;

function TCPU.GetFamily: SG;
begin
  Result := FCPUID1 and $00000f00 shr 8;
end;

function TCPU.GetModel: SG;
begin
  Result := FCPUID1 and $000000f0 shr 4;
end;

function TCPU.GetStepping: SG;
begin
  Result := FCPUID1 and $000000f;
end;

procedure TCPU.SetLogicalProcessorCount(const Value: SG);
begin
  FLogicalProcessorCount := Value;
end;

procedure TCPU.UpdateSystemInfo;
var
  SystemInfo: SYSTEM_INFO;
begin
  GetSystemInfo(SystemInfo);
  FLogicalProcessorCount := SystemInfo.dwNumberOfProcessors;
end;

function TCPU.GetDefaultFrequency: U8;
var
	Reg: TRegistry;
begin
  Result := 0;
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      Result := 1000000 * U8(Reg.ReadInteger('~MHz'));
      Reg.CloseKey;
    end;
	finally
		Reg.Free;
	end;
end;


const
//	SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeInformation = 3;

type
{
	TSystem_Basic_Information = packed record
    dwUnknown1: DWORD;
    uKeMaximumIncrement: ULONG;
    uPageSize: ULONG;
		uMmNumberOfPhysicalPages: ULONG;
		uMmLowestPhysicalPage: ULONG;
		uMmHighestPhysicalPage: ULONG;
		uAllocationGranularity: ULONG;
		pLowestUserAddress: Pointer;
		pMmHighestUserAddress: Pointer;
		uKeActiveProcessors: ULONG;
		bKeNumberProcessors: byte;
		bUnknown2: byte;
		wUnknown3: word;
	end;}

	TSystem_Performance_Information = packed record
		liIdleTime: LARGE_INTEGER;
		dwSpare: array[0..75] of DWORD;
	end;

type
	TSystem_Time_Information = packed record
		liKeBootTime: LARGE_INTEGER;
		liKeSystemTime: LARGE_INTEGER;
		liExpTimeZoneBias: LARGE_INTEGER;
		uCurrentTimeZoneId: ULONG;
		dwReserved: DWORD;
	end;

function TCPU.GetCPUUsageForce: FG;
var
  SystemInfo: SYSTEM_INFO;
	SysPerfInfo: TSystem_Performance_Information;
	SysTimeInfo: TSystem_Time_Information;
	status: DWORD;
	dbSystemTime: U8;
	dbIdleTime: U8;
//	s: string;
begin
	Result := 0;

	if not Assigned(FNtQuerySystemInformation) then
    Exit;

  GetSystemInfo(SystemInfo);

		// get new system time
	status := FNtQuerySystemInformation(SystemTimeInformation, @SysTimeInfo, SizeOf(SysTimeInfo), nil);
	if status <> 0 then Exit;

	// get new CPU's idle time
	status := FNtQuerySystemInformation(SystemPerformanceInformation, @SysPerfInfo, SizeOf(SysPerfInfo), nil);
	if status <> 0 then Exit;

	// if it's a first call - skip it
	if (liOldIdleTime.QuadPart <> 0) then
	begin

		// CurrentValue = NewValue - OldValue
		dbIdleTime := SysPerfInfo.liIdleTime.QuadPart - liOldIdleTime.QuadPart;
		dbSystemTime := SysTimeInfo.liKeSystemTime.QuadPart - liOldSystemTime.QuadPart;

		// CurrentCpuIdle = IdleTime / SystemTime

		// CurrentCpuUsage% = 100 - (CurrentCpuIdle * 100) / NumberOfProcessors
    if dbSystemTime = 0 then
      Result := 0
    else
    begin
    		Result := Round(CPUUsageMul * (100.0 - (dbIdleTime / dbSystemTime) * 100.0 / SystemInfo.dwNumberOfProcessors));
  		Result := Range(0, Result, 100 * CPUUsageMul);
    end;

		// Show Percentage
//		Result := RoundN(100 * dbIdleTime);
	end;

		// store new CPU's idle and system time
		liOldIdleTime := SysPerfInfo.liIdleTime;
		liOldSystemTime := SysTimeInfo.liKeSystemTime;
end;

(*
function GetProcessorTime : int64;
type
	TPerfDataBlock = packed record
		signature              : array [0..3] of wchar;
		littleEndian           : U4;
		version                : U4;
		revision               : U4;
		totalByteLength        : U4;
		headerLength           : U4;
		numObjectTypes         : S4;
		defaultObject          : U4;
		systemTime             : TSystemTime;
		perfTime               : S8;
		perfFreq               : S8;
		perfTime100nSec        : S8;
		systemNameLength       : U4;
		systemnameOffset       : U4;
	end;
	TPerfObjectType = packed record
		totalByteLength        : U4;
		definitionLength       : U4;
		headerLength           : U4;
		objectNameTitleIndex   : U4;
		objectNameTitle        : PWideChar;
		objectHelpTitleIndex   : U4;
		objectHelpTitle        : PWideChar;
		detailLevel            : U4;
		numCounters            : S4;
		defaultCounter         : S4;
		numInstances           : S4;
		codePage               : U4;
		perfTime               : S8;
		perfFreq               : S8;
	end;
	TPerfCounterDefinition = packed record
		byteLength             : U4;
		counterNameTitleIndex  : U4;
		counterNameTitle       : PWideChar;
		counterHelpTitleIndex  : U4;
		counterHelpTitle       : PWideChar;
		defaultScale           : S4;
		defaultLevel           : U4;
		counterType            : U4;
		counterSize            : U4;
		counterOffset          : U4;
	end;
	TPerfInstanceDefinition = packed record
		byteLength             : U4;
		parentObjectTitleIndex : U4;
		parentObjectInstance   : U4;
		uniqueID               : S4;
		nameOffset             : U4;
		nameLength             : U4;
	end;
var
	c1, c2, c3      : U4;
	i1, i2          : S4;
	perfDataBlock   : ^TPerfDataBlock;
	perfObjectType  : ^TPerfObjectType;
	perfCounterDef  : ^TPerfCounterDefinition;
	perfInstanceDef : ^TPerfInstanceDefinition;
begin
	result := 0;
	perfDataBlock := nil;
	try
		c1 := $10000;
		while True do 
		begin
			ReallocMem(perfDataBlock, c1);
			c2 := c1;
			case RegQueryValueEx(HKEY_PERFORMANCE_DATA, '238'{'Processor/Processor Time'}, nil, @c3, Pointer(perfDataBlock), @c2) of
			ERROR_MORE_DATA: c1 := c1 * 2;
			ERROR_SUCCESS: Break;
			else Exit;
			end;
		end;
		perfObjectType := Pointer(UG(perfDataBlock) + perfDataBlock^.headerLength);
		for i1 := 0 to perfDataBlock^.numObjectTypes - 1 do
		begin
			if perfObjectType^.objectNameTitleIndex = 238 then
			begin   // 238 -> "Processor"
				perfCounterDef := Pointer(UG(perfObjectType) + perfObjectType^.headerLength);
				for i2 := 0 to perfObjectType^.numCounters - 1 do
				begin
					if perfCounterDef^.counterNameTitleIndex = 6 then
					begin    // 6 -> "% Processor Time"
						perfInstanceDef := Pointer(UG(perfObjectType) + perfObjectType^.definitionLength);
						result := PS8(UG(perfInstanceDef) + perfInstanceDef^.byteLength + perfCounterDef^.counterOffset)^;
						break;
					end;
					inc(perfCounterDef);
				end;
				break;
			end;
			perfObjectType := Pointer(UG(perfObjectType) + perfObjectType^.totalByteLength);
		end;
	finally
		FreeMem(perfDataBlock);
	end;
end;
*)

function TCPU.GetCPUUsage: FG;
const
	IntervalTime = Second div 2;
var
	tickCount     : U8;
//	processorTime : U8;
	Dummy: array[0..KB] of U1;
  CPUUsage: Integer;
begin
	if NTSystem then
	begin
//		tickCount := GetTickCount;
		tickCount := PerformanceCounter;
		if tickCount < LastTickCount then
		begin
			// Possible after hibernation or overflow
			LastTickCount := tickCount;
		end;
		if tickCount < LastTickCount + IntervalTime then
		begin
			Result := FUsage;
			Exit;
		end;
//		processorTime := GetProcessorTime;

		if {(LastTickCount <> 0) and} (tickCount > LastTickCount) {and (processorTime >= LastProcessorTime)} then
		begin // 1 000 * 10 000 = 10 000 000 / sec
(*			CPUUsage := 100 * CPUUsageMul - RoundDivS8(PerformanceFrequency * (processorTime - LastProcessorTime), 1000 * (tickCount - LastTickCount){ + 1}) ;
			CPUUsage := Range(0, CPUUsage, 100 * CPUUsageMul);}*)
			FUsage := GetCPUUsageForce;
		end;

		Result := FUsage;

		LastTickCount := tickCount;
//		LastProcessorTime := processorTime;
	end
	else
	begin
		Result := FUsage;
		if Reg = nil then
		begin
			Reg := TRegistry.Create(KEY_QUERY_VALUE);
			Reg.RootKey := HKEY_DYN_DATA;
//			Reg.CreateKey('PerfStats');
			if Reg.OpenKeyReadOnly('PerfStats\StartStat') then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', Dummy, SizeOf(Dummy));
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
        FUsage := CPUUsage / 100;
				Reg.CloseKey;
			end;

			if Reg.OpenKeyReadOnly('PerfStats\StatData') then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				Result := CPUUsageMul * CPUUsage;
				Reg.CloseKey;
			end;
		end;

		if Reg.OpenKeyReadOnly('PerfStats\StatData') then
		begin
			Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
			Result := CPUUsageMul * CPUUsage;
			Reg.CloseKey;
		end;
	end;
end;

destructor TCPU.Destroy;
var
  CPUUsage: Integer;
begin
	if NTSystem = False then
	begin
		if Reg <> nil then
		begin
			if Reg.OpenKey('PerfStats\StopStat', False) then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				Reg.CloseKey;
			end;

			FreeAndNil(Reg);
		end;
	end;

  inherited;
end;

initialization
  GCPU := TCPU.Create;
finalization
  FreeAndNil(GCPU);
end.
