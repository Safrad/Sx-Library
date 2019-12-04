unit uWindowsCPU;

interface

uses
  Winapi.Windows,
  System.Win.Registry,

  uTypes,
  uTimeSpan,
  uMainTimer,
  uCustomCPU;

type
  TNtQuerySystemInformation = function(infoClass: DWORD; buffer: Pointer; bufSize: DWORD; returnSize: PDWORD): DWORD; stdcall;

  TWindowsCPU = class(TCustomCPU)
  private
    // Temp
    FRegistry: TRegistry;

    FLastTickCountForCPUUsage: TTimeSpan;

    FNtQuerySystemInformation: TNtQuerySystemInformation;
    FOldIdleTime: LARGE_INTEGER;
    FOldSystemTime: LARGE_INTEGER;
    FInitialized: BG;
  protected
    function GetID: U4; override;
    procedure UpdateName; override;
    procedure UpdateSystemInfo; override;
    function GetDefaultFrequency: U8; override;
    function GetCPUUsage: FG; override;
    function GetCPUUsageForce: FG; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  uChar,
  uStrings,
  uPipedExternalApplication,
  uStartupWindowState,
  uLog,
  uMath,
  uOperatingSystem,
  uFiles;

type
  TCPUIDB = record
    CLFLUSH: U2;
    LogicalProcessorCount: U1;
    APICID: U1;
  end;
var
  // Result of CPUID instruction
  FCPUIDA: U4;
  FCPUIDB: TCPUIDB;
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
  mov dword ptr [FCPUIDStr+1], ebx
  mov dword ptr [FCPUIDStr+5], edx
  mov dword ptr [FCPUIDStr+9], ecx

  mov eax, 1
  xor ebx, ebx
  xor ecx, ecx
  xor edx, edx
  cpuid
  mov [FCPUIDA], eax
  mov [FCPUIDB], ebx

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
  mov FCPUIDA, eax
  mov FCPUIDB, ebx

  popad
{$endif}
end;

{ TWindowsCPU }

destructor TWindowsCPU.Destroy;
var
  CPUUsage: Integer;
begin
	if OperatingSystem.IsNT = False then
	begin
		if FRegistry <> nil then
		begin
			if FRegistry.OpenKey('PerfStats\StopStat', False) then
			begin
				FRegistry.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				FRegistry.CloseKey;
			end;

			FreeAndNil(FRegistry);
		end;
	end;

  inherited;
end;

procedure TWindowsCPU.UpdateName;
var
  ExternalApplication: TPipedExternalApplication;
  InLineIndex: SG;
  StartupWindowState: TStartupWindowState;
begin
  ExternalApplication := TPipedExternalApplication.Create;
  try
    try
      ExternalApplication.FileName := 'wmic';
      ExternalApplication.Parameters := 'cpu get name';
      ExternalApplication.CurrentDirectory := LocalAppDataDir;
      StartupWindowState.WindowState := hwsHidden;
      StartupWindowState.Active := False;
      ExternalApplication.StartupWindowState := StartupWindowState;
      ExternalApplication.RequireOutputText := True;

      ExternalApplication.Execute;
      ExternalApplication.CheckErrorCode;
      ExternalApplication.WaitFor;

      InLineIndex := 1;
      ReadToChar(ExternalApplication.OutputText, InLineIndex, CharLF);
      FName := DelBESpaceF(ReadToChar(ExternalApplication.OutputText, InLineIndex, CharLF));
      if FName = '' then
        FName := '?';
    except
      FName := '?';
    end;
  finally
    ExternalApplication.Free;
  end;
end;

procedure TWindowsCPU.UpdateSystemInfo;
var
  SystemInfo: SYSTEM_INFO;
begin
  if not FInitialized then
  begin
    FInitialized := True;

    inherited UpdateSystemInfo;
    try
      GetSystemInfo(SystemInfo);

      Assert(SystemInfo.dwPageSize = 4 * KB);
      Assert(SystemInfo.dwAllocationGranularity = 64 * KB);
      Assert(SystemInfo.wProcessorLevel = Family);
      Assert(SystemInfo.wProcessorRevision = 256 * Model + Stepping);

      FLogicalProcessorCount := SystemInfo.dwNumberOfProcessors;
      Assert(FLogicalProcessorCount = System.CPUCount);
      FPageSize := SystemInfo.dwPageSize;
      FAllocationGranularity := SystemInfo.dwAllocationGranularity;
    except
      // No code
    end;
  end;
end;

function TWindowsCPU.GetDefaultFrequency: U8;
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

function TWindowsCPU.GetID: U4;
begin
  if FCPUIDA = 0 then
    CallCPUID;
  Result := FCPUIDA;
end;

function TWindowsCPU.GetCPUUsageForce: FG;
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
var
	SysPerfInfo: TSystem_Performance_Information;
	SysTimeInfo: TSystem_Time_Information;
	status: DWORD;
	dbSystemTime: U8;
	dbIdleTime: U8;
begin
	Result := 0;

	if not Assigned(FNtQuerySystemInformation) then
  begin
    FNtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');
  	if not Assigned(FNtQuerySystemInformation) then
      Exit;
  end;

		// get new system time
	status := FNtQuerySystemInformation(SystemTimeInformation, @SysTimeInfo, SizeOf(SysTimeInfo), nil);
	if status <> 0 then Exit;

	// get new CPU's idle time
	status := FNtQuerySystemInformation(SystemPerformanceInformation, @SysPerfInfo, SizeOf(SysPerfInfo), nil);
	if status <> 0 then Exit;

	// if it's a first call - skip it
	if (FOldIdleTime.QuadPart <> 0) then
	begin

		// CurrentValue = NewValue - OldValue
		dbIdleTime := TimeDifference(SysPerfInfo.liIdleTime.QuadPart, FOldIdleTime.QuadPart);
		dbSystemTime := TimeDifference(SysTimeInfo.liKeSystemTime.QuadPart, FOldSystemTime.QuadPart);

		// CurrentCpuIdle = IdleTime / SystemTime

		// CurrentCpuUsage% = 100 - (CurrentCpuIdle * 100) / NumberOfProcessors
    if dbSystemTime = 0 then
      Result := 0
    else
    begin
      Result := 1 - (dbIdleTime / dbSystemTime) / LogicalProcessorCount;
  		Result := Range(0, Result, 1);
    end;
    MainLogAdd('GetCPUUsageForce=' + FloatToStr(Result), mlDebug);
	end;

	// store new CPU's idle and system time
	FOldIdleTime := SysPerfInfo.liIdleTime;
	FOldSystemTime := SysTimeInfo.liKeSystemTime;
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

function TWindowsCPU.GetCPUUsage: FG;
var
	Dummy: array[0..KB] of U1;
  CPUUsage: Integer;
begin
  MainLogAdd('GetCPUUsage', mlDebug);
	if OperatingSystem.IsNT then
	begin
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
	end
	else
	begin
		Result := FUsage;
		if FRegistry = nil then
		begin
			FRegistry := TRegistry.Create(KEY_QUERY_VALUE);
			FRegistry.RootKey := HKEY_DYN_DATA;
//			Reg.CreateKey('PerfStats');
			if FRegistry.OpenKeyReadOnly('PerfStats\StartStat') then
			begin
				FRegistry.ReadBinaryData('KERNEL\CPUUsage', Dummy, SizeOf(Dummy));
				FRegistry.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
        FUsage := CPUUsage / 100;
				FRegistry.CloseKey;
			end;

			if FRegistry.OpenKeyReadOnly('PerfStats\StatData') then
			begin
				FRegistry.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				Result := CPUUsage;
				FRegistry.CloseKey;
			end;
		end;

		if FRegistry.OpenKeyReadOnly('PerfStats\StatData') then
		begin
			FRegistry.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
			Result := CPUUsage;
			FRegistry.CloseKey;
		end;
	end;
end;

end.
