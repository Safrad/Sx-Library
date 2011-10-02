unit uSysInfo;

interface

uses
	uTypes, uMath,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDForm, uDEdit;

const
	CPUUsageMul = 100;
const
	CPUStrOffset = 4 + 4 + 1;
type
  {$if CompilerVersion < 20}
  DWORDLONG = S8;
  PMemoryStatusEx = ^TMemoryStatusEx;
  LPMEMORYSTATUSEX = PMemoryStatusEx;
  {$EXTERNALSYM LPMEMORYSTATUSEX}
  _MEMORYSTATUSEX = packed record
    dwLength : DWORD;
    dwMemoryLoad : DWORD;
    ullTotalPhys : DWORDLONG;
    ullAvailPhys : DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual : DWORDLONG;
    ullAvailVirtual : DWORDLONG;
    ullAvailExtendedVirtual : DWORDLONG;
  end;
  {$EXTERNALSYM _MEMORYSTATUSEX}
  TMemoryStatusEx = _MEMORYSTATUSEX;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  {$EXTERNALSYM MEMORYSTATUSEX}
  {$ifend}

	PSysInfo = ^TSysInfo;
	TSysInfo = packed record // 256
		CPU: U4;
		CPU2: U4;
		CPUStr: string[12]; // 13
		LogicalProcessorCount: SG;
		Reserved0: array[0..2] of S1; // 3
		CPUFrequency: U8; // precision 0,00041666 (0.1s/4min, 1.5s/1hod. 36sec/24hod)
		CPUPower: U8;
		PerformanceFrequency: U4;
//		DiskFree, DiskTotal: U8; // 16
//		Reserved: array[0..3] of U4; // 16
		CPUUsage: S4; // 4 (0..10000)
		MS: TMemoryStatusEx; // 8 * 8 = 64
		OS: TOSVersionInfo; // 148
//		ProgramVersion: string[15]; // 10.32.101.10000
//		Graph: string[127]; // 128
//		Reserved1: array[0..15] of U1; // 16
	end;

type
	TfSysInfo = class(TDForm)
		Bevel1: TBevel;
		LabelTOperatingSystem: TLabel;
		EditOS: TDEdit;
		Bevel4: TBevel;
		LabelUsed: TLabel;
		LabelFree: TLabel;
		LabelTotal: TLabel;
		edMT: TDEdit;
		edMF: TDEdit;
		edFF: TDEdit;
		edFT: TDEdit;
		edMU: TDEdit;
		edFU: TDEdit;
		LabelTPhysicalMemory: TLabel;
		LabelTPageFile: TLabel;
		Bevel3: TBevel;
		Bevel2: TBevel;
		DLabel3: TLabel;
		Bevel5: TBevel;
		EditCPU: TDEdit;
		ButtonOk: TDButton;
		DLabelCPUFrequency: TLabel;
		EditCPUFrequency: TDEdit;
		LabelAMDDuronCmp: TLabel;
		EditDuron: TDEdit;
		DLabelCPUUsage: TLabel;
		EditCPUUsage: TDEdit;
		EditCounter: TDEdit;
		LabelMBoardCounter: TLabel;
		ComboBoxSize: TComboBox;
		procedure ButtonOkClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure FillComp(SysInfo: PSysInfo);
	end;

var
	GSysInfo: TSysInfo;
	NTSystem: Boolean;
	RegionCompatibily: Boolean;
	fSysInfo: TfSysInfo;

function GetKey(Default: U2): U2;
function OSToStr(const OS: TOSVersionInfo): string;
function GetCPUUsage: SG;
procedure FillDynamicInfo(var SysInfo: TSysInfo); // FillMemoryStatus + FillCPUTest
procedure FillMemoryStatus(var SysInfo: TSysInfo);
procedure FillCPUTest(var SysInfo: TSysInfo);
procedure DisplaySysInfo(SysInfo: PSysInfo; const AOwner: TComponent = nil);
procedure UpdateSysInfo(SysInfo: PSysInfo);

implementation

{$R *.DFM}
uses
  uMsg,
	uGraph, uScreen, uStrings, uOutputFormat, uSimulation, uDictionary,
	uProjectInfo,
	Registry, Math;

{$if CompilerVersion < 20}
procedure GlobalMemoryStatus(var lpBuffer: TMemoryStatus); stdcall;
  external kernel32;
{$EXTERNALSYM GlobalMemoryStatus}

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall;
type
  TFNGlobalMemoryStatusEx = function(var msx: TMemoryStatusEx): BOOL; stdcall;
var
  FNGlobalMemoryStatusEx: TFNGlobalMemoryStatusEx;
begin
  FNGlobalMemoryStatusEx := TFNGlobalMemoryStatusEx(
    GetProcAddress(GetModuleHandle(kernel32), 'GlobalMemoryStatusEx'));
  if not Assigned(FNGlobalMemoryStatusEx) then
  begin
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result := False;
  end
  else
  begin
    Result := FNGlobalMemoryStatusEx(lpBuffer);
		ErrorMsg(GetLastError);
  end;
end;
{$ifend}

function GetKey(Default: U2): U2;
var
	Keyboard: TKeyboardState;
	i: Integer;
begin
	while True do
	begin
		Application.ProcessMessages;
		GetKeyboardState(Keyboard);
		for i := Low(Keyboard) to High(Keyboard) do
		begin
			if (Keyboard[i] and $80 <> 0) and (i <> VK_LBUTTON) and (i <> VK_RBUTTON)
			and (i <> VK_MBUTTON) then
			begin
				Result := i;
				if (not NTSystem) or (not (i in [VK_SHIFT, VK_CONTROL, VK_MENU])) then Exit;
			end;
		end;
	end;
end;

function OSToStr(const OS: TOSVersionInfo): string;
{$ifndef UNICODE}
const
	VER_PLATFORM_WIN32_CE = 3;
{$endif}
var S: string;
begin
	case OS.dwPlatformId of
	VER_PLATFORM_WIN32s: // 0
		S := 'Win32';
	VER_PLATFORM_WIN32_WINDOWS: // 1
	begin
		S := 'Windows ';
		if (OS.dwMajorVersion < 4) or ((OS.dwMajorVersion = 4) and (OS.dwMinorVersion < 10)) then
			S := S + '95'
		else
			S := S + '98';
	end;
	VER_PLATFORM_WIN32_NT: // 2
	begin
		S := 'Windows ';
		if OS.dwMajorVersion < 5 then
			S := S + 'NT'
		else if OS.dwMajorVersion < 6 then
		begin
			if OS.dwMinorVersion = 0 then
				S := S + '2000'
			else
				S := S + 'XP';
		end
		else
		begin
			if OS.dwMinorVersion = 0 then
				S := S + 'Vista'
			else
				S := S + '7';
		end;
	end;
	VER_PLATFORM_WIN32_CE: // 3
		S := 'Windows CE'
	else // 3
		S := 'Unknown System ' + IntToStr(OS.dwPlatformId - VER_PLATFORM_WIN32_NT);
	end;
	S := S + ' (Build ' +
		IntToStr(OS.dwMajorVersion) + '.' +
		IntToStr(OS.dwMinorVersion) + '.' +
		IntToStr(LoWord(OS.dwBuildNumber)) + ' ' +
		OS.szCSDVersion + ')';
	Result := S;
end;

procedure FillMemoryStatus(var SysInfo: TSysInfo);
begin
	SysInfo.MS.dwLength := SizeOf(SysInfo.MS);
	GlobalMemoryStatusEx(SysInfo.MS);
end;

const
	SystemBasicInformation = 0;
  SystemPerformanceInformation = 2;
  SystemTimeInformation = 3;

type
  TPDWord = ^DWORD;

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
	end;

type
	TSystem_Performance_Information = packed record
		liIdleTime: LARGE_INTEGER; {LARGE_INTEGER}
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
	NtQuerySystemInformation: function(infoClass: DWORD;
		buffer: Pointer;
		bufSize: DWORD;
		returnSize: TPDword): DWORD; stdcall = nil;


	liOldIdleTime: LARGE_INTEGER = ();
	liOldSystemTime: LARGE_INTEGER = ();

{function Li2Double(x: LARGE_INTEGER): Double;
begin
	Result := x.HighPart * 4.294967296E9 + x.LowPart
end;}

function GetLogicalProcessorCount: SG;
var
	SysBaseInfo: TSystem_Basic_Information;
	status: DWORD;
begin
	if @NtQuerySystemInformation = nil then
		NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'),
			'NtQuerySystemInformation');

	// get number of processors in the system

	Result := 1;

  // TODO : x64 DNW
	status := NtQuerySystemInformation(SystemBasicInformation, @SysBaseInfo, SizeOf(SysBaseInfo), nil);
	if status <> 0 then Exit;

	Result := SysBaseInfo.bKeNumberProcessors;
end;

var
	CPUUsage: SG;

function GetCPUUsageForce: SG;
var
	SysBaseInfo: TSystem_Basic_Information;
	SysPerfInfo: TSystem_Performance_Information;
	SysTimeInfo: TSystem_Time_Information;
	status: DWORD;
	dbSystemTime: U8;
	dbIdleTime: U8;
//	s: string;
begin
	Result := 0;
	if @NtQuerySystemInformation = nil then
		NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'),
			'NtQuerySystemInformation');

	// get number of processors in the system

	status := NtQuerySystemInformation(SystemBasicInformation, @SysBaseInfo, SizeOf(SysBaseInfo), nil);
	if status <> 0 then Exit;

	// Show some information
{	with SysBaseInfo do
	begin
			s :=
			Format('uKeMaximumIncrement: %d'#13'uPageSize: %d'#13+
			'uMmNumberOfPhysicalPages: %d'+#13+'uMmLowestPhysicalPage: %d'+#13+
			'uMmHighestPhysicalPage: %d'+#13+'uAllocationGranularity: %d'#13+
			'uKeActiveProcessors: %d'#13'bKeNumberProcessors: %d',
			[uKeMaximumIncrement, uPageSize, uMmNumberOfPhysicalPages,
			uMmLowestPhysicalPage, uMmHighestPhysicalPage, uAllocationGranularity,
			uKeActiveProcessors, bKeNumberProcessors]);
	end;}

		// get new system time
	status := NtQuerySystemInformation(SystemTimeInformation, @SysTimeInfo, SizeOf(SysTimeInfo), nil);
	if status <> 0 then Exit;

	// get new CPU's idle time
	status := NtQuerySystemInformation(SystemPerformanceInformation, @SysPerfInfo, SizeOf(SysPerfInfo), nil);
	if status <> 0 then Exit;

	// if it's a first call - skip it
	if (liOldIdleTime.QuadPart <> 0) then
	begin

		// CurrentValue = NewValue - OldValue
		dbIdleTime := SysPerfInfo.liIdleTime.QuadPart - liOldIdleTime.QuadPart;
		dbSystemTime := SysTimeInfo.liKeSystemTime.QuadPart - liOldSystemTime.QuadPart;

		// CurrentCpuIdle = IdleTime / SystemTime

		// CurrentCpuUsage% = 100 - (CurrentCpuIdle * 100) / NumberOfProcessors
		Result := Round(CPUUsageMul * (100.0 - (dbIdleTime / dbSystemTime) * 100.0 / SysBaseInfo.bKeNumberProcessors));
		Result := Range(0, Result, 100 * CPUUsageMul);

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
var
	Reg: TRegistry;
	LastTickCount{, LastProcessorTime}: U8;

function GetCPUUsage: SG;
const
	IntervalTime = Second div 2;
var
	tickCount     : U8;
//	processorTime : U8;
	Dummy: array[0..KB] of U1;
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
			Result := CPUUsage;
			Exit;
		end;
//		processorTime := GetProcessorTime;

		if {(LastTickCount <> 0) and} (tickCount > LastTickCount) {and (processorTime >= LastProcessorTime)} then
		begin // 1 000 * 10 000 = 10 000 000 / sec
(*			CPUUsage := 100 * CPUUsageMul - RoundDivS8(PerformanceFrequency * (processorTime - LastProcessorTime), 1000 * (tickCount - LastTickCount){ + 1}) ;
			CPUUsage := Range(0, CPUUsage, 100 * CPUUsageMul);}*)
			CPUUsage := GetCPUUsageForce;
		end;

		Result := CPUUsage;

		LastTickCount     := tickCount;
//		LastProcessorTime := processorTime;
	end
	else
	begin
		Result := CPUUsage;
		if Reg = nil then
		begin
			Reg := TRegistry.Create(KEY_QUERY_VALUE);
			Reg.RootKey := HKEY_DYN_DATA;
//			Reg.CreateKey('PerfStats');
			if Reg.OpenKeyReadOnly('PerfStats\StartStat') then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', Dummy, SizeOf(Dummy));
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
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

function IsCPUID_Available : Boolean; register;
{$ifdef CPUX64}
begin
  Result := False;
{$else}
const
	ID_BIT	=	$200000;			// EFLAGS ID bit
asm
	PUSHFD							{direct access to flags no possible, only via stack}
	POP     EAX					{flags to EAX}
	MOV     EDX,EAX			{save current flags}
	XOR     EAX,ID_BIT	{not ID bit}
	PUSH    EAX					{onto stack}
	POPFD								{from stack to flags, with not ID bit}
	PUSHFD							{back to stack}
	POP     EAX					{get back to EAX}
	XOR     EAX,EDX			{check if ID bit affected}
	JZ      @exit				{no, CPUID not availavle}
	MOV     AL,True			{Result=True}
@exit:
{$endif}
end;

procedure FillCPUID(var SysInfo: TSysInfo);
begin
	SysInfo.CPUStr := '            '; //StringOfChar(CharSpace, 12);
	try
{$ifdef CPUX64}
{$else}
		asm
		pushad
		xor eax, eax
		xor ebx, ebx
		xor ecx, ecx
		xor edx, edx
		dw 0a20fh // cpuid
		mov eax, SysInfo
		add eax, CPUStrOffset
		mov [eax], ebx
		mov [eax+4], edx
		mov [eax+8], ecx

		mov eax, 1
		xor ebx, ebx
		xor ecx, ecx
		xor edx, edx
		dw 0a20fh // cpuid

		mov edx, SysInfo
		mov [edx], eax
		mov [edx+4], ebx
		popad
		end;
{$endif}
	finally
//		SysInfo.LogicalProcessorCount := Max(1, Lo(SysInfo.CPU2 shr 16));
		SysInfo.LogicalProcessorCount := GetLogicalProcessorCount;
	end;
end;

var
	CPUException: Boolean; // Cyrix

procedure FillCPUTest(var SysInfo: TSysInfo);
var
	TickCount: U8;
	CPUTick: U8;
const
	Count = 1 shl 22;
var
	MaxMem4, MaxMem: UG;
	PMem: Pointer;
begin
	if IsCPUID_Available and (CPUException = False) then
	begin
		if Assigned(fSysInfo) then
			MaxMem4 :=  (1 shl Max(0, fSysInfo.ComboBoxSize.ItemIndex)){14 for Duron} - 1 {10 = 4kB; 14=64kB}
		else
			MaxMem4 :=  1 shl 14 - 1; {10 = 4kB; 14 = 64kB}
		MaxMem := 4 * (MaxMem4 + 1) - 1;
		GetMem(PMem, MaxMem + 1);
		try
			SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
			try
				TickCount := PerformanceCounter;
				CPUTick := GetCPUCounter.A;
{$ifdef CPUX64}
{$else}
				asm
				pushad

	{     mov ecx, 999 // 1M

				@Loop:
					mov edi, U4 ptr PMem
					mov esi, U4 ptr PMem2
					push ecx
					mov ecx, 32768
					shr ecx, 2
					cld
						rep movsd
					pop ecx
					sub ecx, 1
				jnz @Loop}
	(*
				mov ecx, 999998 // 1M
	//      mov edi, U4 ptr PMem
				@Loop: // 3 - Duron, 4 - P4
					mov esi, edi
					mov ebx, ecx
					and ebx, 32767
					add esi, ebx
	//        mov [esi], cl
					sub ecx, 1
				jnz @Loop*)

				mov ecx, Count - 1 // 1M
				mov edi, PMem
				@Loop: // 4 clocks
					mov eax, ecx
					mov esi, edi
					and eax, MaxMem4
					sub ecx, 1
					mov [esi+4*eax], ebx
				jnz @Loop

				popad
				end;
{$endif}
				CPUTick := GetCPUCounter.A - CPUTick;
				TickCount := PerformanceCounter - TickCount;
				if TickCount > 0 then
				begin
					SysInfo.CPUFrequency := RoundDivS8(CPUTick * PerformanceFrequency, TickCount);
					SysInfo.CPUPower := RoundDivS8(4 * Count * PerformanceFrequency, TickCount);
				end
				else
				begin
					SysInfo.CPUFrequency := 0;
					SysInfo.CPUPower := 0;
				end;
			except
				CPUException := True;
				SysInfo.CPUFrequency := 0;
				SysInfo.CPUPower := 0;
			end;
		finally
			SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
			FreeMem(PMem);
		end;
	end;
end;
(*
procedure FillOS(var SysInfo: TSysInfo);
var VersionInfo: TrpVersionInfo;
begin
{	SysInfo.OS.dwOSVersionInfoSize := SizeOf(SysInfo.OS);
	GetVersionEx(SysInfo.OS);}
	SysInfo.OS := GSysInfo.OS;

{	if DriverDesc = '' then
	begin
		DriverDesc := 'Not Available';
		ReadScreenModes;
	end;
	SInfo.Graph := DriverDesc;}
end; *)

procedure FillDynamicInfo(var SysInfo: TSysInfo);
begin
	FillMemoryStatus(SysInfo);
	SysInfo.CPUUsage := GetCPUUsage;
	FillCPUTest(SysInfo);
end;

{
function GetCpuSpeed: string;
var
	Reg: TRegistry;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
try
	Reg.RootKey := HKEY_LOCAL_MACHINE;
	if Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0', False) then
	begin
		Result := IntToStr(Reg.ReadInteger('~MHz')) + ' MHz';
		Reg.CloseKey;
	end;
	finally
		Reg.Free;
	end;
end;
}

procedure TfSysInfo.FillComp(SysInfo: PSysInfo);
var
	s: string;
	Family, Model: SG;
begin
	EditOS.Text := OSToStr(SysInfo.OS);

	Family := SysInfo.CPU and $00000f00 shr 8;
	Model := SysInfo.CPU and $000000f0 shr 4;

	s := 'Unknown';
	if SysInfo.CPUStr = 'AuthenticAMD' then
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
	else if SysInfo.CPUStr = 'GenuineIntel' then
	begin
		case Family of
		0..3: s := '';
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
	else if SysInfo.CPUStr = 'CyrixInstead' then
		s := 'Cyrix '
	else if SysInfo.CPUStr = 'NexGenDriven' then
		s := 'NexGen '
	else if SysInfo.CPUStr = 'CentaurHauls' then
		s := 'Centaur '
	else if SysInfo.CPUStr = 'RiseRiseRise' then
		s := 'Rise '
	else if SysInfo.CPUStr = 'UMC UMC UMC ' then
		s := 'UMC ';

	if s <> '' then
		s := s + ListSeparator;
	s := s + 'Family: ' + NToS(Family) + ListSeparator;
	s := s + 'Model: ' + NToS(Model) + ListSeparator;
	s := s + 'Stepping: ' + NToS(SysInfo.CPU and $000000f) + ListSeparator;
	s := s + 'Cores: ' + NToS(SysInfo.LogicalProcessorCount);
	EditCPU.Text := s;

	EditCPUUsage.Text := NToS(SysInfo.CPUUsage, 2) + '%';
	EditCPUFrequency.Text := NToS(SysInfo.CPUFrequency) + ' Hz';
	EditDuron.Text := NToS(SysInfo.CPUPower) + ' Hz';
	EditCounter.Text := NToS(SysInfo.PerformanceFrequency) + ' Hz';

	edMU.Text := BToStr(SysInfo.MS.ullTotalPhys - SysInfo.MS.ullAvailPhys);
	edMF.Text := BToStr(SysInfo.MS.ullAvailPhys);
	edMT.Text := BToStr(SysInfo.MS.ullTotalPhys);

	edFU.Text := BToStr(SysInfo.MS.ullTotalPageFile - SysInfo.MS.ullAvailPageFile);
	edFF.Text := BToStr(SysInfo.MS.ullAvailPageFile);
	edFT.Text := BToStr(SysInfo.MS.ullTotalPageFile);
end;

procedure TfSysInfo.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfSysInfo.FormCreate(Sender: TObject);
var
	i: SG;
	m: UG;
begin
	Background := baGradient;

	ComboBoxSize.Items.BeginUpdate;
	try
		for i := 2 to 29 do
		begin
			m := 1 shl i;
			if m >= GSysInfo.MS.ullAvailPhys div 2 then Break;
			ComboBoxSize.Items.Add(BToStr(m));
		end;
		ComboBoxSize.ItemIndex := 14;
	finally
		ComboBoxSize.Items.EndUpdate;
	end;
end;

procedure Init;
begin
	GSysInfo.OS.dwOSVersionInfoSize := SizeOf(GSysInfo.OS);
	GetVersionEx(GSysInfo.OS);
	NTSystem := GSysInfo.OS.dwMajorVersion >= 5;
	RegionCompatibily := not ((GSysInfo.OS.dwMajorVersion < 4) or ((GSysInfo.OS.dwMajorVersion = 4) and (GSysInfo.OS.dwMinorVersion < 10)));

	InitPerformanceCounter;
	FillCPUID(GSysInfo);
{	PerformanceType := ptCPU;
	FillCPUTest(GSysInfo);
	PerformanceFrequency := GSysInfo.CPUFrequency;}

	GSysInfo.PerformanceFrequency := PerformanceFrequency;

//	GSysInfo.ProgramVersion := GetProjectInfo(piProductVersion);

	CPUUsage := 0 * CPUUsageMul;
	GetCPUUsage;
end;

procedure DisplaySysInfo(SysInfo: PSysInfo; const AOwner: TComponent = nil);
begin
	if not Assigned(fSysInfo) then
		fSysInfo := TfSysInfo.Create(AOwner);
	fSysInfo.FillComp(SysInfo);
	fSysInfo.ShowModal;
end;

procedure UpdateSysInfo(SysInfo: PSysInfo);
begin
	if FormDraw(fSysInfo) then
		fSysInfo.FillComp(SysInfo);
end;

initialization
	Init;
finalization
//	FreeAndNil(fSysInfo);
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
end.
