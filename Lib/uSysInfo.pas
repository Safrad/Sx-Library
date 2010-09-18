//* File:     Lib\uSysInfo.pas
//* Created:  2000-07-01
//* Modified: 2004-09-02
//* Version:  X.X.32.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uSysInfo;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDForm;

type
	TfSysInfo = class(TDForm)
		Bevel1: TBevel;
		LabelTOperatingSystem: TDLabel;
		EditOS: TEdit;
		Bevel4: TBevel;
		LabelUsed: TDLabel;
		LabelFree: TDLabel;
		LabelTotal: TDLabel;
    PMT: TDLabel;
    PMF: TDLabel;
    PFF: TDLabel;
    PFT: TDLabel;
    PMU: TDLabel;
    PFU: TDLabel;
		LabelTPhysicalMemory: TDLabel;
		LabelTPageFile: TDLabel;
		Bevel3: TBevel;
		Bevel2: TBevel;
		DLabel3: TDLabel;
		DLabel5: TDLabel;
		Bevel5: TBevel;
		EditCPU: TEdit;
		EditDiskU: TDLabel;
		EditDiskF: TDLabel;
		EditDiskT: TDLabel;
		Bevel6: TBevel;
		ButtonOk: TDButton;
		DLabelCPUFrequency: TDLabel;
		EditCPUFrequency: TEdit;
		DLabel2: TDLabel;
		EditDuron: TEdit;
		DLabelCPUUsage: TDLabel;
		EditCPUUsage: TEdit;
		procedure ButtonOkClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure FillComp;
//    procedure MemoryStatus;
	end;

var
	fSysInfo: TfSysInfo;

const
	CPUUsageMul = 100;
type
	TSysInfo = packed record // 256
		CPU: U4;
		CPUStr: string[12]; // 13
		Reserved0: array[0..6] of S1; // 7
		CPUFrequency: U8; // precision 0,00041666 (0.1s/4min, 1.5s/1hod. 36sec/24hod) 
		CPUPower: U8;
		DiskFree, DiskTotal: U8; // 16
		CPUUsage: S4; // 4 (0..10000)
		MS: TMemoryStatus; // 8 * 4 = 32
		OS: TOSVersionInfo; // 148
//		Graph: string[127]; // 128
		Reserved1: array[0..15] of U1; // 16
	end;

var
	SysInfo: TSysInfo;
	NTSystem: Boolean;
	RegCap: Boolean;

function GetKey(Default: Word): Word;
function OSToStr(OS: TOSVersionInfo): string;
function GetCPUUsage(IntTime: U8): SG;
procedure FillSysInfoS(var SInfo: TSysInfo);
procedure FillSysInfoD(var SysInfo: TSysInfo);

procedure Delay(const ms: LongWord);
procedure DelayEx(const f: U8);
function GetCPUCounter: TU8;
function PerformanceCounter: U8;

var
	PerformanceType: SG;
	PerformanceFrequency: U8;

implementation

{$R *.DFM}
uses
	uGraph, uScreen, uStrings,
	Registry;

function GetKey(Default: Word): Word;
label
	LAgain;
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

function OSToStr(OS: TOSVersionInfo): string;
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
		else
		begin
			if OS.dwMinorVersion = 0 then
				S := S + '2000'
			else
				S := S + 'XP';
		end;
	end;
	else // 3
		S := 'Unknown System ' + NToS(OS.dwPlatformId - VER_PLATFORM_WIN32_NT);
	end;
	S := S + ' (Build ' +
		NToS(OS.dwMajorVersion) + '.' +
		NToS(OS.dwMinorVersion) + '.' +
		NToS(LoWord(OS.dwBuildNumber)) + ' ' +
		OS.szCSDVersion + ')';
	Result := S;
end;

procedure MemoryStatus(var SysInfo: TSysInfo);
begin
	SysInfo.MS.dwLength := SizeOf(SysInfo.MS);
	GlobalMemoryStatus(SysInfo.MS);
end;


function GetProcessorTime : int64;
type
 TPerfDataBlock = packed record
   signature              : array [0..3] of wchar;
	 littleEndian           : cardinal;
   version                : cardinal;
   revision               : cardinal;
   totalByteLength        : cardinal;
   headerLength           : cardinal;
   numObjectTypes         : integer;
   defaultObject          : cardinal;
   systemTime             : TSystemTime;
   perfTime               : comp;
   perfFreq               : comp;
	 perfTime100nSec        : comp;
   systemNameLength       : cardinal;
   systemnameOffset       : cardinal;
 end;
 TPerfObjectType = packed record
   totalByteLength        : cardinal;
   definitionLength       : cardinal;
   headerLength           : cardinal;
   objectNameTitleIndex   : cardinal;
   objectNameTitle        : PWideChar;
   objectHelpTitleIndex   : cardinal;
   objectHelpTitle        : PWideChar;
   detailLevel            : cardinal;
   numCounters            : integer;
   defaultCounter         : integer;
   numInstances           : integer;
   codePage               : cardinal;
   perfTime               : comp;
   perfFreq               : comp;
 end;
 TPerfCounterDefinition = packed record
   byteLength             : cardinal;
   counterNameTitleIndex  : cardinal;
   counterNameTitle       : PWideChar;
   counterHelpTitleIndex  : cardinal;
   counterHelpTitle       : PWideChar;
   defaultScale           : integer;
   defaultLevel           : cardinal;
   counterType            : cardinal;
   counterSize            : cardinal;
   counterOffset          : cardinal;
 end;
 TPerfInstanceDefinition = packed record
   byteLength             : cardinal;
	 parentObjectTitleIndex : cardinal;
	 parentObjectInstance   : cardinal;
   uniqueID               : integer;
	 nameOffset             : cardinal;
   nameLength             : cardinal;
 end;
var
	c1, c2, c3      : cardinal;
	i1, i2          : integer;
	perfDataBlock   : ^TPerfDataBlock;
	perfObjectType  : ^TPerfObjectType;
	perfCounterDef  : ^TPerfCounterDefinition;
	perfInstanceDef : ^TPerfInstanceDefinition;
begin
 result := 0;
 perfDataBlock := nil;
 try
	 c1 := $10000;
	 while true do begin
		 ReallocMem(perfDataBlock, c1);
		 c2 := c1;
		 case RegQueryValueEx(HKEY_PERFORMANCE_DATA, '238', nil, @c3, Pointer(perfDataBlock), @c2) of
		 ERROR_MORE_DATA: c1 := c1 * 2;
		 ERROR_SUCCESS: Break;
		 else Exit;
		 end;
	 end;
	 perfObjectType := pointer(cardinal(perfDataBlock) + perfDataBlock^.headerLength);
	 for i1 := 0 to perfDataBlock^.numObjectTypes - 1 do begin
		 if perfObjectType^.objectNameTitleIndex = 238 then begin   // 238 -> "Processor"
			 perfCounterDef := pointer(cardinal(perfObjectType) + perfObjectType^.headerLength);
			 for i2 := 0 to perfObjectType^.numCounters - 1 do begin
				 if perfCounterDef^.counterNameTitleIndex = 6 then begin    // 6 -> "% Processor Time"
					 perfInstanceDef := pointer(cardinal(perfObjectType) + perfObjectType^.definitionLength);
					 result := PInt64(cardinal(perfInstanceDef) + perfInstanceDef^.byteLength + perfCounterDef^.counterOffset)^;
					 break;
				 end;
         inc(perfCounterDef);
       end;
       break;
     end;
     perfObjectType := pointer(cardinal(perfObjectType) + perfObjectType^.totalByteLength);
   end;
 finally FreeMem(perfDataBlock) end;
end; 

var
	LastTickCount, LastProcessorTime: U8;
	CPUUsage: SG;

	CPUException: Boolean; // Cyrix
	Reg: TRegistry;
	Dummy: array[0..1024] of Byte;

function GetCPUUsage(IntTime: U8): SG;
var
	tickCount     : U8;
	processorTime : U8;
begin
	if NTSystem then
	begin
//		tickCount := GetTickCount;
		tickCount := PerformanceCounter;
		if tickCount < LastTickCount + IntTime then
		begin
			Result := CPUUsage;
			Exit;
		end;
		processorTime := GetProcessorTime;

		if (LastTickCount <> 0) and (tickCount <> LastTickCount) and (processorTime >= LastProcessorTime) then
		begin // 1 000*10 000 = 10 000 000 / sec
			CPUUsage := 10000 - RoundDivS8(PerformanceFrequency * (processorTime - LastProcessorTime), 1000 * (tickCount - LastTickCount){ + 1});
			if CPUUsage < 0 then CPUUsage := 0;
		end;

		Result := CPUUsage;

		LastTickCount     := tickCount;
		LastProcessorTime := processorTime;
	end
	else
	begin
		Result := CPUUsage;
		if Reg = nil then
		begin
			Reg := TRegistry.Create;
			Reg.RootKey := HKEY_DYN_DATA;
//			Reg.CreateKey('PerfStats');
			if Reg.OpenKey('PerfStats\StartStat', True) then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', Dummy, SizeOf(Dummy));
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				Reg.CloseKey;
			end;

			if Reg.OpenKey('PerfStats\StatData', False) then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				Result := 100 * CPUUsage;
				Reg.CloseKey;
			end;
		end;

		if Reg.OpenKey('PerfStats\StatData', False) then
		begin
			Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
			Result := 100 * CPUUsage;
			Reg.CloseKey;
		end;
	end;
end;

function IsCPUID_Available : Boolean; register;
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
end;

procedure FillSysInfoD(var SysInfo: TSysInfo);
var
	TickCount: U8;
	CPUTick: U8;
const
	P: array[0..3] of Char = ('C', ':', '\', CharNul);
begin
{	P[0] := 'C';
	P[1] := ':';
	P[2] := '\';
	P[3] := CharNul;}
{ SectorsPerCluster := 0;
	BytesPerSector := 0;}
{
		GetDiskFreeSpace(P, SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
			TotalNumberOfClusters);
		SysInfo.DiskFree := BytesPerSector * SectorsPerCluster * NumberOfFreeClusters;
		SysInfo.DiskTotal := BytesPerSector * SectorsPerCluster * TotalNumberOfClusters;
}
	GetDiskFreeSpaceEx(P, SysInfo.DiskFree, SysInfo.DiskTotal, nil);

	MemoryStatus(SysInfo);

	SysInfo.CPUUsage := GetCPUUsage(0);

	if IsCPUID_Available and (CPUException = False) then
	begin
		SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
{   GetMem(PMem, 32768);
		GetMem(PMem2, 32768);}
		try
			SysInfo.CPUStr := '            '; // 12 spaces
			asm
			pushad
			mov eax, 0
			mov ebx, 0
			mov ecx, 0
			mov edx, 0
			dw 0a20fh // cpuid
			mov eax, SysInfo
			add eax, 5
			mov [eax], ebx
			mov [eax+4], edx
			mov [eax+8], ecx

			mov eax, 1
			mov ebx, 0
			mov ecx, 0
			mov edx, 0
			dw 0a20fh // cpuid

			mov edx, SysInfo
			mov [edx], eax
			popad
			end;

			TickCount := PerformanceCounter;
			CPUTick := GetCPUCounter.A;
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

			mov ecx, 999999 // 1M
//      mov edi, U4 ptr PMem
			@Loop:
				mov esi, edi
				mov ebx, ecx
				and ebx, 32767
				add esi, ebx
//        mov [esi], cl
				sub ecx, 1
			jnz @Loop

			popad
			end;
			CPUTick := GetCPUCounter.A - CPUTick;
			TickCount := PerformanceCounter - TickCount;
			if TickCount > 0 then
			begin
				SysInfo.CPUFrequency := RoundDivS8(CPUTick * PerformanceFrequency, TickCount);
				SysInfo.CPUPower := RoundDivS8(3 * 1000000 * PerformanceFrequency, TickCount);
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
		SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
{   FreeMem(PMem2, 32768);
		FreeMem(PMem, 32768);}
	end;
end;


procedure FillSysInfoS(var SInfo: TSysInfo);
begin
{	SysInfo.OS.dwOSVersionInfoSize := SizeOf(SysInfo.OS);
	GetVersionEx(SysInfo.OS);}
	SInfo.OS := SysInfo.OS;

{	if DriverDesc = '' then
	begin
		DriverDesc := 'Not Available';
		ReadScreenModes;
	end;
	SInfo.Graph := DriverDesc;}
end;

procedure TfSysInfo.FillComp;
var
	s: string;
	Family, Model: SG;
begin
	EditOS.Text := OSToStr(SysInfo.OS);

//  SysInfo.CPUStr :=
	Family := SysInfo.CPU and $00000f00 shr 8;
	Model := SysInfo.CPU and $000000f0 shr 4;

	s := '';
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
			5: s := 'Pentium II';
			6: s := 'Celeron';
			7: s := 'Pentium III';
			8: s := 'Pentium III E';
			else s := 'Pentium 4';
			end;
		end;
		end;
		s := 'Intel ' + s;
	end
	else if SysInfo.CPUStr = 'CyrixInstead' then
	else if SysInfo.CPUStr = 'NexGenDriven' then
	else if SysInfo.CPUStr = 'CentaurHauls' then
	else if SysInfo.CPUStr = 'RiseRiseRise' then
	else if SysInfo.CPUStr = 'UMC UMC UMC ' then


	else
		s := SysInfo.CPUStr;


	if s <> '' then
		s := s + ', ';
	s := s + 'Family: ' + NToS(Family) + ', ';
	s := s + 'Model: ' + NToS(Model) + ', ';
	s := s + 'Stepping: ' + NToS(SysInfo.CPU and $000000f);
	EditCPU.Text := s;

	EditCPUUsage.Text := NToS(SysInfo.CPUUsage, 2) + '%';
	EditCPUFrequency.Text := NToS(SysInfo.CPUFrequency) + ' Hz';
	EditDuron.Text := NToS(SysInfo.CPUPower) + ' Hz';


	EditDiskU.Caption := BToStr(SysInfo.DiskTotal - SysInfo.DiskFree);
	EditDiskF.Caption := BToStr(SysInfo.DiskFree);
	EditDiskT.Caption := BToStr(SysInfo.DiskTotal);

	PMU.Caption := BToStr(SysInfo.MS.dwTotalPhys - SysInfo.MS.dwAvailPhys);
	PMF.Caption := BToStr(SysInfo.MS.dwAvailPhys);
	PMT.Caption := BToStr(SysInfo.MS.dwTotalPhys);

	PFU.Caption := BToStr(SysInfo.MS.dwTotalPageFile - SysInfo.MS.dwAvailPageFile);
	PFF.Caption := BToStr(SysInfo.MS.dwAvailPageFile);
	PFT.Caption := BToStr(SysInfo.MS.dwTotalPageFile);

end;

procedure TfSysInfo.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure InitPerformanceCounter;
begin
	if QueryPerformanceFrequency(PerformanceFrequency) then
	begin
		PerformanceType := 1;
	end
	else
	begin
		PerformanceType := 0;
		PerformanceFrequency := 1000;
	end;
end;

function GetCPUCounter: TU8;
begin
	asm
	mov ecx, 10h
	dw 310fh // RDTSC 10clocks
	mov ebx, Result
	mov [ebx], eax
	mov [ebx + 4], edx
	end;
end;

function PerformanceCounter: Int64;
begin
	case PerformanceType of
	0: Result := GetTickCount;
	1: QueryPerformanceCounter(Result);
	else Result := GetCPUCounter.A;
	end;
end;

procedure Delay(const ms: LongWord);
var
	TickCount: LongWord;
begin
	TickCount := GetTickCount + ms;
	while GetTickCount < TickCount do
end;

procedure DelayEx(const f: Int64);
var
	TickCount: Int64;
begin
	TickCount := PerformanceCounter + f;
	while PerformanceCounter < TickCount do
end;

procedure TfSysInfo.FormCreate(Sender: TObject);
begin
	Background := baGradient;
end;

initialization
	SysInfo.OS.dwOSVersionInfoSize := SizeOf(SysInfo.OS);
	GetVersionEx(SysInfo.OS);
	NTSystem := SysInfo.OS.dwMajorVersion >= 5;
	RegCap := not ((SysInfo.OS.dwMajorVersion < 4) or ((SysInfo.OS.dwMajorVersion = 4) and (SysInfo.OS.dwMinorVersion < 10)));

	InitPerformanceCounter;
{ PerformanceType := 2;
	FillSysInfoD(SysInfo);
	PerformanceFrequency := SysInfo.CPUFrequency;}

	CPUUsage := 30 * 100;
	GetCPUUsage(0);

finalization
	if NTSystem = False then
	begin
		if Reg <> nil then
		begin
			if Reg.OpenKey('PerfStats\StopStat', False) then
			begin
				Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
				Reg.CloseKey;
			end;

			Reg.Free;
			Reg := nil;
		end;
	end;
end.
