//* File:     Lib\uSysInfo.pas
//* Created:  2000-07-01
//* Modified: 2005-12-11
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uSysInfo;

interface

uses
	uTypes, uMath,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDForm;

type
	TfSysInfo = class(TDForm)
		Bevel1: TBevel;
    LabelTOperatingSystem: TLabel;
		EditOS: TEdit;
		Bevel4: TBevel;
    LabelUsed: TLabel;
    LabelFree: TLabel;
    LabelTotal: TLabel;
    edMT: TEdit;
    edMF: TEdit;
    edFF: TEdit;
    edFT: TEdit;
    edMU: TEdit;
    edFU: TEdit;
    LabelTPhysicalMemory: TLabel;
    LabelTPageFile: TLabel;
		Bevel3: TBevel;
		Bevel2: TBevel;
		DLabel3: TLabel;
    LabelDisk: TLabel;
		Bevel5: TBevel;
		EditCPU: TEdit;
    edDiskU: TEdit;
    edDiskF: TEdit;
    edDiskT: TEdit;
		ButtonOk: TDButton;
    DLabelCPUFrequency: TLabel;
		EditCPUFrequency: TEdit;
    LabelAMDDuronCmp: TLabel;
		EditDuron: TEdit;
    DLabelCPUUsage: TLabel;
		EditCPUUsage: TEdit;
    EditCounter: TEdit;
    LabelMBoardCounter: TLabel;
		ComboBoxSize: TComboBox;
		procedure ButtonOkClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure FillComp;
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
		ProgramVersion: string[15]; // 10.32.101.10000
//		Graph: string[127]; // 128
//		Reserved1: array[0..15] of U1; // 16
	end;

var
	GSysInfo: TSysInfo;
	NTSystem: Boolean;
	RegCap: Boolean;

function GetKey(Default: Word): Word;
function OSToStr(OS: TOSVersionInfo): string;
function GetCPUUsage(IntTime: U8): SG;
procedure FillMemoryStatus(var SysInfo: TSysInfo);
procedure FillCPUTest(var SysInfo: TSysInfo);

implementation

{$R *.DFM}
uses
	uGraph, uScreen, uStrings, uFormat,
	rpVersionInfo,
	Registry, Math;

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

procedure FillMemoryStatus(var SysInfo: TSysInfo);
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
		 case RegQueryValueEx(HKEY_PERFORMANCE_DATA, '238'{'Processor/Processor Time'}, nil, @c3, Pointer(perfDataBlock), @c2) of
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

function GetCPUUsage(IntTime: U8): SG;
var
	tickCount     : U8;
	processorTime : U8;
	Dummy: array[0..1024] of Byte;
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
			CPUUsage := 100 * CPUUsageMul - RoundDivS8(PerformanceFrequency * (processorTime - LastProcessorTime), 1000 * (tickCount - LastTickCount){ + 1});
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
				Result := CPUUsageMul * CPUUsage;
				Reg.CloseKey;
			end;
		end;

		if Reg.OpenKey('PerfStats\StatData', False) then
		begin
			Reg.ReadBinaryData('KERNEL\CPUUsage', CPUUsage, SizeOf(CPUUsage));
			Result := CPUUsageMul * CPUUsage;
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

procedure FillDiskInfo(var SysInfo: TSysInfo);
const
	P: array[0..3] of Char = ('C', ':', '\', CharNul);
begin
	GetDiskFreeSpaceEx(P, SysInfo.DiskFree, SysInfo.DiskTotal, nil);
end;

procedure FillCPUTest(var SysInfo: TSysInfo);
var
	TickCount: U8;
	CPUTick: U8;
const
	P: array[0..3] of Char = ('C', ':', '\', CharNul);
	Count = 1 shl 22;
var
	MaxMem4, MaxMem: UG;
	PMem: Pointer;
begin
	if Assigned(fSysInfo) then
		MaxMem4 :=  (1 shl Max(0, fSysInfo.ComboBoxSize.ItemIndex)){14 for Duron} - 1 {10 = 4kB; 14=64kB}
	else
		MaxMem4 :=  1 shl 14 - 1; {10 = 4kB; 14=64kB}
	MaxMem := 4 * (MaxMem4 + 1) - 1;
	if IsCPUID_Available and (CPUException = False) then
	begin
		GetMem(PMem, MaxMem + 1);
		SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
		try
			SysInfo.CPUStr := '            '; // 12 spaces
			asm
			pushad
			xor eax, eax
			xor ebx, ebx
			xor ecx, ecx
			xor edx, edx
			dw 0a20fh // cpuid
			mov eax, SysInfo
			add eax, 5
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
		SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
		FreeMem(PMem);
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
	FillDiskInfo(SysInfo);
	FillMemoryStatus(SysInfo);
	SysInfo.CPUUsage := GetCPUUsage(0);
	FillCPUTest(SysInfo);
end;

procedure TfSysInfo.FillComp;
var
	s: string;
	Family, Model: SG;
begin
	FillDynamicInfo(GSysInfo);

	EditOS.Text := OSToStr(GSysInfo.OS);

{
function GetCpuSpeed: string;
var
	Reg: TRegistry;
begin
	Reg := TRegistry.Create;
try
	Reg.RootKey := HKEY_LOCAL_MACHINE;
	if Reg.OpenKey('Hardware\Description\System\CentralProcessor\0', False) then
	begin
		Result := IntToStr(Reg.ReadInteger('~MHz')) + ' MHz';
		Reg.CloseKey;
	end;
	finally
		Reg.Free;
	end;
end;
}

	Family := GSysInfo.CPU and $00000f00 shr 8;
	Model := GSysInfo.CPU and $000000f0 shr 4;

	s := '';
	if GSysInfo.CPUStr = 'AuthenticAMD' then
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
	else if GSysInfo.CPUStr = 'GenuineIntel' then
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
			5: s := 'Pentium II';
			6: s := 'Celeron';
			7: s := 'Pentium III';
			8: s := 'Pentium III E';
			else s := 'Pentium 4';
			end;
		end
		else // 15
			s := 'Pentium 4';
		end;
		if  s <> '' then s := ' ' + s;
		s := 'Intel' + s;
	end
	else if GSysInfo.CPUStr = 'CyrixInstead' then
		s := 'Cyrix '
	else if GSysInfo.CPUStr = 'NexGenDriven' then
		s := 'NexGen '
	else if GSysInfo.CPUStr = 'CentaurHauls' then
		s := 'Centaur '
	else if GSysInfo.CPUStr = 'RiseRiseRise' then
		s := 'Rise '
	else if GSysInfo.CPUStr = 'UMC UMC UMC ' then
		s := 'UMC ';

	if s <> '' then
		s := s + ', ';
	s := s + 'Family: ' + NToS(Family) + ', ';
	s := s + 'Model: ' + NToS(Model) + ', ';
	s := s + 'Stepping: ' + NToS(GSysInfo.CPU and $000000f);
	EditCPU.Text := s;

	EditCPUUsage.Text := NToS(GSysInfo.CPUUsage, 2) + '%';
	EditCPUFrequency.Text := NToS(GSysInfo.CPUFrequency) + ' Hz';
	EditDuron.Text := NToS(GSysInfo.CPUPower) + ' Hz';
	EditCounter.Text := NToS(PerformanceFrequency) + ' Hz';

	edDiskU.Text := BToStr(GSysInfo.DiskTotal - GSysInfo.DiskFree);
	edDiskF.Text := BToStr(GSysInfo.DiskFree);
	edDiskT.Text := BToStr(GSysInfo.DiskTotal);

	edMU.Text := BToStr(GSysInfo.MS.dwTotalPhys - GSysInfo.MS.dwAvailPhys);
	edMF.Text := BToStr(GSysInfo.MS.dwAvailPhys);
	edMT.Text := BToStr(GSysInfo.MS.dwTotalPhys);

	edFU.Text := BToStr(GSysInfo.MS.dwTotalPageFile - GSysInfo.MS.dwAvailPageFile);
	edFF.Text := BToStr(GSysInfo.MS.dwAvailPageFile);
	edFT.Text := BToStr(GSysInfo.MS.dwTotalPageFile);

end;

procedure TfSysInfo.ButtonOkClick(Sender: TObject);
begin
	Close;
end;

procedure TfSysInfo.FormCreate(Sender: TObject);
var i: SG;
begin
	Background := baGradient;
	for i := 2 to 24 do
		ComboBoxSize.Items.Add(BToStr(1 shl i));
	ComboBoxSize.ItemIndex := 14;
end;

procedure Init;
var
	VersionInfo: TrpVersionInfo;
begin
	GSysInfo.OS.dwOSVersionInfoSize := SizeOf(GSysInfo.OS);
	GetVersionEx(GSysInfo.OS);
	NTSystem := GSysInfo.OS.dwMajorVersion >= 5;
	RegCap := not ((GSysInfo.OS.dwMajorVersion < 4) or ((GSysInfo.OS.dwMajorVersion = 4) and (GSysInfo.OS.dwMinorVersion < 10)));

	InitPerformanceCounter;
{ PerformanceType := 2;
	FillSysInfoD(SysInfo);
	PerformanceFrequency := SysInfo.CPUFrequency;}

	VersionInfo := TrpVersionInfo.Create(nil);
	GSysInfo.ProgramVersion := VersionInfo.FileVersion;
	VersionInfo.Free;

	CPUUsage := 0 * CPUUsageMul;
	GetCPUUsage(0);

end;

initialization
	Init;
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

			FreeAndNil(Reg);
		end;
	end;
end.
