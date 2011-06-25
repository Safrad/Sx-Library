//* File:     Lib\GUI\uSysInfo.pas
//* Created:  2000-07-01
//* Modified: 2007-11-27
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uSysInfo;

interface

uses
	uTypes, uMath,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uDLabel, uDButton, uDForm, uDEdit;

const
	CPUUsageMul = 100;
type
	PSysInfo = ^TSysInfo;
	TSysInfo = packed record // 256
		CPU: U4;
		CPUStr: string[12]; // 13
		Reserved0: array[0..6] of S1; // 7
		CPUFrequency: U8; // precision 0,00041666 (0.1s/4min, 1.5s/1hod. 36sec/24hod)
		CPUPower: U8;
		PerformanceFrequency: U4;
//		DiskFree, DiskTotal: U8; // 16
//		Reserved: array[0..3] of U4; // 16
		CPUUsage: S4; // 4 (0..10000)
		MS: TMemoryStatus; // 8 * 4 = 32
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
	RegCap: Boolean;

function GetKey(Default: U2): U2;
function OSToStr(OS: TOSVersionInfo): string;
function GetCPUUsage(IntTime: U8): SG;
procedure FillDynamicInfo(var SysInfo: TSysInfo); // FillMemoryStatus + FillCPUTest
procedure FillMemoryStatus(var SysInfo: TSysInfo);
procedure FillCPUTest(var SysInfo: TSysInfo);
procedure DisplaySysInfo(SysInfo: PSysInfo);
procedure UpdateSysInfo(SysInfo: PSysInfo);

implementation

{$R *.DFM}
uses
	uGraph, uScreen, uStrings, uOutputFormat, uSimulation,
	uProjectInfo,
	Registry, Math;

var
	fSysInfo: TfSysInfo;

function GetKey(Default: U2): U2;
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

var
	LastTickCount, LastProcessorTime: U8;
	CPUUsage: SG;

	CPUException: Boolean; // Cyrix
	Reg: TRegistry;

function GetCPUUsage(IntTime: U8): SG;
var
	tickCount     : U8;
	processorTime : U8;
	Dummy: array[0..KB] of U1;
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

		if (LastTickCount <> 0) and (tickCount > LastTickCount) and (processorTime >= LastProcessorTime) then
		begin // 1 000*10 000 = 10 000 000 / sec
			CPUUsage := 100 * CPUUsageMul - RoundDivS8(PerformanceFrequency * (processorTime - LastProcessorTime), 1000 * (tickCount - LastTickCount){ + 1}) ;
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
				SysInfo.CPUStr := StringOfChar(CharSpace, 12);
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
	SysInfo.CPUUsage := GetCPUUsage(0);
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
			5: s := 'Pentium II';
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
		s := s + ', ';
	s := s + 'Family: ' + NToS(Family) + ', ';
	s := s + 'Model: ' + NToS(Model) + ', ';
	s := s + 'Stepping: ' + NToS(SysInfo.CPU and $000000f);
	EditCPU.Text := s;

	EditCPUUsage.Text := NToS(SysInfo.CPUUsage, 2) + '%';
	EditCPUFrequency.Text := NToS(SysInfo.CPUFrequency) + ' Hz';
	EditDuron.Text := NToS(SysInfo.CPUPower) + ' Hz';
	EditCounter.Text := NToS(SysInfo.PerformanceFrequency) + ' Hz';

	edMU.Text := BToStr(SysInfo.MS.dwTotalPhys - SysInfo.MS.dwAvailPhys);
	edMF.Text := BToStr(SysInfo.MS.dwAvailPhys);
	edMT.Text := BToStr(SysInfo.MS.dwTotalPhys);

	edFU.Text := BToStr(SysInfo.MS.dwTotalPageFile - SysInfo.MS.dwAvailPageFile);
	edFF.Text := BToStr(SysInfo.MS.dwAvailPageFile);
	edFT.Text := BToStr(SysInfo.MS.dwTotalPageFile);
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
begin
	GSysInfo.OS.dwOSVersionInfoSize := SizeOf(GSysInfo.OS);
	GetVersionEx(GSysInfo.OS);
	NTSystem := GSysInfo.OS.dwMajorVersion >= 5;
	RegCap := not ((GSysInfo.OS.dwMajorVersion < 4) or ((GSysInfo.OS.dwMajorVersion = 4) and (GSysInfo.OS.dwMinorVersion < 10)));

	InitPerformanceCounter;
{	PerformanceType := ptCPU;
	FillCPUTest(GSysInfo);
	PerformanceFrequency := GSysInfo.CPUFrequency;}

	GSysInfo.PerformanceFrequency := PerformanceFrequency;

//	GSysInfo.ProgramVersion := GetProjectInfo(piProductVersion);

	CPUUsage := 0 * CPUUsageMul;
	GetCPUUsage(0);
end;

procedure DisplaySysInfo(SysInfo: PSysInfo);
begin
	if not Assigned(fSysInfo) then
		fSysInfo := TfSysInfo.Create(nil);
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
