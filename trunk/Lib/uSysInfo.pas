unit uSysInfo;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, uDPanel, StdCtrls, uDLabel, uDButton;

type
	TfSysInfo = class(TForm)
		Bevel1: TBevel;
		LabelTOperatingSystem: TDLabel;
		EditOS: TEdit;
		Bevel4: TBevel;
		LabelUsed: TDLabel;
		LabelFree: TDLabel;
		LabelTotal: TDLabel;
		PMT: TDPanel;
		PMF: TDPanel;
		PFF: TDPanel;
		PFT: TDPanel;
		PMU: TDPanel;
		PFU: TDPanel;
		LabelTPhysicalMemory: TDLabel;
		LabelTPageFile: TDLabel;
		Bevel3: TBevel;
		Bevel2: TBevel;
		DLabel3: TDLabel;
		DLabel5: TDLabel;
		DLabel6: TDLabel;
		Bevel5: TBevel;
		EditCPU: TEdit;
		EditDiskU: TDPanel;
		EditGraph: TEdit;
		EditDiskF: TDPanel;
		EditDiskT: TDPanel;
		Bevel6: TBevel;
		ButtonOk: TDButton;
		DLabel1: TDLabel;
		EditCPUFrequency: TEdit;
		DLabel2: TDLabel;
		EditDuron: TEdit;
		procedure ButtonOkClick(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure FillComp;
//    procedure MemoryStatus;
	end;

var
	fSysInfo: TfSysInfo;

type
	TSysInfo = packed record
		CPU: U32;
		CPUStr: string[12]; // 13
		CPUFrequency: U64;
		CPUPower: U64;
		MS: TMemoryStatus;
		DiskFree, DiskTotal: U64;
		Graph: string[127];
		OS: TOSVersionInfo; // 148
	end;

var
	SysInfo: TSysInfo;

function OSToStr(OS: TOSVersionInfo): string;
procedure FillSysInfoS(var SysInfo: TSysInfo);
procedure FillSysInfoD(var SysInfo: TSysInfo);

procedure Delay(const ms: LongWord);
function GetCPUCounter: TU64;
function PerformanceCounter: Int64;
procedure DelayEx(const f: Int64);

var
	PerformanceType: SG;
	PerformanceFrequency: U64;

implementation

{$R *.DFM}
uses uGraph, Registry, uScreen;

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
			S := S + '2000';
	end;
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

procedure MemoryStatus(var SysInfo: TSysInfo);
begin
	SysInfo.MS.dwLength := SizeOf(SysInfo.MS);
	GlobalMemoryStatus(SysInfo.MS);
end;

var
	CPUException: Boolean; // Cyrix

procedure FillSysInfoD(var SysInfo: TSysInfo);
var
	P: array[0..3] of Char;
{ PMem: PByteArray;
	PMem2: PByteArray;}
	TickCount: U64;
{ SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
	TotalNumberOfClusters: U32;}
	CPUTick: U64;
begin
	P[0] := 'C';
	P[1] := ':';
	P[2] := '\';
	P[3] := #0;
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
	if CPUException = False then
	begin
		SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
{   GetMem(PMem, 32768);
		GetMem(PMem2, 32768);}
		try
			SysInfo.CPUStr := '            ';
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

			mov edx, SysInfo.CPU
			mov [edx], eax
			popad
			end;

			TickCount := PerformanceCounter;
			CPUTick := GetCPUCounter.A;
			asm
			pushad

{     mov ecx, 999 // 1M

			@Loop:
				mov edi, dword ptr PMem
				mov esi, dword ptr PMem2
				push ecx
				mov ecx, 32768
				shr ecx, 2
				cld
					rep movsd
				pop ecx
				sub ecx, 1
			jnz @Loop}

			mov ecx, 999999 // 1M
//      mov edi, dword ptr PMem
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
			TickCount := PerformanceCounter - TickCount + 1;
			SysInfo.CPUFrequency := RoundDiv64(CPUTick * PerformanceFrequency, TickCount);
			SysInfo.CPUPower := RoundDiv64(3 * 1000000 * PerformanceFrequency, TickCount);
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

procedure FillSysInfoS(var SysInfo: TSysInfo);
begin
	SysInfo.OS := OS;

	if DriverDesc = '' then
	begin
		DriverDesc := 'Not Available';
		ReadScreenModes;
	end;
	SysInfo.Graph := DriverDesc;
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
			3: s := 'Duron';
			4, 5: s := 'Thunderbird';
			end;
		end;
		end;
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
			end;
		end;
		end;
	end
	else if SysInfo.CPUStr = 'CyrixInstead' then
	else if SysInfo.CPUStr = 'NexGenDriven' then
	else if SysInfo.CPUStr = 'CentaurHauls' then
	else if SysInfo.CPUStr = 'RiseRiseRise' then
	else if SysInfo.CPUStr = 'UMC UMC UMC ' then


	else
		s := SysInfo.CPUStr;


	s := s + ', ';
	s := s + 'Family: ' + IntToStr(Family) + ', ';
	s := s + 'Model: ' + IntToStr(Model) + ', ';
	s := s + 'Stepping: ' + IntToStr(SysInfo.CPU and $000000f);
	EditCPU.Text := s;

	EditCPUFrequency.Text :=
		Using('~#,###,###,###,###,###,###,###,##0', SysInfo.CPUFrequency) + ' Hz';
	EditDuron.Text :=
		Using('~#,###,###,###,###,###,###,###,##0', SysInfo.CPUPower) + ' Hz';


	EditDiskU.Caption := BToStr(SysInfo.DiskTotal - SysInfo.DiskFree);
	EditDiskF.Caption := BToStr(SysInfo.DiskFree);
	EditDiskT.Caption := BToStr(SysInfo.DiskTotal);

	PMU.Caption := BToStr(SysInfo.MS.dwTotalPhys - SysInfo.MS.dwAvailPhys);
	PMF.Caption := BToStr(SysInfo.MS.dwAvailPhys);
	PMT.Caption := BToStr(SysInfo.MS.dwTotalPhys);

	PFU.Caption := BToStr(SysInfo.MS.dwTotalPageFile - SysInfo.MS.dwAvailPageFile);
	PFF.Caption := BToStr(SysInfo.MS.dwAvailPageFile);
	PFT.Caption := BToStr(SysInfo.MS.dwTotalPageFile);

	EditGraph.Text := SysInfo.Graph;
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

function GetCPUCounter: TU64;
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

initialization
	InitPerformanceCounter;
{ PerformanceType := 2;
	PerformanceFrequency := SysInfo.CPUFrequency;}
end.
