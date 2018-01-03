unit uSysInfo;

interface

uses
	uTypes,
	Windows, Messages, SysUtils;

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
		MS: TMemoryStatusEx; // 8 * 8 = 64
		OS: TOSVersionInfo; // 148
	end;

var
	GSysInfo: TSysInfo;
	NTSystem: Boolean;
	Aero: Boolean;
	RegionCompatibily: Boolean;

function OSToStr(const OS: TOSVersionInfo): string;
procedure FillMemoryStatus(var SysInfo: TSysInfo);

//function MMUsedMemory: U8;
function MaxPhysicalMemorySize: U8;
function ProcessAllocatedVirtualMemory: U8;
function CanAllocateMemory(const Size: UG): BG;

implementation

uses
  PsAPI,
  uMsg,
  uMath,
	uStrings, uOutputFormat, uDictionary,
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
  end;
end;
{$ifend}

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
		else if OS.dwMajorVersion = 6 then
		begin
			case OS.dwMinorVersion of
			0: S := S + 'Vista';
      1: S := S + '7';
      2: S := S + '8';
      3: S := S + '8.1';
      else S := S + '10';
      end;
		end
		else // if OS.dwMajorVersion = 10 then
      S := S + IntToStr(OS.dwMajorVersion);
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

procedure Init;
begin
	GSysInfo.OS.dwOSVersionInfoSize := SizeOf(GSysInfo.OS);
	GetVersionEx(GSysInfo.OS);
	NTSystem := GSysInfo.OS.dwMajorVersion >= 5;
  Aero := GSysInfo.OS.dwMajorVersion >= 6; // >= Vista
	RegionCompatibily := not ((GSysInfo.OS.dwMajorVersion < 4) or ((GSysInfo.OS.dwMajorVersion = 4) and (GSysInfo.OS.dwMinorVersion < 10)));
end;

//function MMUsedMemory: U8;
//var
//    st: TMemoryManagerState;
//    sb: TSmallBlockTypeState;
//    i: SG;
//begin
//  GetMemoryManagerState(st);
//  Result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
//  for i := Low(st.SmallBlockTypeStates) to High(st.SmallBlockTypeStates) do
//  begin
//    sb := st.SmallBlockTypeStates[i];
//      Inc(Result, sb.UseableBlockSize * sb.AllocatedBlockCount);
//  end;
//end;

function ProcessAllocatedVirtualMemory: U8;
var
  MemCounters: TProcessMemoryCounters;
begin
  MemCounters.cb := SizeOf(MemCounters);
  Result := 0;
  if GetProcessMemoryInfo(GetCurrentProcess,
      @MemCounters,
      SizeOf(MemCounters)) then
  begin
    // MemCounters.PagefileUsage is defined as SIZE_T (size is 4 bytes in 32 bit version and 8 bytes in 64 bit version)
    Result := MemCounters.PagefileUsage;
  end
  else
    RaiseLastOSError;
end;

function MaxPhysicalMemorySize: U8;
begin
	FillMemoryStatus(GSysInfo);
  Result := Min(2 * GSysInfo.MS.ullTotalPhys div 3 {66%}, GSysInfo.MS.ullTotalVirtual);
end;

function MaxAllocationSize: U8;
begin
	FillMemoryStatus(GSysInfo);
  Result := Max(0, MaxPhysicalMemorySize - ProcessAllocatedVirtualMemory);

  Result := 2 * Result div 3; // Fragmentation
end;

function CanAllocateMemory(const Size: UG): BG;
const
  ReservedSize = 8 * MB;
var
  P: Pointer;
begin
//  Result := Size + ReservedSize < MaxAllocationSize;
  try
    GetMem(P, Size + ReservedSize);
    Result := P <> nil;
    FreeMem(P);
  except
    Result := False;
  end;
end;

initialization
{$IFNDEF NoInitialization}
	Init;
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}

{$ENDIF NoFinalization}
end.
