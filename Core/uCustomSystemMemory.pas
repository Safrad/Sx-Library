unit uCustomSystemMemory;

interface

uses
  uTypes,
  uRatioValue;

type
  TCustomSystemMemory = class
  private
    FVirtual: TRatioValue;
    FPhysical: TRatioValue;
    FPageFile: TRatioValue;
  public
    procedure Update; virtual; abstract;

    /// <summary>Maximal available memory size for all applications.
    /// Calculated as all physical memory minus reserved memory for system and basic applications.
    /// </summary>
    /// <returns>Available physical memory size in bytes. <c>For example 13 GB on 16 GB system.</c></returns>
    function MaxPhysicalMemorySize64: U8;

    /// <summary>This is default value for memory allocation.
    /// <see cref="MaxPhysicalMemorySize64">See MaxPhysicalMemorySize64.</see>
    /// </summary>
    /// <returns>On 64 bit system is equal to maximal available memory size for all applications.
    /// Limit on 32 bit system is 3 GB.</returns>
    function MaxPhysicalMemorySize: U8;

    /// <summary>This is default value for one block memory allocation.
    /// <see cref="MaxPhysicalMemorySize64">See MaxPhysicalMemorySize64.</see>
    /// </summary>
    /// <returns>
    /// On 64 bit system is equal to maximal available memory size for all applications.
    /// Limit on 32 bit system is 1.5 GB.
    /// </returns>
    function MaxPhysicalMemoryOneBlockSize: U8;

    /// <summary>
    /// Counterd as total available memory minus system reserved memory
    /// <code>
    /// <para>Operating System         Default minimal system reserved RAM [MB]</para>
    /// <para>Windows 95                  8 (4 running minimal)</para>
    /// <para>Windows 98                 24 (16 running minimal)</para>
    /// <para>Windows XP Home Edition   128 (64 running minimal)</para>
    /// <para>Windows Vista Home Basic  512</para>
    /// <para>Windows Vista others:    1024</para>
    /// <para>Win7/8/10:               1024</para>
    /// <para>Win7/8/10 x64:           2048</para>
    ///  </code>
    /// </summary>
    /// <returns>Total available memory for all processes</returns>
    function ReservedPhysicalMemoryForOthers: U8;

    /// <summary>Total allocated virtual memory of current process.
    /// </summary>
    /// <returns>Memory size in bytes. <c>For example 50 MB.</c></returns>
    function ProcessAllocatedVirtualMemory: U8; virtual; abstract;

    /// <summary>Try to allocate <paramref name="ASize" /> memory</summary>
    /// <param name="ASize">Allocation size in bytes</param>
    /// <returns>True if memory can be allocated otherwise False</returns>
    function CanAllocateMemory(const ASize: UG): BG;

    property Physical: TRatioValue read FPhysical;
    property PageFile: TRatioValue read FPageFile;
    property Virtual: TRatioValue read FVirtual;
  end;

implementation

uses
  Math;

{ TCustomSystemMemory }

function TCustomSystemMemory.CanAllocateMemory(const ASize: UG): BG;
const
  ReservedSize = 8 * MB;
var
  P: Pointer;
begin
  try
    GetMem(P, ASize + ReservedSize);
    Result := P <> nil;
    FreeMem(P);
  except
    Result := False;
  end;
end;

function TCustomSystemMemory.MaxPhysicalMemorySize64: U8;
begin
	Update;
  Result := U8(Min(Physical.Total, Virtual.Total)) - ReservedPhysicalMemoryForOthers;
end;

function TCustomSystemMemory.MaxPhysicalMemorySize: U8;
const
  MemorySizeLimit = 3 * U8(GB);
begin
  Result := MaxPhysicalMemorySize64;

  {$ifdef CPUX86}
  if Result > MemorySizeLimit then
  begin
    Result := MemorySizeLimit;
  end;
  {$endif}
end;

function TCustomSystemMemory.MaxPhysicalMemoryOneBlockSize: U8;
const
  OneBlockMemorySizeLimit = 1536 * U8(MB);
begin
  Result := MaxPhysicalMemorySize64;

  {$ifdef CPUX86}
  if Result > OneBlockMemorySizeLimit then
  begin
    Result := OneBlockMemorySizeLimit;
  end;
  {$endif}
end;

function TCustomSystemMemory.ReservedPhysicalMemoryForOthers: U8;
begin
  if Physical.Total < 96 * MB then
    Result := 4 * Physical.Total div 5 // 80 %
  else if Physical.Total < 384 * MB then
    Result := 3 * Physical.Total div 4 // 75 %
  else if Physical.Total < 1536 * MB then
    Result := 3 * Physical.Total div 5 // 60 %
  else if Physical.Total < 6 * U8(GB) then
    Result := Physical.Total div 2 // 50 %
  else
    Result := 3 * U8(GB); // < 50 %

  Result := Min(U8(Physical.Used), Result);
end;

end.
