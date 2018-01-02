unit uMemoryBenchmark;

interface

uses
  uTypes,
  uProjectVersion,
  uBenchmark;

type
  TMemoryBenchmark = class(TBenchmark)
  private
    FCPUPower: U8;
    FCPUFrequency: U8;
    FBlockSize: UG;
    procedure SetCPUFrequency(const Value: U8);
    procedure SetCPUPower(const Value: U8);
    procedure SetBlockSize(const Value: UG);
  protected
    function GetVersion: TProjectVersion; override;
  public
    constructor Create;
    procedure Execute; override;

    property CPUFrequency: U8 read FCPUFrequency write SetCPUFrequency;
    property CPUPower: U8 read FCPUPower write SetCPUPower;
    property BlockSize: UG read FBlockSize write SetBlockSize;
  end;

implementation

uses
  uMath;


{$ifdef CPUX64}
procedure Loop(PMem: Pointer; MaxMem4: NativeInt; Count: NativeInt);
asm
  push rax
  push rbx
  push rcx
  push rdi
  mov rdi, PMem{rcx}
  mov rcx, Count
  sub rcx, 1
  @Loop:
    mov rax, rcx
    and rax, MaxMem4
    mov [rdi+8*rax], rbx
    sub rcx, 1
  jnz @Loop
  pop rdi
  pop rcx
  pop rbx
  pop rax
{
      j := 0;
      while j < Count do
      begin
        PDWORD(PByte(PMem) + (SizeOf(Pointer) * j) and MaxMem4)^ := j;
        Inc(j);
      end;}
end;
{$else}
procedure Loop(PMem: Pointer; MaxMem4: U4; Count: U4);
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

{ TProcessorTests }

constructor TMemoryBenchmark.Create;
begin
  inherited;

  FBlockSize := 64 * KB;
end;

procedure TMemoryBenchmark.Execute;
var
	TickCount: U8;
	CPUTick: U8;
const
	Count = 1 shl 22;
var
	MaxMem4: UG;
	PMem: Pointer;
begin
  MaxMem4 := MaxDiv(FBlockSize, SizeOf(Pointer)) - 1;
  GetMem(PMem, FBlockSize);
  try
    try
      TickCount := PerformanceCounter;
      CPUTick := GetCPUCounter.A;
      Loop(PMem, MaxMem4, Count div 2);

      CPUTick := GetCPUCounter.A - CPUTick;
      TickCount := IntervalFrom(TickCount);
      if (TickCount > 0) and (CPUTick < High(Int64) div (2 * PerformanceFrequency)) then
      begin
        CPUFrequency := RoundDivS8(CPUTick * PerformanceFrequency, TickCount);
        CPUPower := RoundDivS8(4 * Count * PerformanceFrequency, TickCount);
      end
      else
      begin
        CPUFrequency := 0;
        CPUPower := 0;
      end;
    except
      CPUFrequency := 0;
      CPUPower := 0;
    end;
  finally
    CalculatedItems := CPUPower;
    FreeMem(PMem);
	end;
end;

function TMemoryBenchmark.GetVersion: TProjectVersion;
begin
  Result.Major := 1;
  Result.Minor := 1;
  Result.Release := 0;
  Result.Build := 0;
end;

procedure TMemoryBenchmark.SetBlockSize(const Value: UG);
begin
  FBlockSize := Value;
end;

procedure TMemoryBenchmark.SetCPUFrequency(const Value: U8);
begin
  FCPUFrequency := Value;
end;

procedure TMemoryBenchmark.SetCPUPower(const Value: U8);
begin
  FCPUPower := Value;
end;

end.

	ComboBoxSize.Items.BeginUpdate;
	try
		for i := {$ifdef CPUX64}3{$else}2{$endif} to 29 do
		begin
			m := 1 shl i;
			if m >= GSysInfo.MS.ullAvailPhys div 2 then Break;
			ComboBoxSize.Items.Add(BToStr(m));
		end;
		ComboBoxSize.ItemIndex := 14;
	finally
		ComboBoxSize.Items.EndUpdate;
	end;

