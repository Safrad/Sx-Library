unit uMemoryBenchmark;

interface

uses
  uTypes,
  uProjectVersion,
  uBenchmark;

type
  TMemoryBenchmark = class(TBenchmark)
  private
    FBlockSize: UG;
    procedure SetBlockSize(const Value: UG);
  protected
    function GetName: string; override;
    function GetVersion: TProjectVersion; override;
    procedure Execute; override;
  public
    constructor Create;

    // Input
    property BlockSize: UG read FBlockSize write SetBlockSize;
  end;

implementation

uses
  uOutputFormat,
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

  mov ecx, U4 ptr Count
  mov edi, U4 ptr PMem
  @Loop: // 4 clocks
    mov eax, ecx
    and eax, MaxMem4
    sub ecx, 1
    mov [edi+4*eax], ebx
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
const
  TotalStored = 64 * MB;
var
	Count, MaxMemoryAddress: UG;
	PData: Pointer;
begin
  inherited;

  MaxMemoryAddress := MaxDiv(FBlockSize, SizeOf(Pointer)) - 1;
  GetMem(PData, FBlockSize);
  try
    CalculatedItems := 0;
    while not Terminated do
    begin
      Count := TotalStored div SizeOf(Pointer);
      Loop(PData, MaxMemoryAddress, Count);
      CalculatedItems := CalculatedItems + TotalStored;
    end;
  finally
    FreeMem(PData);
	end;
end;

function TMemoryBenchmark.GetName: string;
begin
  Result := 'Memory block ' + BToStr(FBlockSize);
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

end.
