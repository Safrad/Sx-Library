unit uMoveStack;

interface

uses
  uTypes,
  uMoveList;

type
  TMoveStack = class
  private
    FMemory: Pointer; // Start Index of Free block of CMove
    FLastMoveList: PMoveList;
    const Size = 128 * KB; // Fixed size, can not be reallocated
  public
    constructor Create;
    destructor Destroy; override;
    function GetNewMoveList: PMoveList;
    procedure RemovePrevious;
  end;

implementation

{ TMoveStack }

constructor TMoveStack.Create;
begin
  inherited;

  GetMem(FMemory, Size);
  FLastMoveList := nil;
end;

destructor TMoveStack.Destroy;
begin
  FreeMem(FMemory);

  inherited;
end;

function TMoveStack.GetNewMoveList: PMoveList;
begin
  Assert(PByte(FLastMoveList) - PByte(FMemory) < Size div 2);
  if FLastMoveList = nil then
  begin
    Result := FMemory;
  end
  else
  begin
    Result := PMoveList(PByte(FLastMoveList) + FLastMoveList.Size);
  end;

  // Initialize new list
  Result^ := Default(TMoveList);
  Result.SetStartMovePointer(Pointer(SG(Result) + SizeOf(TMoveList)));
  Result.Size := SizeOf(TMoveList);
  Result.LastMoveList := FLastMoveList;
  FLastMoveList := Result;
end;

procedure TMoveStack.RemovePrevious;
begin
  Assert(FLastMoveList <> nil);
  if FLastMoveList = FMemory then
    FLastMoveList := nil
  else
  begin
    FLastMoveList := FLastMoveList.LastMoveList;
    Assert(PByte(FLastMoveList) >= PByte(FMemory));
  end;
end;

end.
