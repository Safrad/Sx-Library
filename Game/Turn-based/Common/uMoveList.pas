unit uMoveList;

interface

uses
  uTypes,

  uMove;

type
  PMoveList = ^TMoveList;
  TMoveList = record // Data ok, Size wrong
  public
    Size: UG;
    Count: UG;
    LastMoveList: PMoveList;
    Index: UG;
    function FirstMove: PMove;
    function NextMove: PMove;
    procedure Clear;
    function Add(const AMoveSize: SG): PMove;
    function Get(const AIndex: SG): PMove;
    procedure SetStartMovePointer(const AMove: PMove);
  private
    FStartMovePointer: PMove;
    FGetMovePointer: PMove;
    FAddMovePointer: PMove;
    // … Moves
  end;

implementation

{ TMoveList }

function TMoveList.Add(const AMoveSize: SG): PMove;
begin
  Assert(Index = 0);
  Assert(FAddMovePointer <> nil);
  Result := FAddMovePointer;
  FAddMovePointer^.Size := AMoveSize;
  Inc(FAddMovePointer, AMoveSize);
  Inc(Size, AMoveSize);
  Inc(Count);
  Assert(Index = 0);
end;

procedure TMoveList.Clear;
begin
  Assert(Index = 0);
  FAddMovePointer := FStartMovePointer;
  Count := 0;
  Size := SizeOf(TMoveList);
end;

function TMoveList.FirstMove: PMove;
begin
  if Count > 0 then
  begin
    FGetMovePointer := FStartMovePointer;
    Result := FGetMovePointer;
    Inc(FGetMovePointer, FGetMovePointer^.Size);
    Index := 0;
  end
  else
  begin
    Result := nil;
  end;
end;

function TMoveList.Get(const AIndex: SG): PMove;
begin
  Result := FStartMovePointer;
  Inc(Result, Result^.Size * AIndex);
end;

function TMoveList.NextMove: PMove;
begin
  Inc(Index);
  Assert(FGetMovePointer <> nil);
  if Index < Count then
  begin
    Result := FGetMovePointer;
    Inc(FGetMovePointer, FGetMovePointer^.Size);
  end
  else
    Result := nil;
end;

procedure TMoveList.SetStartMovePointer(const AMove: PMove);
begin
  FStartMovePointer := AMove;
  FAddMovePointer := AMove;
end;

end.
