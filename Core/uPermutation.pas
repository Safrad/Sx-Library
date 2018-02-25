unit uPermutation;

interface

uses
  uTypes;

type
  TPermutation = class
  private
    FOrder: TArrayOfSG;
    FCount: SG;
    FChanged: TArrayOfSG;

    FPermutationIndex: SG;
    FPermutationCount: SG;
    procedure Init;
    procedure SetCount(const Value: SG);
  public
    constructor Create;
    function Next: BG;

    property Order: TArrayOfSG read FOrder;
    property PermutationCount: SG read FPermutationCount;
    property Count: SG read FCount write SetCount;
  end;

implementation

uses
  Math,
  uMath;

{ TPermutation }

constructor TPermutation.Create;
begin
  inherited;

  Init;
end;

procedure TPermutation.Init;
var
  i: SG;
begin
  SetLength(FOrder, 0);
  SetLength(FOrder, FCount);
  SetLength(FChanged, 0);
  SetLength(FChanged, FCount);
  for i := 0 to FCount - 1 do
  begin
    FOrder[i] := i;
  end;
  FPermutationIndex := 0;
  FPermutationCount := Factorial(FCount);
end;

function TPermutation.Next: BG;
var
  Index: SG;
begin
  if FPermutationIndex + 1 >= FPermutationCount then
  begin
    Result := False;
    Exit;
  end;

  if FCount <= 3 then
  begin
    // Fast version
    Index := FPermutationIndex mod (FCount - 1);
    Exchange(FOrder[Index], FOrder[Index + 1]);
  end
  else
  begin
    // General version
    // https://en.wikipedia.org/wiki/Heap's_algorithm
    Index := 0;
    while Index < FCount do
    begin
      if FChanged[Index] < Index then
      begin
        if Index mod 2 = 0 then
          Exchange(FOrder[0], FOrder[Index])
        else
          Exchange(FOrder[FChanged[Index]], FOrder[Index]);

        Inc(FChanged[Index]);
        Break;
      end
      else
      begin
        FChanged[Index] := 0;
        Inc(Index);
      end;
    end;
  end;

  Inc(FPermutationIndex);
  Result := True;
end;

procedure TPermutation.SetCount(const Value: SG);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    Init;
  end;
end;

end.
