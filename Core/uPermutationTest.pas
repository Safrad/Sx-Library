unit uPermutationTest;

interface

uses
  TestFrameWork;

type
  TPermutationTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uPermutation,
  uPermutationList;

{ TPermutationTest }

procedure TPermutationTest.Test;
var
  Permutation: TPermutation;
  PermutationList: TPermutationList;
begin
  Permutation := nil;
  PermutationList := nil;
  try
    Permutation := TPermutation.Create;
    PermutationList := TPermutationList.Create(True);
    Permutation.Count := 0;
    CheckEquals(1, Permutation.PermutationCount);
    CheckEquals(0, Length(Permutation.Order));

    Permutation.Count := 1;
    CheckEquals(1, Permutation.PermutationCount);
    CheckEquals(0, Permutation.Order[0]);

    Permutation.Count := 2;
    CheckEquals(2, Permutation.PermutationCount);

    CheckEquals(0, Permutation.Order[0]);
    CheckEquals(1, Permutation.Order[1]);
    CheckEquals(True, Permutation.Next);

    CheckEquals(1, Permutation.Order[0]);
    CheckEquals(0, Permutation.Order[1]);
    CheckEquals(False, Permutation.Next);

    Permutation.Count := 3;
    CheckEquals(6, Permutation.PermutationCount);

    CheckEquals(0, Permutation.Order[0]);
    CheckEquals(1, Permutation.Order[1]);
    CheckEquals(2, Permutation.Order[2]);
    CheckEquals(True, Permutation.Next);

    CheckEquals(1, Permutation.Order[0]);
    CheckEquals(0, Permutation.Order[1]);
    CheckEquals(2, Permutation.Order[2]);
    CheckEquals(True, Permutation.Next);

    CheckEquals(1, Permutation.Order[0]);
    CheckEquals(2, Permutation.Order[1]);
    CheckEquals(0, Permutation.Order[2]);
    CheckEquals(True, Permutation.Next);

    CheckEquals(2, Permutation.Order[0]);
    CheckEquals(1, Permutation.Order[1]);
    CheckEquals(0, Permutation.Order[2]);
    CheckEquals(True, Permutation.Next);

    CheckEquals(2, Permutation.Order[0]);
    CheckEquals(0, Permutation.Order[1]);
    CheckEquals(1, Permutation.Order[2]);
    CheckEquals(True, Permutation.Next);

    CheckEquals(0, Permutation.Order[0]);
    CheckEquals(2, Permutation.Order[1]);
    CheckEquals(1, Permutation.Order[2]);
    CheckEquals(False, Permutation.Next);

    Permutation.Count := 4;
    CheckEquals(24, Permutation.PermutationCount);

    repeat
      CheckFalse(PermutationList.Exists(Permutation.Order));
      PermutationList.Add(Permutation.Order);
    until not Permutation.Next;

    Permutation.Count := 5;
    CheckEquals(120, Permutation.PermutationCount);

    PermutationList.Clear;
    repeat
      CheckFalse(PermutationList.Exists(Permutation.Order));
      PermutationList.Add(Permutation.Order);
    until not Permutation.Next;

    Permutation.Count := 6;
    CheckEquals(720, Permutation.PermutationCount);

    PermutationList.Clear;
    repeat
      CheckFalse(PermutationList.Exists(Permutation.Order));
      PermutationList.Add(Permutation.Order);
    until not Permutation.Next;
  finally
    PermutationList.Free;
    Permutation.Free;
  end;
end;

initialization
	RegisterTest('Permutation Test', TPermutationTest.Suite);
end.
