unit uIntegerListTest;

interface

uses TestFrameWork;

type
  TIntegerListTest = class(TTestCase)
  published
    procedure Test;
    procedure TestAddSpeed;
    procedure TestDeleteSpeed;
  end;

implementation

uses
  uTypes,
  uIntegerList;

procedure TIntegerListTest.Test;
var
  IntegerList: TIntegerList<S2>;
begin
  IntegerList := TIntegerList<S2>.Create;
  try
    IntegerList.Add(10);
    IntegerList.Add(7);
    CheckEquals(1, IntegerList.FindValue(7, 0));
  finally
    IntegerList.Free;
  end;
end;

procedure TIntegerListTest.TestAddSpeed;
const
  Count = 100000000;
var
  IntegerList: TIntegerList<S4>;
  i: SG;
begin
  IntegerList := TIntegerList<S4>.Create;
  try
    for i := 0 to Count - 1 do
    begin
      IntegerList.Add(5498496);
    end;
  finally
    IntegerList.Free;
  end;
end;

procedure TIntegerListTest.TestDeleteSpeed;
const
  Count = 100000000;
var
  IntegerList: TIntegerList<S4>;
  i: SG;
begin
  IntegerList := TIntegerList<S4>.Create;
  try
    for i := 0 to Count - 1 do
    begin
      IntegerList.Add(5498496);
      IntegerList.Delete(0);
    end;    
  finally
    IntegerList.Free;
  end;
end;

initialization
	RegisterTest('Integer List Test', TIntegerListTest.Suite);
end.
