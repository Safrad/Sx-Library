unit uFindTest;

interface

uses TestFrameWork;

type
  TFindTest = class(TTestCase)
  published
    procedure TestHamming;
    procedure TestLevenshtein;
//    procedure TestLevenshtein2;
  end;

implementation

uses
	uTypes,
  uHammingDistance,
  uLevenshteinDistance;

{ TFindTest }

procedure TFindTest.TestHamming;
var
  i, j: SG;
  HammingDistance: THammingDistance;
begin
  HammingDistance := THammingDistance.Create;
  try
    for i := 0 to 99999 do
    begin
      HammingDistance.Pattern := 'dog';

      HammingDistance.Text := 'adogb';
      HammingDistance.ErrorLen := 0;
      HammingDistance.Update;
      Check(HammingDistance.Distance = 2);

      HammingDistance.Text := 'adobb';
      HammingDistance.ErrorLen := 0;
      HammingDistance.Update;
      Check(HammingDistance.Distance = 0);

      HammingDistance.Text := 'adobb';
      HammingDistance.ErrorLen := 1;
      HammingDistance.Update;
      Check(HammingDistance.Distance = 2);

      HammingDistance.Text := 'adgb';
      HammingDistance.ErrorLen := 2;
      HammingDistance.Update;
      Check(HammingDistance.Distance = 1);

      for j := 2 to THammingDistance.MaximalDistance do
      begin
        HammingDistance.Text := 'adgb';
        HammingDistance.ErrorLen := j;
        HammingDistance.Update;
        Check(HammingDistance.Distance = 1);
      end;

    //  Check(SearchHamming('dog', 'adgb', 1) = 2);
    end;
  finally
    HammingDistance.Free;
  end;
end;

procedure TFindTest.TestLevenshtein;
var
  i: SG;
begin
  for i := 0 to 99999 do
  begin
    Check(LevenshteinDistance('dog', 'abdoged') = 4);
    Check(LevenshteinDistance('dog', 'dog') = 0);
    Check(LevenshteinDistance('dog', 'dob') = 1);
    Check(LevenshteinDistance('dog', 'dob') = 1);
    Check(LevenshteinDistance('dog', 'dg') = 1);
    Check(LevenshteinDistance('fish', 'aaac') = 4);
  end;
end;
{
procedure TFindTest.TestLevenshtein2;
var
  i: SG;
begin
  for i := 0 to 99999 do
  begin
    Check(EditDistance('dog', 'abdoged') = 4);
    Check(EditDistance('dog', 'dog') = 0);
    Check(EditDistance('dog', 'dob') = 1);
    Check(EditDistance('dog', 'dob') = 1);
    Check(EditDistance('dog', 'dg') = 1);
  end;
end;
}
initialization
	RegisterTest('Find Test', TFindTest.Suite);
end.
