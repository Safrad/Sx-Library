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
  SysUtils,
	uTypes, uFind;

{ TFindTest }

procedure TFindTest.TestHamming;
var
  i: SG;
begin
  for i := 0 to 99999 do
  begin
    Check(SearchHamming('dog', 'adogb') = 2);
    Check(SearchHamming('dog', 'adobb') = 0);
    Check(SearchHamming('dog', 'adobb', 1) = 2);
  //  Check(SearchHamming('dog', 'adgb', 1) = 2);
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
