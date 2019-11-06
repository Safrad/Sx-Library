unit uColorSequenceTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TColorSequenceTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  UITypes,
  uColor,
  uColorSequence;

{ TColorSequenceTest }

procedure TColorSequenceTest.Test;
var
  C1, C2: TColor;
	ColorSequence: TColorSequence;
  i: SG;
begin
	ColorSequence := TColorSequence.Create;
  try
    ColorSequence.Background := $ffffff;

    ColorSequence.Count := 1;
    C1 := ColorSequence.GetNextColor;
    ColorSequence.Reset;
    C2 := ColorSequence.GetNextColor;
    CheckEquals(C1, C2, 'The same color after reset required.');

    ColorSequence.Reset;
    ColorSequence.FirstColor := $7f0f7f;
    ColorSequence.Count := 12;
    for i := 0 to 11 do
      ColorSequence.GetNextColor;

    ColorSequence.Reset;
    ColorSequence.Count := 12;
    for i := 0 to 11 do
      ColorSequence.GetNextColor;
	finally
  	ColorSequence.Free;
	end;
end;

initialization
	RegisterTest('Color Sequence Test', TColorSequenceTest.Suite);
end.
