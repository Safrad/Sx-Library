unit uCompareTest;

interface

uses TestFrameWork;

type
  TCompareTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
	uTypes, uCompare, uFiles, uStrings;

{ TCompareTest }

procedure TCompareTest.Test;
var
  Texts: array[0..2] of string;
  i: SG;
  r: string;
begin
  for i := 0 to Length(Texts) - 1 do
  begin
	  Texts[i] := ReplaceF(ReadStringFromFile(DataDir + 'Text' + IntToStr(i + 1) + '.txt'), FullSep, LineSep);
  end;

  r := CompareTexts(Texts[0], Texts[1], True);
//  WriteStringToFile(DataDir + '$Result.txt', r, False);
  Check(r = Texts[2]);

//  r := CompareTexts(Texts[0], Texts[1], False, True);
//  WriteStringToFile(DataDir + '$Result.html', r, False);
end;

initialization
	RegisterTest('Compare Test', TCompareTest.Suite);
end.
