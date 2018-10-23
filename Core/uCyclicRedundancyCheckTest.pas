unit uCyclicRedundancyCheckTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TCyclicRedundancyCheckTest = class(TTestCase)
  private
    const
      FSampleArray: array[0..9] of Byte = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  published
    procedure CountOnesComplementTest;
    procedure CountCyclicRedundancyCheck8Test;
    procedure CountCyclicRedundancyCheck16Test;
    procedure CountCyclicRedundancyCheck32Test;
    procedure UpdateCyclicRedundancyCheck32Test;

    procedure SplitCountOnesComplementTest;
    procedure SplitCountCyclicRedundancyCheck8Test;
    procedure SplitCountCyclicRedundancyCheck16Test;
    procedure SplitCountCyclicRedundancyCheck32Test;

    procedure CyclicRedundancyCheck32LookupTableTest;
  end;

implementation

uses
  uCyclicRedundancyCheck;

{ TCyclicRedundancyCheckTest }

procedure TCyclicRedundancyCheckTest.CountCyclicRedundancyCheck16Test;
var
  Actual: U2;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck16(@FSampleArray, Length(FSampleArray), $ffff);
  CheckEquals(50099, Actual);
end;

procedure TCyclicRedundancyCheckTest.CountCyclicRedundancyCheck32Test;
var
  Actual: U4;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(@FSampleArray, Length(FSampleArray), 0);
  CheckEquals(3333046029, Actual);
end;

procedure TCyclicRedundancyCheckTest.CountCyclicRedundancyCheck8Test;
var
  Actual: U1;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck8(@FSampleArray, Length(FSampleArray));
  CheckEquals(182, Actual);
end;

procedure TCyclicRedundancyCheckTest.CountOnesComplementTest;
var
  Actual: U1;
begin
  Actual := TCyclicRedundancyCheck.OnesComplement(@FSampleArray, Length(FSampleArray));
  CheckEquals(55, Actual);
end;

procedure TCyclicRedundancyCheckTest.CyclicRedundancyCheck32LookupTableTest;
var
	CRC: U4;
	n, k: SG;
begin
	for n := 0 to 255 do
	begin
		CRC := n;
		for k := 0 to 7 do
		begin
			if Boolean(CRC and 1) then
				CRC := $edb88320 xor (CRC shr 1) // CRC-32-IEEE 802.3
			else
				CRC := CRC shr 1;
		end;
    CheckEquals(CRC, CyclicRedundancyCheck32LookupTable[n]);
	end;
end;

procedure TCyclicRedundancyCheckTest.SplitCountCyclicRedundancyCheck16Test;
var
  Actual: U2;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck16(@FSampleArray, Length(FSampleArray) div 2);
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck16(@FSampleArray[5], Length(FSampleArray) div 2, Actual);
  CheckEquals(TCyclicRedundancyCheck.CountCyclicRedundancyCheck16(@FSampleArray, Length(FSampleArray)), Actual);
end;

procedure TCyclicRedundancyCheckTest.SplitCountCyclicRedundancyCheck32Test;
var
  Actual: U4;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(@FSampleArray, Length(FSampleArray) div 2);
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(@FSampleArray[5], Length(FSampleArray) div 2, Actual);
  CheckEquals(TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(@FSampleArray, Length(FSampleArray)), Actual);
end;

procedure TCyclicRedundancyCheckTest.SplitCountCyclicRedundancyCheck8Test;
var
  Actual: U1;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck8(@FSampleArray, Length(FSampleArray) div 2);
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck8(@FSampleArray[5], Length(FSampleArray) div 2, Actual);
  CheckEquals(TCyclicRedundancyCheck.CountCyclicRedundancyCheck8(@FSampleArray, Length(FSampleArray)), Actual);
end;

procedure TCyclicRedundancyCheckTest.SplitCountOnesComplementTest;
var
  Actual: U1;
begin
  Actual := TCyclicRedundancyCheck.OnesComplement(@FSampleArray, Length(FSampleArray) div 2);
  Actual := TCyclicRedundancyCheck.OnesComplement(@FSampleArray[5], Length(FSampleArray) div 2, Actual);
  CheckEquals(TCyclicRedundancyCheck.OnesComplement(@FSampleArray, Length(FSampleArray)), Actual);
end;

procedure TCyclicRedundancyCheckTest.UpdateCyclicRedundancyCheck32Test;
const
  Previous = 1;
var
  Actual: U4;
begin
  Actual := TCyclicRedundancyCheck.CountCyclicRedundancyCheck32(@FSampleArray, Length(FSampleArray), Previous);
  CheckEquals(694703155, Actual);
end;

initialization
	RegisterTest('Cyclic Redundancy Check Test', TCyclicRedundancyCheckTest.Suite);
end.
