unit uOutputFormatTest;

interface

uses TestFrameWork;

type
  TOutputFormatTest = class(TTestCase)
  published
    procedure TestFToS;
    procedure TestFormatAndFloatToStr;
    procedure TestAddThousandSeparators;
  end;

implementation

uses
  uOutputFormat,
  Math,
  uTypes,
  uStrings,
	SysUtils;

const
  MaxNumber = 10;
var
  Index: SG;

function TestNumber: FG;
const
  SampleNumbers: array[0..MaxNumber] of FG = (0, 1, 0.1, 10, 0.123456789012345678, 1e20, 1e1000, 1e-1000, 0.123456789012345678e-100, 0.123456789012345678e100, Infinity);
begin
  Result := SampleNumbers[Index div 2];
  if Index and 1 <> 0 then
  	Result := -Result;
	Inc(Index);
end;

procedure TOutputFormatTest.TestFToS;
begin
  CheckEquals('1000.789', FToS(1000.789, ofIO));
  CheckEquals('2' + ThousandSeparator + '451' + ThousandSeparator + '000' + DecimalSeparator + '789', FToS(2451000.789, ofDisplay));
  CheckEquals('1000', FToS(1000, ofIO));
  CheckEquals('2' + ThousandSeparator + '451' + ThousandSeparator + '000', FToS(2451000, ofDisplay));
end;

procedure TOutputFormatTest.TestAddThousandSeparators;
var
  i: SG;
  s: string;
begin
  CheckEquals('', AddThousandSeparators('', IOFormatSettings, 'e'));
  CheckEquals('1', AddThousandSeparators('1', IOFormatSettings, 'e'));
  CheckEquals('1.7784', AddThousandSeparators('1.7784', IOFormatSettings, 'e'));
  CheckEquals('10.7784', AddThousandSeparators('10.7784', IOFormatSettings, 'e'));
  CheckEquals('100.7784', AddThousandSeparators('100.7784', IOFormatSettings, 'e'));
  CheckEquals('1,000.7784', AddThousandSeparators('1000.7784', IOFormatSettings, 'e'));
  CheckEquals('10,000.7784', AddThousandSeparators('10000.7784', IOFormatSettings, 'e'));
  CheckEquals('100,000.7784', AddThousandSeparators('100000.7784', IOFormatSettings, 'e'));
  CheckEquals('1,000,000.7784', AddThousandSeparators('1000000.7784', IOFormatSettings, 'e'));
  CheckEquals('', AddThousandSeparators('', IOFormatSettings, 'e'));
  CheckEquals('-1', AddThousandSeparators('-1', IOFormatSettings, 'e'));
  CheckEquals('-1.7784', AddThousandSeparators('-1.7784', IOFormatSettings, 'e'));
  CheckEquals('-10.7784', AddThousandSeparators('-10.7784', IOFormatSettings, 'e'));
  CheckEquals('-100.7784', AddThousandSeparators('-100.7784', IOFormatSettings, 'e'));
  CheckEquals('-1,000.7784', AddThousandSeparators('-1000.7784', IOFormatSettings, 'e'));
  CheckEquals('-10,000.7784', AddThousandSeparators('-10000.7784', IOFormatSettings, 'e'));
  CheckEquals('-100,000.7784', AddThousandSeparators('-100000.7784', IOFormatSettings, 'e'));
  CheckEquals('-1,000,000.7784', AddThousandSeparators('-1000000.7784', IOFormatSettings, 'e'));
  CheckEquals('- 100,000.7784', AddThousandSeparators('- 100000.7784', IOFormatSettings, 'e'));
  CheckEquals('- 100,000.7784e10', AddThousandSeparators('- 100000.7784e10', IOFormatSettings, 'e'));
  CheckEquals('- 100,000e10', AddThousandSeparators('- 100000e10', IOFormatSettings, 'e'));
  s := StringOfChar('7', 9999);
  for i := 0 to 999 do
  begin
    CheckEquals('1,000,000', AddThousandSeparators('1000000', IOFormatSettings, 'e'));
    AddThousandSeparators(s, IOFormatSettings, 'e');
  end;
end;

procedure TOutputFormatTest.TestFormatAndFloatToStr;
var
  s1, s2: string;
  e: FG;
  i: Integer;
begin
  Index := 0;
	for i := 0 to MaxNumber do
  begin
    e := TestNumber;
    s1 := ReplaceF(Format('%g', [e], IOFormatSettings), 'E0', 'E');
    s2 := FloatToStr(e, IOFormatSettings);
    CheckEquals(s1, s2, 'Output format is wrong.');
  end;
end;

initialization
	RegisterTest('OutputFormat Test', TOutputFormatTest.Suite);
end.
