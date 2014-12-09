unit uOutputFormatTest;

interface

uses TestFrameWork;

type
  TOutputFormatTest = class(TTestCase)
  published
    procedure Test;
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

function TestNumber: Extended;
const
  SampleNumbers: array[0..MaxNumber] of Extended = (0, 1, 0.1, 10, 0.123456789012345678, 1e20, 1e1000, 1e-1000, 0.123456789012345678e-100, 0.123456789012345678e100, Infinity);
begin
  Result := SampleNumbers[Index div 2];
  if Index and 1 <> 0 then
  	Result := -Result;
	Inc(Index);
end;

procedure TOutputFormatTest.Test;
var
  s1, s2: string;
  e: Extended;
  i: Integer;
begin
  CheckEquals('1000.789', FToS(1000.789, ofIO));
  CheckEquals('2' + ThousandSeparator + '451' + ThousandSeparator + '000' + DecimalSeparator + '789', FToS(2451000.789, ofDisplay));
  CheckEquals('1000', FToS(1000, ofIO));
  CheckEquals('2' + ThousandSeparator + '451' + ThousandSeparator + '000', FToS(2451000, ofDisplay));
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
