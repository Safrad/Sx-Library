unit uPercentFormatterTest;

interface

uses
  TestFrameWork;

type
  TPercentFormatterTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uPercentFormatter,
  uStrings,
  uChar;

{ TPercentFormatterTest }

procedure TPercentFormatterTest.Test;
const
  PercentSuffix = CharUnbrokableSpace + '%';
  PerMilleSuffix = CharUnbrokableSpace + '‰';
var
  PercentFormatter: TPercentFormatter;
begin
	PercentFormatter := TPercentFormatter.Create;
  try
    CheckEquals('12,5' + CharTimes, PercentFormatter.Format(12.54));

    CheckEquals('754' + PercentSuffix, PercentFormatter.Format(7.54));
    CheckEquals('100' + PercentSuffix, PercentFormatter.Format(1));
    CheckEquals('100' + PercentSuffix, PercentFormatter.Format(1.0));
    CheckEquals('77,5' + PercentSuffix, PercentFormatter.Format(0.77532));
    CheckEquals('77,5'+ PercentSuffix, PercentFormatter.Format(0.775));
    CheckEquals('1' + PercentSuffix, PercentFormatter.Format(0.01));

    CheckEquals('5' + PerMilleSuffix, PercentFormatter.Format(0.005));
    CheckEquals('1' + PerMilleSuffix, PercentFormatter.Format(0.001));
    CheckEquals('0,1' + PerMilleSuffix, PercentFormatter.Format(0.0001));

    CheckEquals('0' + PercentSuffix, PercentFormatter.Format(0));

    CheckEquals('-1' + PerMilleSuffix, PercentFormatter.Format(-0.001));
    CheckEquals('-77,5' + PercentSuffix, PercentFormatter.Format(-0.77532));
    CheckEquals('-12,5' + CharTimes, PercentFormatter.Format(-12.54));
	finally
  	PercentFormatter.Free;
	end;
end;

initialization
	RegisterTest('Percent Formatter Test', TPercentFormatterTest.Suite);
end.
