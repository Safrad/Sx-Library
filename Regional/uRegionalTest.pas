unit uRegionalTest;

interface

uses
  SysUtils,
  TestFrameWork;

type
  TRegionalTest = class(TTestCase)
  published
    procedure EasterMonday;
    procedure EasterDate;
  end;

implementation

uses
  uRegional,
  uInputFormat,
  DateUtils;

procedure TRegionalTest.EasterMonday;
var
  Year: Integer;
begin
  for Year := 1 to 3000 do
  begin
    CheckEquals(1, Integer(DayOfTheWeek(GetEasterMonday(Year))));
  end;
end;

procedure TRegionalTest.EasterDate;
begin
  CheckEquals(SToDateTime('2011-04-25', ifIO), GetEasterMonday(2011));
  CheckEquals(SToDateTime('2012-04-09', ifIO), GetEasterMonday(2012));
  CheckEquals(SToDateTime('2013-04-01', ifIO), GetEasterMonday(2013));
  CheckEquals(SToDateTime('2014-04-21', ifIO), GetEasterMonday(2014));
  CheckEquals(SToDateTime('2015-04-06', ifIO), GetEasterMonday(2015));
  CheckEquals(SToDateTime('2016-03-28', ifIO), GetEasterMonday(2016));
  CheckEquals(SToDateTime('2017-04-17', ifIO), GetEasterMonday(2017));
  CheckEquals(SToDateTime('2018-04-02', ifIO), GetEasterMonday(2018));
  CheckEquals(SToDateTime('2019-04-22', ifIO), GetEasterMonday(2019));
  CheckEquals(SToDateTime('2020-04-13', ifIO), GetEasterMonday(2020));
end;

initialization
	RegisterTest('Regional Test', TRegionalTest.Suite);
end.
