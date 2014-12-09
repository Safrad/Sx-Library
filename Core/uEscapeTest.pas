unit uEscapeTest;

interface

uses TestFrameWork;

type
  TEscapeTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uTypes, uStrings, uEscape;

{ TEscapeTest }

procedure TEscapeTest.Test;
var
	i: SG;
	s, s2: string;
begin
	for i := 0 to 99 do
	begin
		s := RandomString(i);
		s2 := AddEscape(s);
		s2 := RemoveEscape(s2);
    Check(s = s2, 'AddEscape & RemoveEscape is not compatible');
	end;
end;

initialization
	RegisterTest('Escape Test', TEscapeTest.Suite);
end.
