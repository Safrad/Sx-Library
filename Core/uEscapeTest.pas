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
	Original, Escaped, Unescaped: string;
begin
	for i := 0 to 99 do
	begin
		Original := RandomString(i);
		Escaped := AddEscape(Original);
		Unescaped := RemoveEscape(Escaped);
    Check(Original = Unescaped, 'AddEscape & RemoveEscape is not compatible');
	end;
end;

initialization
	RegisterTest('Escape Test', TEscapeTest.Suite);
end.
