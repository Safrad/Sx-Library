unit uStringsTest;

interface

uses TestFrameWork;

type
  TStringsTest = class(TTestCase)
  published
    procedure TestMatch;
  end;

implementation

uses uStrings;

procedure TStringsTest.TestMatch;
begin
	Check(Match('dfd', '*') = True);
	Check(Match('', '*') = True);
	Check(Match('pefsd', 'p*') = True);
	Check(Match('epefsd', 'p*') = False);
//	Check(Match('pefsdp', '*p') = True);
//	Check(Match('pefsd', '*p') = False);
	Check(Match('apefsdp', '*pe*') = True);
	Check(Match('apfsd', '*pe*') = False);

	Check(FileMatch('pefsd.txt', '*.*') = True);
	Check(FileMatch('pefsd.txt', '*') = False);
end;

initialization
	RegisterTest('Strings Test', TStringsTest.Suite);
end.
