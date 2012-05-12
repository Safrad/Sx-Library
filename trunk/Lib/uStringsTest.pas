unit uStringsTest;

interface

implementation

uses uStrings;

procedure TestMatch;
begin
	Assert(Match('dfd', '*') = True);
	Assert(Match('', '*') = True);
	Assert(Match('pefsd', 'p*') = True);
	Assert(Match('epefsd', 'p*') = False);
//	Assert(Match('pefsdp', '*p') = True);
//	Assert(Match('pefsd', '*p') = False);
	Assert(Match('apefsdp', '*pe*') = True);
	Assert(Match('apfsd', '*pe*') = False);

	Assert(FileMatch('pefsd.txt', '*.*') = True);
	Assert(FileMatch('pefsd.txt', '*') = False);
end;

initialization
  TestMatch;
end.
