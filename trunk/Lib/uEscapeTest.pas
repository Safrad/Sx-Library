unit uEscapeTest;

interface

implementation

procedure Test;
var
	i: SG;
	s, s2: string;
begin
	for i := 0 to 32767 do
	begin
		s := RandomString(i);
		s2 := AddEscape(s);
		s2 := RemoveEscape(s2);
		Assert(s = s2);
	end;
end;

initialization
	Test;
end.
