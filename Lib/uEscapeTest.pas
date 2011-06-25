//* File:     Lib\uEscapeTest.pas
//* Created:  2007-05-20
//* Modified: 2007-05-20
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

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
