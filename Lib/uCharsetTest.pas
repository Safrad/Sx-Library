//* File:     Lib\uCharsetTest.pas
//* Created:  2001-12-01
//* Modified: 2007-05-06
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uCharsetTest;

interface

implementation

procedure Test;
var
	s: string;
begin
	// Tests
	s := 'äáèïéìí¾òóôàøšúùı';
	ConvertCharset(s, cp1250, cp852);
	Assert(s = '„ ŸÔ‚Ø¡–å¢“êıçœ£…ì§');

	s := 'ñ';
	ConvertCharset(s, cp1250, cp852);
	Assert(s = 'ä');

	s := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	ConvertCharset(s, cp1250, cp852);
	Assert(s = 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§');

	s := 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§';
	ConvertCharset(s, cp852, cp1250);
	Assert(s = 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı');

	s := 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾';
	ConvertCharset(s, cpISO88592, cp1250);
	Assert(s = 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı');

	s := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	ConvertCharset(s, cp1250, cpISO88592);
	Assert(s = 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾');

	s := 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§';
	ConvertCharset(s, cp852, cpISO88592);
	Assert(s = 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾');

	s := 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾';
	ConvertCharset(s, cpISO88592, cp852);
	Assert(s = 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§');

	s := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	ConvertCharset(s, cp1250, cpAscii);
	Assert(s = 'ACDEEINORSTUUYZacdeeinorstuuyz');

	s := 'Frühauf David';
	ConvertCharset(s, cp1250, cpAscii);
	Assert(s = 'Fruhauf David');
end;

initialization
	Test;
end.
