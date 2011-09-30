unit uCharsetTest;

interface

uses TestFrameWork;

type
  TCharsetTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses uTypes, uCharset;

{ TCharsetTest }

procedure TCharsetTest.Test;
var
	a: AnsiString;
	w: UnicodeString;
begin
	// Tests
	a := 'äáèïéìí¾òóôàøšúùı';
	ConvertCharset(a, cp1250, cp852);
	Assert(a = '„ ŸÔ‚Ø¡–å¢“êıçœ£…ì§');

	a := 'ñ';
	ConvertCharset(a, cp1250, cp852);
	Assert(a = 'ä');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	ConvertCharset(a, cp1250, cp852);
	Assert(a = 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§');

	a := 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§';
	ConvertCharset(a, cp852, cp1250);
	Assert(a = 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı');

	a := 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾';
	ConvertCharset(a, cpISO88592, cp1250);
	Assert(a = 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	ConvertCharset(a, cp1250, cpISO88592);
	Assert(a = 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾');

	a := 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§';
	ConvertCharset(a, cp852, cpISO88592);
	Assert(a = 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾');

	a := 'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾';
	ConvertCharset(a, cpISO88592, cp852);
	Assert(a = 'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§');

	a := 'Frühauf David';
	ConvertCharset(a, cp1250, cpAscii);
	Assert(a = 'Fruhauf David');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	ConvertCharset(a, cp1250, cpAscii);
	Assert(a = 'ACDEEINORSTUUYZacdeeinorstuuyz');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	a := ConvertToAscii(a);
	Assert(a = 'ACDEEINORSTUUYZacdeeinorstuuyz');

	w := 'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı';
	a := ConvertToAscii(w);
	Assert(a = 'ACDEEINORSTUUYZacdeeinorstuuyz');

end;

initialization
	RegisterTest('Charset Test', TCharsetTest.Suite);
end.
