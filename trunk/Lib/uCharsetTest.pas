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
	a := 'äáèïéìí¾òóôàøšúùýž';
	ConvertCharset(a, cp1250, cp852);
	Check(a = '„ ŸÔ‚Ø¡–å¢“êýçœ£…ì§', 'cp1250 -> cp852');

	a := 'ñ';
	ConvertCharset(a, cp1250, cp852);
	Check(a = 'ä', 'cp1250 -> cp852');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž';
	ConvertCharset(a, cp1250, cp852);
	Check(a = 'µ¬Ò·ÖÕàüæ›éÞí¦ ŸÔ‚Ø¡å¢ýçœ£…ì§', 'cp1250 -> cp852');

	a := 'µ¬Ò·ÖÕàüæ›éÞí¦ ŸÔ‚Ø¡å¢ýçœ£…ì§';
	ConvertCharset(a, cp852, cp1250);
	Check(a = 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž', 'cp852 -> cp1250');

	a := 'ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®áèïéìíòóø¹»úùý¾';
	ConvertCharset(a, cpISO88592, cp1250);
	Check(a = 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž', 'cpISO8859-2 -> cp1250');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž';
	ConvertCharset(a, cp1250, cpISO88592);
	Check(a = 'ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®áèïéìíòóø¹»úùý¾', 'cp1250 -> cpISO8859-2');

	a := 'µ¬Ò·ÖÕàüæ›éÞí¦ ŸÔ‚Ø¡å¢ýçœ£…ì§';
	ConvertCharset(a, cp852, cpISO88592);
	Check(a = 'ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®áèïéìíòóø¹»úùý¾', 'cp852 -> cpISO8859-2');

	a := 'ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®áèïéìíòóø¹»úùý¾';
	ConvertCharset(a, cpISO88592, cp852);
	Check(a = 'µ¬Ò·ÖÕàüæ›éÞí¦ ŸÔ‚Ø¡å¢ýçœ£…ì§', 'cpISO8859-2 -> cp852');

	a := 'Frühauf David';
	ConvertCharset(a, cp1250, cpAscii);
	Check(a = 'Fruhauf David', 'cp1250 -> cpAscii');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž';
	ConvertCharset(a, cp1250, cpAscii);
	Check(a = 'ACDEEINORSTUUYZacdeeinorstuuyz', 'cp1250 -> cpAscii');

	a := 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž';
	a := ConvertToAscii(a);
	Check(a = 'ACDEEINORSTUUYZacdeeinorstuuyz', 'cpLocal -> cpAscii');

	w := 'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž';
	a := ConvertToAscii(w);
	Check(a = 'ACDEEINORSTUUYZacdeeinorstuuyz', 'cpLocal -> cpAscii');

end;

initialization
	RegisterTest('Charset Test', TCharsetTest.Suite);
end.
