unit uCharset;

interface

uses uTypes;

type
	TCodePage =
		(cpAscii, cp1250, cp852, cpISO88592{, cpKeybCS2, cpMacCE,
		cpKOI8CS, cpkodxx, cpWFW_311, cpISO88591, cpT1, cpMEXSK, cpw311_cw,
		cpVavrusa, cpNavi}, cpUTF8{, cpUnicode});

const
	CodePageNames: array[TCodePage] of string =
		('ASCII', 'windows-1250'{Central European, Windows Latin 2}, 'IBM852'{DOS code page for Central European}, 'ISO-8859-2'{Central European, ISO Latin 2}, 'utf-8'{, 'utf-16'});

function ConvertUTF8ToUnicode(const s: AnsiString): UnicodeString;
function ConvertUnicodeToUTF8(const s: UnicodeString): AnsiString;

function ConvertToAscii(const s: AnsiString): AnsiString; overload;
function ConvertToAscii(const s: UnicodeString): AnsiString; overload;

procedure ConvertCharset(var s: AnsiString; const FromCharset, ToCharset: TCodePage);
function ConvertCharsetF(const s: AnsiString; const FromCharset, ToCharset: TCodePage): AnsiString;


var
	TableUpCaseCz: array[AnsiChar] of AnsiChar;

function UpCaseCz(const s: string): string;

implementation

uses SysUtils{$ifopt d+}, uCharsetTest{$endif};

type
{const
	CZX: array[TCodePage] of TCzLetters = (
		'ACDEEINORSTUUYZacdeeinorstuuyz', // ASCII
		'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı', // ANSI-CP1250
		'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§', // OEM-CP852 (LATIN 2)
		'ÁÈÏÉÌÍÒÓØ©«ÚÙİ®áèïéìíòóø¹»úùı¾', // ISO-8859-2
		'€…‰‹¥•›†—¦’ ‡ƒ‚ˆ¡¤¢©¨Ÿ£–˜‘', // KEYBCS2 (Kamenicky)
		'ç‰‘ƒêÅîÛáèòñøë‡‹“’Ë—Şäéœóùì', // MAC CE
		'áãäçåéîïòóôõêùúÁÃÄ×ÅÉÎÏÒÓÔÕÊÙÚ', // KOI8-CS
		'ÁÈÏÉÌÍÒ¡ØŠ¡Ùİáèïéìíòóøšúùı', // kodxx
		'ÁÈÏÉÌÍÒÓØŠÚ¡İ¡áèïéìíòóøšúùı', // WFW_3-11
		'0CDÉEÍ+ÓRSTÚUİZ®cdéeínórstúuız', // ISO-8859-1
		'ÁâëÉùÍ‰Ó¤·ËÚÒ¯ÎáãìéûíÀóŞ”ÈúÛ˜Ï', // T1
		'ÁÈÏÉÌÍÒÓØŠÚ¡İáèïéìíòóøšú¡ı', // MEXSK
		'Á%ÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı', // w311_ce
		'ÁÈÏÉÌÍÒÓØŠÚ¡İáèïìéíòóøšú¡ı', // vavrusa
		'ÁÈÏÉÌÍÒÓ+ŠÚ¡İ¡áèïéìíòóøšúùı'); // navi
	}

	TUpAscii = array[#128..#255] of AnsiChar;

var
	// [to, from]
	CodePage: array[cpAscii..cpISO88592, cp1250..cpISO88592] of TUpAscii = (
		(
		// to ASCII
		'E'+#$27+'ƒ".++ˆ%S<STZZ'+#$27+#$27+'"".--˜ts>stzz   LoA| cS<--RZ~+ l'+#$27+'u. as>L lzRAAAALCCCEEEEIIDDNNOOOOxRUUUUYTbraaaalccceeeeiiddnnoooo/ruuuuyt ', // from CP1250
		'CueaauccleOoiZACELlooLlSsOUTtLxcaiouAaZzEe-zCs<>   ||AAES||++Zz++++|-|Aa++==|=|odDDEdNIIe++  TU ObONnnSsRUrUyYt'+#$27+'-    / ~  uRr  ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ A LoLS SSTZ-ZZ~a l'+#$27+'ls  sstz zzRAAAALCCCEEEEIIDDNNOOOOxRUUUUYTbraaaalccceeeeiiddnnoooo/ruuuuyt '  // from ISO-8859-2
		)
		,
		(
		// to CP1250
		'€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ', // from CP1250
		'Çüéâäùæç³ëÕõîÄÆÉÅåôö¼¾ŒœÖÜ£×èáíóú¥¹Êê¬ŸÈº«»   ||ÁÂÌª||++¯¿++++|-|Ãã++==|=|¤ğĞÏËïÒÍÎì++  ŞÙ ÓßÔÑñòŠšÀÚàÛıİş´­½²¡¢§÷¸°¨ÿûØø  ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¥¢£¤¼Œ§¨Šª­¯°¹²³´¾œ¡¸šºŸ½¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ' // from ISO-8859-2
		)
		,
		(
		// to OEM-CP852 (LATIN 2)
		'E'+#$27+'ƒ".ÅÅˆ%æ<—›¦'+#$27+#$27+'"".--˜tç>˜œ§«ÿóôÏ¤|õùc¸®ªğR½ø+òˆïu.÷¥­¯•ñ–¾èµ¶Æ‘€¬¨Ó·Ö×ÒÑãÕàâŠ™üŞéëšíİáê ƒÇ„’†‡Ÿ‚©‰Ø¡ŒÔĞäå¢“‹”öı…£ûìîú', // from CP1250
		'€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸÿ¤ôÏ•—õùæ¸›ğ¦½ø¥òˆï–˜ó÷ç­œ«ñ§¾èµ¶Æ‘€¬¨Ó·Ö×ÒÑãÕàâŠ™üŞéëšíİáê ƒÇ„’†‡Ÿ‚©‰Ø¡ŒÔĞäå¢“‹”öı…£ûìîú' // from ISO-8859-2
		)
		,
		(
		// to ISO-8859-2
		'E'+#$27+'ƒ".++ˆ%©<¦«®¬'+#$27+#$27+'"".--˜t¹>¶»¾¼ ·¢£¤¡|§¨cª<-­R¯°+²³´u.¸±º>¥½µ¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ', // from CP1250
		'Çüéâäùæç³ëÕõî¬ÄÆÉÅåôö¥µ¦¶ÖÜ«»£×èáíóú¡±®¾Êê-¼Èº<>   ||ÁÂÌª||++¯¿++++|-|Ãã++==|=|¤ğĞÏËïÒÍÎì++  ŞÙ ÓßÔÑñò©¹ÀÚàÛıİş´­½²·¢§÷¸°¨ÿûØø  ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡ˆ‰Š‹Œ‘’“”•–—˜™š›œŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞßàáâãäåæçèéêëìíîïğñòóôõö÷øùúûüışÿ' // from ISO-8859-2
		)
	);


function ConvertUTF8ToUnicode(const s: AnsiString): UnicodeString;
begin
	SetLength(Result, 2 * Length(s));
	SetLength(Result, Utf8ToUnicode(PWideChar(Result), PAnsiChar(s), 2 * Length(s)) - 1);
end;

function ConvertUnicodeToUTF8(const s: UnicodeString): AnsiString;
var
	l: SG;
begin
//	Result := StringTo UTF8ToString(s;
	SetLength(Result, 2 * Length(s) + 1);
	l := UnicodeToUtf8(PAnsiChar(Result), Length(Result), PWideChar(s), Length(s));
	SetLength(Result, l - 1);
end;

function ConvertToAscii(const s: AnsiString): AnsiString;
begin
	Result := ConvertCharsetF(s, cp1250, cpAscii);
end;

function ConvertToAscii(const s: UnicodeString): AnsiString;
begin
	Result := ConvertToAscii(AnsiString(s));
end;

procedure ConvertCharset(var s: AnsiString; const FromCharset, ToCharset: TCodePage);
var
	i: SG;
begin
	Assert(FromCharset <> cpAscii);
	if FromCharset = ToCharset then Exit;

{	if (ToCharset = cpUnicode) then
	begin
		if (FromCharset <> cpUTF8) then
			ConvertCharset(s, FromCharset, cpUTF8);
		Utf8ToUnicode(PWideChar(PChar(s)), PAnsiChar(s), 2 * Length(s));
		Exit;
	end;}
{	if (ToCharset = cpUnicode) then
	begin
		if (FromCharset <> cpUTF8) then
			ConvertCharset(s, FromCharset, cpUTF8);
		s := PAnsiChar(ConvertUTF8ToUnicode(s));
		Exit;
	end;

	if (FromCharset = cpUnicode) then
	begin
		UnicodeToUtf8(PAnsiChar(s), PWideChar(PChar(s)), 2 * Length(s));
		if (ToCharset <> cpUTF8) then
			ConvertCharset(s, cpUTF8, ToCharset);
		Exit;
	end;}

	if (FromCharset = cpUTF8) then
	begin
		s := Utf8ToAnsi(s);
		if (ToCharset <> cp1250) then
			ConvertCharset(s, cp1250, ToCharset);
		Exit;
	end;

	if (ToCharset = cpUTF8) then
	begin
		if (FromCharset <> cp1250) then
			ConvertCharset(s, FromCharset, cp1250);
		s := AnsiToUtf8(s);
		Exit;
	end;

	for i := 1 to Length(s) do
	begin
		if Ord(s[i]) >= $80 then
			s[i] := CodePage[ToCharset, FromCharset][s[i]];
	end;
end;

function ConvertCharsetF(const s: AnsiString; const  FromCharset, ToCharset: TCodePage): AnsiString;
{var
	i: SG;}
begin
	Result := s;
	ConvertCharset(Result, FromCharset, ToCharset);
{	Assert(FromCharset <> cpAscii);
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		if Ord(s[i]) >= $80 then
			Result[i] := CodePage[ToCharset, FromCharset][s[i]]
		else
			Result[i] := s[i];
	end;}
end;

{$ifndef UNICODE}
procedure FillCharsTable;
var c, Result: AnsiChar;
begin
	for c := Low(c) to High(c) do
	begin
		// UpCaseCz
		case c of
		'a'..'z': Result := AnsiChar(Ord(c) - Ord('a') + Ord('A'));
		'á': Result := 'Á';
		'è': Result := 'È';
		'ï': Result := 'Ï';
		'é': Result := 'É';
		'ì': Result := 'Ì';
		'í': Result := 'Í';
		'ò': Result := 'Ò';
		'ó': Result := 'Ó';
		'ø': Result := 'Ø';
		'š': Result := 'Š';
		'': Result := '';
		'ú': Result := 'Ú';
		'ù': Result := 'Ù';
		'ı': Result := 'İ';
		'': Result := '';
		else Result := c;
		end;
		TableUpCaseCz[c] := Result;
	end;
end;
{$endif}

function UpCaseCz(const s: string): string;
{$ifndef UNICODE}
var i: Integer;
{$endif}
begin
	{$ifdef UNICODE}
	Result := Uppercase(s, loUserLocale);
	{$else}
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := Char(TableUpCaseCz[AnsiChar(s[i])])
	end;
	{$endif}
end;

initialization
	{$ifndef UNICODE}
	FillCharsTable;
	{$endif}
end.
