unit uCharset;

interface

type
	TCodePage = (cpAscii, cp1250, cp852, cpISO88592{, cpKeybCS2, cpMacCE,
		cpKOI8CS, cpkodxx, cpWFW_311, cpISO88591, cpT1, cpMEXSK, cpw311_cw,
		cpVavrusa, cpNavi});
var
	TableUpCaseCz{,
	TableDelCz,
	TableDosCzToWin,
	TableWinCzSkToDos,
	TableWinPlToDos,
	TableWinHuToDos}: array[Char] of Char;

procedure ConvertCharset(var s: string; FromCharset, ToCharset: TCodePage); overload;
function ConvertCharsetF(const s: string; FromCharset: TCodePage; ToCharset: TCodePage): string; overload;

function UpCaseCz(const s: string): string;
function DelCz(const s: string): string;

implementation

uses uTypes;

type
{const
	CZX: array[TCodePage] of TCzLetters =
	 ('ACDEEINORSTUUYZacdeeinorstuuyz', // ASCII
		'ΑΘΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ', // ANSI-CP1250
		'µ¬Ò·ΦΥΰόζ›ιήν¦ Τ‚Ψ΅εΆύη£…μ§', // OEM-CP852 (LATIN 2)
		'ΑΘΟΙΜΝÒΣΨ©«ΪΩέ®αθοιμνςσψΉ»ϊωύΎ', // ISO-8859-2
		'€…‰‹¥•›†—¦’ ‡ƒ‚΅¤Ά©¨£–‘', // KEYBCS2 (Kamenicky)
		'η‰‘ƒκΕξΫαθςρψλ‡‹“’Λ—ήδισωμ', // MAC CE
		'αγδηειξοςστυκωϊΑΓΔΧΕΙΞΟÒΣΤΥΚΩΪ', // KOI8-CS
		'ΑΘΟΙΜΝÒ΅Ψ΅Ωέαθοιμνςσψϊωύ', // kodxx
		'ΑΘΟΙΜΝÒΣΨΪ΅έ΅αθοιμνςσψϊωύ', // WFW_3-11
		'0CDΙEΝ+ΣRSTΪUέZ®cdιeνnσrstϊuύz', // ISO-8859-1
		'ΑβλΙωΝ‰Σ¤·ΛΪÒ―Ξαγμιϋνΐσή”ΘϊΫΟ', // T1
		'ΑΘΟΙΜΝÒΣΨΪ΅έαθοιμνςσψϊ΅ύ', // MEXSK
		'Α%ΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ', // w311_ce
		'ΑΘΟΙΜΝÒΣΨΪ΅έαθομινςσψϊ΅ύ', // vavrusa
		'ΑΘΟΙΜΝÒΣ+Ϊ΅έ΅αθοιμνςσψϊωύ'); // navi
	 }

	TUpAscii = array[#128..#255] of Char;

var
	// [to, from]
	CodePage: array[TCodePage, cp1250..High(TCodePage)] of TUpAscii = (
		(
		// to ASCII
		'E'+#$27+'ƒ".++%S<STZZ'+#$27+#$27+'"".--ts>stzz   LoA| cS<--RZ~+ l'+#$27+'u. as>L lzRAAAALCCCEEEEIIDDNNOOOOxRUUUUYTbraaaalccceeeeiiddnnoooo/ruuuuyt ', // from CP1250
		'CueaauccleOoiZACELlooLlSsOUTtLxcaiouAaZzEe-zCs<>   ||AAES||++Zz++++|-|Aa++==|=|odDDEdNIIe++  TU ObONnnSsRUrUyYt'+#$27+'-    / ~  uRr  ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡‰‹‘’“”•–—™› A LoLS SSTZ-ZZ~a l'+#$27+'ls  sstz zzRAAAALCCCEEEEIIDDNNOOOOxRUUUUYTbraaaalccceeeeiiddnnoooo/ruuuuyt '  // from ISO-8859-2
		)
		,
		(
		// to CP1250
		'€‚ƒ„…†‡‰‹‘’“”•–—™› ΅Ά£¤¥¦§¨©ª«¬­®―°±²³΄µ¶·ΈΉΊ»Ό½ΎΏΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡÒΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώÿ', // from CP1250
		'Ηόιβδωζη³λΥυξΔΖΙΕετφΌΎΦά£Χθανσϊ¥ΉΚκ¬ΘΊ«»   ||ΑΒΜª||++―Ώ++++|-|Γγ++==|=|¤πΠΟΛοÒΝΞμ++  ήΩ ΣίΤΡρςΐΪΰΫύέώ΄­½²΅Ά§χΈ°¨ÿϋΨψ  ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡‰‹‘’“”•–—™› ¥Ά£¤Ό§¨ª­―°Ή²³΄Ύ΅ΈΊ½ΏΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡÒΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώÿ' // from ISO-8859-2
		)
		,
		(
		// to OEM-CP852 (LATIN 2)
		'E'+#$27+'ƒ".ΕΕ%ζ<—›¦'+#$27+#$27+'"".--tη>§«ÿστΟ¤|υωcΈ®ªπR½ψ+ςοu.χ¥­―•ρ–Ύθµ¶Ζ‘€¬¨Σ·ΦΧÒΡγΥΰβ™όήιλνέακ ƒΗ„’†‡‚©‰Ψ΅ΤΠδεΆ“‹”φύ…£ϋμξϊ', // from CP1250
		'€‚ƒ„…†‡‰‹‘’“”•–—™› ΅Ά£¤¥¦§¨©ª«¬­®―°±²³΄µ¶·ΈΉΊ»Ό½ΎΏΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡÒΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώÿ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡‰‹‘’“”•–—™›ÿ¤τΟ•—υωζΈ›π¦½ψ¥ςο–σχη­«ρ§Ύθµ¶Ζ‘€¬¨Σ·ΦΧÒΡγΥΰβ™όήιλνέακ ƒΗ„’†‡‚©‰Ψ΅ΤΠδεΆ“‹”φύ…£ϋμξϊ' // from ISO-8859-2
		)
		,
		(
		// to ISO-8859-2
		'E'+#$27+'ƒ".++%©<¦«®¬'+#$27+#$27+'"".--tΉ>¶»ΎΌ ·Ά£¤΅|§¨cª<-­R―°+²³΄u.Έ±Ί>¥½µΏΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡÒΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώÿ', // from CP1250
		'Ηόιβδωζη³λΥυξ¬ΔΖΙΕετφ¥µ¦¶Φά«»£Χθανσϊ΅±®ΎΚκ-ΌΘΊ<>   ||ΑΒΜª||++―Ώ++++|-|Γγ++==|=|¤πΠΟΛοÒΝΞμ++  ήΩ ΣίΤΡρς©ΉΐΪΰΫύέώ΄­½²·Ά§χΈ°¨ÿϋΨψ  ', // from OEM-CP852 (LATIN 2)
		'€‚ƒ„…†‡‰‹‘’“”•–—™› ΅Ά£¤¥¦§¨©ª«¬­®―°±²³΄µ¶·ΈΉΊ»Ό½ΎΏΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡÒΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώÿ' // from ISO-8859-2
		)
	);
{
Unicode
Α	00C1	Ν	00CD		0164
α	00E1	ν	00ED		0165
Θ	010C	Ò	0147	Ϊ	00DA
θ	010D	ς	0148	ϊ	00FA
Ο	010E	Σ	00D3	Ω	016E
ο	010F	σ	00F3	ω	016F
Ι	00C9	Ψ	0158	έ	00DD
ι	00E9	ψ	0159	ύ	00FD
Μ	011A		0160		017D
μ	011B		0161		017E
}

procedure ConvertCharset(var s: string; FromCharset: TCodePage; ToCharset: TCodePage); overload;
var
	i: SG;
{	c, d: Char;
	CP: array[Char] of Char;}
begin
	Assert(FromCharset <> cpAscii);
{	if ToCharset = cp1250 then
	begin}
		for i := 1 to Length(s) do
		begin
			if Ord(s[i]) >= $80 then
				s[i] := CodePage[ToCharset, FromCharset][s[i]];
		end;
{	end
	else
	begin
		// Fill
		for c := Low(c) to High(c) do
		begin
			d := #0;
			for i := Low(TCzLetters) to High(TCzLetters) do
			begin
				if CZX[FromCharset][i] = c then
				begin
					d := CZX[ToCharset][i];
				end;
			end;
			CP[c] := d;
		end;

		// Convert
		for i := 1 to Length(s) do
		begin
			s[i] := CP[s[i]];
		end;
	end;}
end;

function ConvertCharsetF(const s: string; FromCharset: TCodePage; ToCharset: TCodePage): string; overload;
var
	i: SG;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		if Ord(s[i]) >= $80 then
			Result[i] := CodePage[ToCharset, FromCharset][s[i]]
		else
			Result[i] := s[i];
	end;
end;

procedure FillCharsTable;
var c, Result: Char;
{$ifopt d+}s: string;{$endif}
begin
	for c := Low(c) to High(c) do
	begin
		// UpCaseCz
		case c of
		'a'..'z': Result := Chr(Ord(c) - Ord('a') + Ord('A'));
		'α': Result := 'Α';
		'θ': Result := 'Θ';
		'ο': Result := 'Ο';
		'ι': Result := 'Ι';
		'μ': Result := 'Μ';
		'ν': Result := 'Ν';
		'ς': Result := 'Ò';
		'σ': Result := 'Σ';
		'ψ': Result := 'Ψ';
		'': Result := '';
		'': Result := '';
		'ϊ': Result := 'Ϊ';
		'ω': Result := 'Ω';
		'ύ': Result := 'έ';
		'': Result := '';
		else Result := c;
		end;
		TableUpCaseCz[c] := Result;
	end;
	{$ifopt d+}
	// Tests
	s := 'δαθοιμνΎςστΰψϊωύ';
	ConvertCharset(s, cp1250, cp852);
	Assert(s = '„ Τ‚Ψ΅–εΆ“κύη£…μ§');

	s := 'ρ';
	ConvertCharset(s, cp1250, cp852);
	Assert(s = 'δ');

	s := 'ΑΘΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ';
	ConvertCharset(s, cp1250, cp852);
	Assert(s = 'µ¬Ò·ΦΥΰόζ›ιήν¦ Τ‚Ψ΅εΆύη£…μ§');

	s := 'µ¬Ò·ΦΥΰόζ›ιήν¦ Τ‚Ψ΅εΆύη£…μ§';
	ConvertCharset(s, cp852, cp1250);
	Assert(s = 'ΑΘΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ');

	s := 'ΑΘΟΙΜΝÒΣΨ©«ΪΩέ®αθοιμνςσψΉ»ϊωύΎ';
	ConvertCharset(s, cpISO88592, cp1250);
	Assert(s = 'ΑΘΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ');

	s := 'ΑΘΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ';
	ConvertCharset(s, cp1250, cpISO88592);
	Assert(s = 'ΑΘΟΙΜΝÒΣΨ©«ΪΩέ®αθοιμνςσψΉ»ϊωύΎ');

	s := 'µ¬Ò·ΦΥΰόζ›ιήν¦ Τ‚Ψ΅εΆύη£…μ§';
	ConvertCharset(s, cp852, cpISO88592);
	Assert(s = 'ΑΘΟΙΜΝÒΣΨ©«ΪΩέ®αθοιμνςσψΉ»ϊωύΎ');

	s := 'ΑΘΟΙΜΝÒΣΨ©«ΪΩέ®αθοιμνςσψΉ»ϊωύΎ';
	ConvertCharset(s, cpISO88592, cp852);
	Assert(s = 'µ¬Ò·ΦΥΰόζ›ιήν¦ Τ‚Ψ΅εΆύη£…μ§');

	s := 'ΑΘΟΙΜΝÒΣΨΪΩέαθοιμνςσψϊωύ';
	s := DelCz(s);
	Assert(s = 'ACDEEINORSTUUYZacdeeinorstuuyz');

	s := 'Frόhauf David';
	s := DelCz(s);
	Assert(s = 'Fruhauf David');
	{$endif}
end;

function UpCaseCz(const s: string): string;
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := TableUpCaseCz[s[i]];
	end;
end;

function DelCz(const s: string): string;
begin
	Result := s;
	ConvertCharset(Result, cp1250, cpAscii);
end;

initialization
	FillCharsTable;
end.

