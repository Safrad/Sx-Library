unit uRegionalCodePage;

{$ZEROBASEDSTRINGS OFF}

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

procedure ConvertCharset(var s: AnsiString; const FromCharset, ToCharset: TCodePage);
function ConvertCharsetF(const s: AnsiString; const FromCharset, ToCharset: TCodePage): AnsiString;

var
	TableUpCaseCz, TableLowCaseCz: array[AnsiChar] of AnsiChar;

function UpCaseCz(const s: string): string;
function LowCaseCz(const s: string): string;

implementation

uses
  SysUtils;

{const
	CZX: array[TCodePage] of TCzLetters = (
		'ACDEEINORSTUUYZacdeeinorstuuyz', // ASCII
		'��������؊���ݎ���������������', // ANSI-CP1250
		'��Ґ����������Ԃء��眣��', // OEM-CP852 (LATIN 2)
		'��������ة���ݮ���������������', // ISO-8859-2
		'������������������������������', // KEYBCS2 (Kamenicky)
		'牑�����������뇋����˗������', // MAC CE
		'������������������������������', // KOI8-CS
		'������ҡ؊���ݎ���������������', // kodxx
		'��������؊�ڡݡ���������������', // WFW_3-11
		'0CD�E�+�RST�U�Z�cd�e�n�rst�u�z', // ISO-8859-1
		'�����͉Ӥ���ү���������ޔ��ۘ�', // T1
		'��������؊�ڡݎ���������������', // MEXSK
		'�%������؊���ݎ���������������', // w311_ce
		'��������؊�ڡݎ���������������', // vavrusa
		'��������+��ڡݡ���������������'); // navi
	}
type
	TUpAscii = array[#128..#255] of AnsiChar;

var
	// [to, from]
	CodePage: array[cpAscii..cpISO88592, cp1250..cpISO88592] of TUpAscii = (
		(
		// to ASCII
		'E�'+#$27+'�".++�%S<STZZ�'+#$27+#$27+'"".--�ts>stzz   LoA| cS<--RZ~+ l'+#$27+'u. as>L lzRAAAALCCCEEEEIIDDNNOOOOxRUUUUYTbraaaalccceeeeiiddnnoooo/ruuuuyt ', // from CP1250
		'CueaauccleOoiZACELlooLlSsOUTtLxcaiouAaZzEe-zCs<>   ||AAES||++Zz++++|-|Aa++==|=|odDDEdNIIe++  TU ObONnnSsRUrUyYt'+#$27+'-    / ~  uRr  ', // from OEM-CP852 (LATIN 2)
		'�������������������������������� A LoLS SSTZ-ZZ~a l'+#$27+'ls  sstz zzRAAAALCCCEEEEIIDDNNOOOOxRUUUUYTbraaaalccceeeeiiddnnoooo/ruuuuyt '  // from ISO-8859-2
		)
		,
		(
		// to CP1250
		'��������������������������������������������������������������������������������������������������������������������������������', // from CP1250
		'������������������������܍�������������꬟Ⱥ��   ||��̪||++��++++|-|��++==|=|����������++  �� ���������������������������� �', // from OEM-CP852 (LATIN 2)
		'��������������������������������������������������������������������������������������������������������������������������������' // from ISO-8859-2
		)
		,
		(
		// to OEM-CP852 (LATIN 2)
		'E�'+#$27+'�".�ň%�<�����'+#$27+#$27+'"".--�t�>��������Ϥ|��c����R��+��u.������赶Ǝ������ӷ�������⊙��������ꠃǄ�������ء����墓�����������', // from CP1250
		'��������������������������������������������������������������������������������������������������������������������������������', // from OEM-CP852 (LATIN 2)
		'������������������������������������ϕ���減�������筜��赶Ǝ������ӷ�������⊙��������ꠃǄ�������ء����墓�����������' // from ISO-8859-2
		)
		,
		(
		// to ISO-8859-2
		'E�'+#$27+'�".++�%�<�����'+#$27+#$27+'"".--�t�>����������|��c�<-�R��+���u.���>��������������������������������������������������������������������', // from CP1250
		'������������������������ܫ��������������-�Ⱥ<>   ||��̪||++��++++|-|��++==|=|����������++  �� ���������������������������� �', // from OEM-CP852 (LATIN 2)
		'��������������������������������������������������������������������������������������������������������������������������������' // from ISO-8859-2
		)
	);

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
		s := AnsiString(Utf8ToAnsi(s));
		if (ToCharset <> cp1250) then
			ConvertCharset(s, cp1250, ToCharset);
		Exit;
	end;

	if (ToCharset = cpUTF8) then
	begin
		if (FromCharset <> cp1250) then
			ConvertCharset(s, FromCharset, cp1250);
		s := AnsiToUtf8(string(s));
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
procedure FillTableUpCaseCz;
var c, Result: AnsiChar;
begin
	for c := Low(c) to High(c) do
	begin
		// UpCaseCz
		case c of
		'a'..'z': Result := AnsiChar(Ord(c) - Ord('a') + Ord('A'));
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		else Result := c;
		end;
		TableUpCaseCz[c] := Result;
	end;
end;

procedure FillTableLowCaseCz;
var c, Result: AnsiChar;
begin
	for c := Low(c) to High(c) do
	begin
		// UpCaseCz
		case c of
		'A'..'Z': Result := AnsiChar(Ord(c) - Ord('A') + Ord('a'));
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		'�': Result := '�';
		else Result := c;
		end;
		TableLowCaseCz[c] := Result;
	end;
end;
{$endif}

function UpCaseCz(const s: string): string;
{$ifndef UNICODE}
var i: Integer;
{$endif}
begin
	{$ifdef UNICODE}
	Result := UpperCase(s, loUserLocale);
	{$else}
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := Char(TableUpCaseCz[AnsiChar(s[i])])
	end;
	{$endif}
end;

function LowCaseCz(const s: string): string;
{$ifndef UNICODE}
var i: Integer;
{$endif}
begin
	{$ifdef UNICODE}
	Result := LowerCase(s, loUserLocale);
	{$else}
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := Char(TableLowCaseCz[AnsiChar(s[i])])
	end;
	{$endif}
end;

initialization
{$IFNDEF NoInitialization}
	{$ifndef UNICODE}
	FillTableUpCaseCz;
	FillTableLowCaseCz
	{$endif}
{$ENDIF NoInitialization}
end.
