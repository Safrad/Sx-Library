//* File:     Lib\uLang.pas
//* Created:  1999-11-01
//* Modified: 2004-11-08
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uLang;

interface

uses SysUtils;

type
	TCodePage = (cpAscii, cp1250, cp852, cpISO88592, cpKeybCS2, cpMacCE,
		cpKOI8CS, cpkodxx, cpWFW_311, cpISO88591, cpT1, cpMEXSK, cpw311_cw,
		cpVavrusa, cpNavi);
var
	TableUpCaseCz,
	TableDelCz,
	TableDosCzToWin,
	TableWinCzSkToDos,
	TableWinPlToDos,
	TableWinHuToDos: array[Char] of Char;

procedure ConvertCharset(var s: string; FromCharset, ToCharset: TCodePage);

function UpCaseCz(const s: string): string;
function DelCz(const s: string): string;

function DosCzSkToWin(const s: string): string;

function WinCzSkToDos(const s: string): string;
function WinPlToDos(const s: string): string;
function WinHuToDos(const s: string): string;

//Alphabet
procedure ReadAlphabet(FileName: TFileName);
function AlphaStrToWideStr(Line: string): WideString;

// Dictionary
procedure ReadDictionary(FileName: TFileName);
function Translate(Line: string): string; overload;
procedure TranslateFile(FileName: TFileName); overload;

implementation

uses
	Dialogs, Windows,
	uAdd, uError, uStrings, uFiles, uSorts, uFind, uParser;

type
	TCzLetters = array[0..29] of Char;
const
	CZX: array [TCodePage] of TCzLetters =
	 ('ACDEEINORSTUUYZacdeeinorstuuyz', // ASCII
		'ÁÈÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž', // CP1250
		'µ¬Ò·ÖÕàüæ›éÞí¦ ŸÔ‚Ø¡å¢ýçœ£…ì§', // CP852 (LATIN 2)
		'ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®áèïéìíòóø¹»úùý¾', // ISO-8859-2
		'€…‰‹¥•ž›†—¦’ ‡ƒ‚ˆ¡¤¢©¨Ÿ£–˜‘', // KEYBCS2 (Kamenicky)
		'ç‰‘ƒêÅîÛáèòñøë‡‹“Žž’Ë—Þäéœóùì', // MAC CE
		'áãäçåéîïòóôõêùúÁÃÄ×ÅÉÎÏÒÓÔÕÊÙÚ', // KOI8-CS
		'ÁÈÏÉÌÍÒ¡ØŠ¡ÙÝŽáèïéìíòóøšúùýž', // kodxx
		'ÁÈÏÉÌÍÒÓØŠÚ¡Ý¡áèïéìíòóøšúùýž', // WFW_3-11
		'0CDÉEÍ+ÓRSTÚUÝZ®cdéeínórstúuýz', // ISO-8859-1
		'ÁâëÉùÍ‰Ó¤·ËÚÒ¯ÎáãìéûíÀóÞ”ÈúÛ˜Ï', // T1
		'ÁÈÏÉÌÍÒÓØŠÚ¡ÝŽáèïéìíòóøšú¡ýž', // MEXSK
		'Á%ÏÉÌÍÒÓØŠÚÙÝŽáèïéìíòóøšúùýž', // w311_ce
		'ÁÈÏÉÌÍÒÓØŠÚ¡ÝŽáèïìéíòóøšú¡ýž', // vavrusa
		'ÁÈÏÉÌÍÒÓ+ŠÚ¡Ý¡áèïéìíòóøšúùýž'); // navi


procedure ConvertCharset(var s: string; FromCharset, ToCharset: TCodePage);
var
	i: SG;
	c, d: Char;
	CP: array[Char] of Char;
begin
	// Fill
	for c := Low(c) to High(c) do
	begin
		d := c;
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
end;

procedure FillCharsTable;
var c, Result: Char;
begin
	for c := Low(c) to High(c) do
	begin
		// UpCaseCz
		case c of
		'a'..'z': Result := Chr(Ord(c) - Ord('a') + Ord('A'));
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
		'ý': Result := 'Ý';
		'ž': Result := 'Ž';
		else Result := c;
		end;
		TableUpCaseCz[c] := Result;

		// DelCz
		case c of
		'á': Result := 'a';
		'è': Result := 'c';
		'ï': Result := 'd';
		'ì': Result := 'e';
		'é': Result := 'e';
		'í': Result := 'i';
		'ò': Result := 'n';
		'ó': Result := 'o';
		'ø': Result := 'r';
		'š': Result := 's';
		'': Result := 't';
		'ú': Result := 'u';
		'ù': Result := 'u';
		'ý': Result := 'y';
		'ž': Result := 'z';

		'Á': Result := 'A';
		'È': Result := 'C';
		'Ï': Result := 'D';
		'É': Result := 'E';
		'Ì': Result := 'E';
		'Í': Result := 'I';
		'Ò': Result := 'N';
		'Ó': Result := 'O';
		'Ø': Result := 'R';
		'Š': Result := 'S';
		'': Result := 'T';
		'Ú': Result := 'U';
		'Ù': Result := 'U';
		'Ý': Result := 'Y';
		'Ž': Result := 'Z';
		else Result := c;
		end;
		TableDelCz[c] := Result;

		// DosCzToWin (852 to 1250)
		case c of
		' ': Result := 'á';
		'Ÿ': Result := 'è';
		'Ô': Result := 'ï';
		'‚': Result := 'é';
		'Ø': Result := 'ì';
		'¡': Result := 'í';
		'–': Result := '¾'; // SK
		'å': Result := 'ò';
		'¢': Result := 'ó';
		'“': Result := 'ô'; // SK
		'ý': Result := 'ø';
		'ç': Result := 'š';
		'œ': Result := '';
		'£': Result := 'ú';
		'…': Result := 'ù';
		'ì': Result := 'ý';
		'§': Result := 'ž';

		'µ': Result := 'Á';
		'¬': Result := 'È';
		'Ò': Result := 'Ï';
		'': Result := 'É';
		'·': Result := 'Ì';
		'Ö': Result := 'Í';
		'‘': Result := '¼'; // SK
		'Õ': Result := 'Ò';
		'à': Result := 'Ó';
		'â': Result := 'Ô'; // SK
		'ü': Result := 'Ø';
		'æ': Result := 'Š';
		'›': Result := '';
		'é': Result := 'Ú';
		'Þ': Result := 'Ù';
		'í': Result := 'Ý';
		'¦': Result := 'Ž';
		else Result := c;
		end;
		TableDosCzToWin[c] := Result;

		// WinCzSkToDos
		case c of
		#32..#127: Result := c;
		'ä': Result := '„'; // 132 ???SK
		'á': Result := ' ';
		'è': Result := 'Ÿ';
		'ï': Result := 'Ô';
		'é': Result := '‚';
		'ì': Result := 'Ø';
		'í': Result := '¡';
		'¾': Result := '–'; // SK
		'ò': Result := 'å';
		'ó': Result := '¢';
		'ô': Result := '“'; // SK
		'à': Result := 'ê'; // 234 ???SK
		'ø': Result := 'ý';
		'š': Result := 'ç';
		'': Result := 'œ';
		'ú': Result := '£';
		'ù': Result := '…';
		'ý': Result := 'ì';
		'ž': Result := '§';

		'Ä': Result := 'Ž'; // 142 ???SK
		'Á': Result := 'µ';
		'È': Result := '¬';
		'Ï': Result := 'Ò';
		'É': Result := '';
		'Ì': Result := '·';
		'Í': Result := 'Ö'; // 214
		'¼': Result := '‘'; //     SK
		'Ò': Result := 'Õ';
		'Ó': Result := 'à';
		'Ô': Result := 'â'; // 226 SK
		'À': Result := 'è'; // 232 ???SK
		'Ø': Result := 'ü';
		'Š': Result := 'æ';
		'': Result := '›';
		'Ú': Result := 'é';
		'Ù': Result := 'Þ';
		'Ý': Result := 'í';
		'Ž': Result := '¦';
		else Result := CharNul;
		end;
		TableWinCzSkToDos[c] := Result;

		// WinPlToDos
		case c of
		#32..#127: Result := c;
		'á': Result := ' ';
		'¹': Result := '¥'; // 165
		'æ': Result := '†'; // 134
		'ê': Result := '©';
		'ì': Result := 'Ø';
		'í': Result := '¡';
		'³': Result := '–'; // 136
		'ñ': Result := 'ä';
		'ó': Result := '¢'; // 162
		'ø': Result := 'ý';
		'œ': Result := '˜';
		'š': Result := 'ç';
		'ý': Result := 'ì';
		'Ÿ': Result := '«';
		'¿': Result := '¾';
		'ž': Result := '§';

		'Á': Result := 'µ';
		'¥': Result := '¤'; // 164
		'Æ': Result := ''; // 143
		'Ê': Result := '¨'; // 168
		'Ì': Result := '·';
		'Í': Result := 'Ö';
		'£': Result := ''; // 157
		'Ñ': Result := 'ã'; // 227
		'Ó': Result := 'à'; // 224
		'Ø': Result := 'ü';
		'Œ': Result := 'y'; // 151
		'Š': Result := 'æ';
		'Ý': Result := 'í';
		'': Result := ''; // 141
		'¯': Result := '½'; // 189
		'Ž': Result := '¦';
		else Result := CharNul;
		end;
		TableWinPlToDos[c] := Result;

		// WinHuToDos
		case c of
		#32..#127: Result := c;
		'ä': Result := '„'; // 132
		'á': Result := ' '; // 160
		'é': Result := '‚';
		'í': Result := '¡';
		'ó': Result := '¢';
		'õ': Result := '‹'; // 139
		'ö': Result := '”'; // 148
		'ô': Result := '“'; // 147
		'ø': Result := 'ý';
		'ú': Result := '£';
		'ü': Result := 'À'; // 129
		'û': Result := 'û'; // 251
		'ý': Result := 'ì';

		'Ä': Result := 'Ž'; // 142
		'Á': Result := 'µ';
		'É': Result := '';
		'Í': Result := 'Ö';
		'Ó': Result := 'à';
		'Õ': Result := 'Š'; // 138
		'Ö': Result := '™'; // 153
		'Ô': Result := 'â'; // 226
		'Ø': Result := 'ü';
		'Ú': Result := 'é';
		'Ü': Result := '˜'; // 154
		'Û': Result := 'ë'; // 235
		'Ý': Result := 'í';
		else Result := CharNul;
		end;
		TableWinHuToDos[c] := Result;
	end;
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
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(Result) do
	begin
		Result[i] := TableDelCz[s[i]];
	end;
end;

function DosCzSkToWin(const s: string): string;
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := TableDosCzToWin[s[i]];
	end;
end;

function WinCzSkToDos(const s: string): string;
var
	i: Integer;
	c: Char;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		c := TableWinCzSkToDos[s[i]];
		if c = CharNul then
		begin
			Result[i] := s[i];
			MessageD('Unknown char ' + s[i] + ' (' + IntToStr(Ord(s[i])) + ')', mtError, [mbOk]);
		end
		else
			Result[i] := c;
	end;
end;

function WinPlToDos(const s: string): string;
var
	i: Integer;
	c: Char;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		c := TableWinPlToDos[s[i]];
		if c = CharNul then
		begin
			Result[i] := s[i];
			MessageD('Unknown char ' + s[i] + ' (' + IntToStr(Ord(s[i])) + ')', mtError, [mbOk]);
		end
		else
			Result[i] := c;
	end;
end;

function WinHuToDos(const s: string): string;
var
	i: Integer;
	c: Char;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		c := TableWinHuToDos[s[i]];
		Result[i] := c;
		if c = CharNul then
		begin
			Result[i] := s[i];
			MessageD('Unknown char ' + s[i], mtError, [mbOk]);
		end
		else
			Result[i] := c;
	end;
end;

// Alphabet
var
	Alpha: array of string;
	AlphaCount: Integer;

procedure ReadAlphabet(FileName: TFileName);
label LRetry;
var
	F: TFile;
	s: string;
begin
	F := TFile.Create;
	LRetry:
	AlphaCount := 0; SetLength(Alpha, 0);
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		while not F.Eof do
		begin
			F.Readln(s);
			if Length(s) > 0 then
			begin
				SetLength(Alpha, AlphaCount + 1);
				Alpha[AlphaCount] := s;
				Inc(AlphaCount);
			end;
		end;
		if not F.Close then goto LRetry;
	end;
	F.Free;
end;

function AlphaCharToWord(Line: string; var InLineIndex: Integer): Word;
var
	i: Integer;
	Found: Integer;
begin
	Result := Ord(Line[InLineIndex]);
	Found := 0;
	for i := 0 to AlphaCount - 1 do
	begin
		if Copy(Line, InLineIndex, Length(Alpha[i])) = Alpha[i] then
		begin
			if Length(Alpha[i]) > Found then
			begin
				Result := 65 + i;
				Found := Length(Alpha[i]);
			end;
		end;
	end;
	if Found > 0 then
	begin
		Inc(InLineIndex, Found);
	end
	else
	begin
		if Ord(Line[InLineIndex]) >= 123 then
			Result := Ord(Line[InLineIndex]) + 256
		else
			Result := Ord(Line[InLineIndex]);
		Inc(InLineIndex);
	end;
end;

function AlphaStrToWideStr(Line: string): WideString;
var i, j: Integer;
begin
	SetLength(Result, Length(Line));
	i := 1;
	while i <= Length(Line) do
	begin
		j := i;
		Result[j] := WideChar(AlphaCharToWord(Line, i));
	end;
end;


// Dictionary
type
	TDict = packed record
		Cz, En: string;
	end;
var
	Dict: array of TDict;
	DictCount: SG;
	AIndex: array of SG;
	AValue: array of U4;

procedure ReadDictionary(FileName: TFileName);
var
	Line: string;
	InLineIndex: SG;
	i: SG;
begin
	Line := ReadStringFromFile(FileName);
	InLineIndex := 1;
	DictCount := 0;
	while InLineIndex < Length(Line) do
	begin
		Inc(DictCount);
		SetLength(Dict, DictCount);

		Dict[DictCount - 1].Cz := ReadToChar(Line, InLineIndex, CharTab);
		Dict[DictCount - 1].En := ReadToChar(Line, InLineIndex, CharCR);
		ReadToChar(Line, InLineIndex, CharLF);
	end;
	// Read dictionary
	SetLength(AIndex, DictCount);
	SetLength(AValue, DictCount);
	for i := 0 to DictCount - 1 do
	begin
		AIndex[i] := i;
		AValue[i] := Length(Dict[i].Cz);
	end;

	SortU4(False, True, PArraySG(@AIndex[0]), PArrayU4(@AValue[0]), DictCount);
end;

function Translate(Line: string): string; overload;
var
	i, j, Index, Po: SG;
	WhatS, WhatS2, ToS: string;
begin
	for j := 0 to DictCount - 1 do
	begin
		i := AIndex[j];
		Index := 1;
		WhatS := UpCaseCz(Dict[i].Cz);
		while Index < Length(Line) do
		begin
			Po := Find(WhatS, UpCaseCz(Line), Index);

			if (Po <> 0) then
			begin
				if (CharsTable[Line[Po - 1]] <> ctLetter)
				and (CharsTable[Line[Po + Length(WhatS)]] <> ctLetter)
				and (Line[Po - 1] <> '<')
				and (Line[Po - 1] <> '/')
				and (Ord(Line[Po - 1]) < 128)
				and (Ord(Line[Po + Length(WhatS)]) < 128) then
				begin
					if Line[Po] = Dict[i].Cz[1] then
						ToS := Dict[i].En
					else
					begin
						ToS := Dict[i].En;
						ToS[1] := UpCase(ToS[1]);
					end;

					WhatS2 := Dict[i].Cz;
					Delete(Line, Po, Length(WhatS2));
					Insert(ToS, Line, Po);
					Index := Po + Length(ToS);
				end
				else
					Index := Po + Length(WhatS);
			end
			else
				Break;
		end;

//		Replace(Line, Dict[i].Cz, Dict[i].En);
	end;
//	ConvertCharset(Line, cp1250, cpISO88592);
	Result := Line;
end;

procedure TranslateFile(FileName: TFileName); overload;
var
	FileNameEn: TFileName;
	Line: string;
begin
	Line := Translate(ReadStringFromFile(FileName));
	FileNameEn := Translate(ExtractFileName(FileName));
	if FileNameEn <> FileName then
		FileName := AddAfterName(FileName, 'En')
	else
		FileName := AddAfterName(FileName, 'En');
	WriteStringToFile(FileName, Line, False);

end;

procedure _finalization;
begin
	SetLength(Alpha, 0);
	SetLength(AIndex, 0);
	SetLength(AValue, 0);
	SetLength(Dict, 0);
end;

initialization
	FillCharsTable;
finalization
	_finalization;
end.
