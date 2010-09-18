unit uLang;

interface

function UpCaseCz(const s: string): string;
function DelCz(const s: string): string;

function DosCzSkToWin(const s: string): string;

function WinCzSkToDos(const s: string): string;
function WinPlToDos(const s: string): string;
function WinHuToDos(const s: string): string;


var
	TableUpCaseCz,
	TableDelCz,
	TableDosCzToWin,
	TableWinCzSkToDos,
	TableWinPlToDos,
	TableWinHuToDos: array[Char] of Char;

implementation

uses
	Dialogs, SysUtils,
	uDialog;

const
	CZX: array [0..14] of string =
	 ('ACDEEINORSTUUYZacdeeinorstuuyz', // ASCII
		'ÁÈÏÉÌÍÒÓØŠÚÙİáèïéìíòóøšúùı', // CP1250
		'µ¬Ò·ÖÕàüæ›éŞí¦ ŸÔ‚Ø¡å¢ıçœ£…ì§', // CP852 (LATIN 2)
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
		'ı': Result := 'İ';
		'': Result := '';
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
		'ı': Result := 'y';
		'': Result := 'z';

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
		'İ': Result := 'Y';
		'': Result := 'Z';
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
		'ı': Result := 'ø';
		'ç': Result := 'š';
		'œ': Result := '';
		'£': Result := 'ú';
		'…': Result := 'ù';
		'ì': Result := 'ı';
		'§': Result := '';

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
		'Ş': Result := 'Ù';
		'í': Result := 'İ';
		'¦': Result := '';
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
		'ø': Result := 'ı';
		'š': Result := 'ç';
		'': Result := 'œ';
		'ú': Result := '£';
		'ù': Result := '…';
		'ı': Result := 'ì';
		'': Result := '§';

		'Ä': Result := ''; // 142 ???SK
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
		'Ù': Result := 'Ş';
		'İ': Result := 'í';
		'': Result := '¦';
		else Result := #0;
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
		'ø': Result := 'ı';
		'œ': Result := '˜';
		'š': Result := 'ç';
		'ı': Result := 'ì';
		'Ÿ': Result := '«';
		'¿': Result := '¾';
		'': Result := '§';

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
		'İ': Result := 'í';
		'': Result := ''; // 141
		'¯': Result := '½'; // 189
		'': Result := '¦';
		else Result := #0;
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
		'ø': Result := 'ı';
		'ú': Result := '£';
		'ü': Result := 'À'; // 129
		'û': Result := 'û'; // 251
		'ı': Result := 'ì';

		'Ä': Result := ''; // 142
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
		'İ': Result := 'í';
		else Result := #0;
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
		if c = #0 then
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
		if c = #0 then
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
		if c = #0 then
		begin
			Result[i] := s[i];
			MessageD('Unknown char ' + s[i], mtError, [mbOk]);
		end
		else
			Result[i] := c;
	end;
end;

initialization
	FillCharsTable;
end.
