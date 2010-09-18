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

procedure FillCharsTable;
var c, Result: Char;
begin
	for c := Low(c) to High(c) do
	begin
		// UpCaseCz
		case c of
		'a'..'z': Result := Chr(Ord(c) - Ord('a') + Ord('A'));
		'·': Result := '¡';
		'Ë': Result := '»';
		'Ô': Result := 'œ';
		'È': Result := '…';
		'Ï': Result := 'Ã';
		'Ì': Result := 'Õ';
		'Ú': Result := '“';
		'Û': Result := '”';
		'¯': Result := 'ÿ';
		'ö': Result := 'ä';
		'ù': Result := 'ç';
		'˙': Result := '⁄';
		'˘': Result := 'Ÿ';
		'˝': Result := '›';
		'û': Result := 'é';
		else Result := c;
		end;
		TableUpCaseCz[c] := Result;

		// DelCz
		case c of
		'·': Result := 'a';
		'Ë': Result := 'c';
		'Ô': Result := 'd';
		'Ï': Result := 'e';
		'È': Result := 'e';
		'Ì': Result := 'i';
		'Ú': Result := 'n';
		'Û': Result := 'o';
		'¯': Result := 'r';
		'ö': Result := 's';
		'ù': Result := 't';
		'˙': Result := 'u';
		'˘': Result := 'u';
		'˝': Result := 'y';
		'û': Result := 'z';

		'¡': Result := 'A';
		'»': Result := 'C';
		'œ': Result := 'D';
		'…': Result := 'E';
		'Ã': Result := 'E';
		'Õ': Result := 'I';
		'“': Result := 'N';
		'”': Result := 'O';
		'ÿ': Result := 'R';
		'ä': Result := 'S';
		'ç': Result := 'T';
		'⁄': Result := 'U';
		'Ÿ': Result := 'U';
		'›': Result := 'Y';
		'é': Result := 'Z';
		else Result := c;
		end;
		TableDelCz[c] := Result;

		// DosCzToWin (852 to 1250)
		case c of
		'†': Result := '·';
		'ü': Result := 'Ë';
		'‘': Result := 'Ô';
		'Ç': Result := 'È';
		'ÿ': Result := 'Ï';
		'°': Result := 'Ì';
		'ñ': Result := 'æ'; // SK
		'Â': Result := 'Ú';
		'¢': Result := 'Û';
		'ì': Result := 'Ù'; // SK
		'˝': Result := '¯';
		'Á': Result := 'ö';
		'ú': Result := 'ù';
		'£': Result := '˙';
		'Ö': Result := '˘';
		'Ï': Result := '˝';
		'ß': Result := 'û';

		'µ': Result := '¡';
		'¨': Result := '»';
		'“': Result := 'œ';
		'ê': Result := '…';
		'∑': Result := 'Ã';
		'÷': Result := 'Õ';
		'ë': Result := 'º'; // SK
		'’': Result := '“';
		'‡': Result := '”';
		'‚': Result := '‘'; // SK
		'¸': Result := 'ÿ';
		'Ê': Result := 'ä';
		'õ': Result := 'ç';
		'È': Result := '⁄';
		'ﬁ': Result := 'Ÿ';
		'Ì': Result := '›';
		'¶': Result := 'é';
		else Result := c;
		end;
		TableDosCzToWin[c] := Result;

		// WinCzSkToDos
		case c of
		#32..#127: Result := c;
		'‰': Result := 'Ñ'; // 132 ???SK
		'·': Result := '†';
		'Ë': Result := 'ü';
		'Ô': Result := '‘';
		'È': Result := 'Ç';
		'Ï': Result := 'ÿ';
		'Ì': Result := '°';
		'æ': Result := 'ñ'; // SK
		'Ú': Result := 'Â';
		'Û': Result := '¢';
		'Ù': Result := 'ì'; // SK
		'‡': Result := 'Í'; // 234 ???SK
		'¯': Result := '˝';
		'ö': Result := 'Á';
		'ù': Result := 'ú';
		'˙': Result := '£';
		'˘': Result := 'Ö';
		'˝': Result := 'Ï';
		'û': Result := 'ß';

		'ƒ': Result := 'é'; // 142 ???SK
		'¡': Result := 'µ';
		'»': Result := '¨';
		'œ': Result := '“';
		'…': Result := 'ê';
		'Ã': Result := '∑';
		'Õ': Result := '÷'; // 214
		'º': Result := 'ë'; //     SK
		'“': Result := '’';
		'”': Result := '‡';
		'‘': Result := '‚'; // 226 SK
		'¿': Result := 'Ë'; // 232 ???SK
		'ÿ': Result := '¸';
		'ä': Result := 'Ê';
		'ç': Result := 'õ';
		'⁄': Result := 'È';
		'Ÿ': Result := 'ﬁ';
		'›': Result := 'Ì';
		'é': Result := '¶';
		else Result := #0;
		end;
		TableWinCzSkToDos[c] := Result;

		// WinPlToDos
		case c of
		#32..#127: Result := c;
		'·': Result := '†';
		'π': Result := '•'; // 165
		'Ê': Result := 'Ü'; // 134
		'Í': Result := '©';
		'Ï': Result := 'ÿ';
		'Ì': Result := '°';
		'≥': Result := 'ñ'; // 136
		'Ò': Result := '‰';
		'Û': Result := '¢'; // 162
		'¯': Result := '˝';
		'ú': Result := 'ò';
		'ö': Result := 'Á';
		'˝': Result := 'Ï';
		'ü': Result := '´';
		'ø': Result := 'æ';
		'û': Result := 'ß';

		'¡': Result := 'µ';
		'•': Result := '§'; // 164
		'∆': Result := 'è'; // 143
		' ': Result := '®'; // 168
		'Ã': Result := '∑';
		'Õ': Result := '÷';
		'£': Result := 'ù'; // 157
		'—': Result := '„'; // 227
		'”': Result := '‡'; // 224
		'ÿ': Result := '¸';
		'å': Result := 'y'; // 151
		'ä': Result := 'Ê';
		'›': Result := 'Ì';
		'è': Result := 'ç'; // 141
		'Ø': Result := 'Ω'; // 189
		'é': Result := '¶';
		else Result := #0;
		end;
		TableWinPlToDos[c] := Result;

		// WinHuToDos
		case c of
		#32..#127: Result := c;
		'‰': Result := 'Ñ'; // 132
		'·': Result := '†'; // 160
		'È': Result := 'Ç';
		'Ì': Result := '°';
		'Û': Result := '¢';
		'ı': Result := 'ã'; // 139
		'ˆ': Result := 'î'; // 148
		'Ù': Result := 'ì'; // 147
		'¯': Result := '˝';
		'˙': Result := '£';
		'¸': Result := '¿'; // 129
		'˚': Result := '˚'; // 251
		'˝': Result := 'Ï';

		'ƒ': Result := 'é'; // 142
		'¡': Result := 'µ';
		'…': Result := 'ê';
		'Õ': Result := '÷';
		'”': Result := '‡';
		'’': Result := 'ä'; // 138
		'÷': Result := 'ô'; // 153
		'‘': Result := '‚'; // 226
		'ÿ': Result := '¸';
		'⁄': Result := 'È';
		'‹': Result := 'ò'; // 154
		'€': Result := 'Î'; // 235
		'›': Result := 'Ì';
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
