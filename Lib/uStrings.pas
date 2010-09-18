// Build: 08/2000-08/2000 Author: Safranek David

unit uStrings;

interface

function DosCzToWin(const c: Char): Char; overload;
function DosCzToWin(const s: string): string; overload;
function UpCaseCz(const c: Char): Char; overload;
function UpCaseCz(const s: string): string; overload;
function DelCz(const c: Char): Char; overload;
function DelCz(const s: string): string; overload;

function DelChars(const s: string; const C: Char): string;
function DelQuoteF(const s: string): string;
procedure DelQuote(var s: string);
function DelEndSpaceF(const s: string): string;
procedure DelEndSpace(var s: string);

function ReadToChar(const Line: string; var InLineIndex: Integer;
	const C: Char): string;
function PosWW(Str, SubStr: string): Integer;
function IsSubStr(SubStr: string; Str: string): Boolean;

implementation

var
	TableDosCzToWin: array[Char] of Char;
	TableUpCaseCz: array[Char] of Char;
	TableDelCz: array[Char] of Char;
	TableWordSep: array[0..7] of Char = (' ', ',', '.', '-', '/', ';', '(', ')');

procedure FillCharsTable;
var c, Result: Char;
begin
	for c := Low(c) to High(c) do
	begin
		// DosCzToWin
		case c of
		'†': Result := '·';
		'ü': Result := 'Ë';
		'‘': Result := 'Ô';
		'Ç': Result := 'È';
		'ÿ': Result := 'Ï';
		'°': Result := 'Ì';
		'Â': Result := 'Ú';
		'¢': Result := 'Û';
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
		'’': Result := '“';
		'‡': Result := '”';
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
	end;
end;

function DosCzToWin(const c: Char): Char; overload;
begin
	Result := TableDosCzToWin[c];
end;

function DosCzToWin(const s: string): string; overload;
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := TableDosCzToWin[s[i]];
	end;
end;

function UpCaseCz(const c: Char): Char; overload;
begin
	Result := TableUpCaseCz[c];
end;

function UpCaseCz(const s: string): string; overload;
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		Result[i] := TableUpCaseCz[s[i]];
	end;
end;

function DelCz(const c: Char): Char; overload;
begin
	Result := TableDelCz[c];
end;

function DelCz(const s: string): string; overload;
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(Result) do
	begin
		Result[i] := TableDelCz[s[i]];
	end;
end;


function DelChars(const s: string; const C: Char): string;
var i: Integer;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		if s[i] <> C then Result := Result + s[i];
	end;
end;

function DelQuoteF(const s: string): string;
begin
	Result := s;
	if Result = '' then Exit;
	if Result[1] = '"' then Delete(Result, 1, 1);
	if Result = '' then Exit;
	if Result[Length(Result)] = '"' then SetLength(Result, Length(Result) - 1);
end;

procedure DelQuote(var s: string);
begin
	if s = '' then Exit;
	if s[1] = '"' then Delete(s, 1, 1);
	if s = '' then Exit;
	if s[Length(s)] = '"' then SetLength(s, Length(s) - 1);
end;

function DelEndSpaceF(const s: string): string;
var
	i: Integer;
begin
	for i := Length(s) downto 1 do
	begin
		if s[i] <> ' ' then
		begin
			Result := Copy(s, 1, i);
			Exit;
		end;
	end;
	Result := s;
end;

procedure DelEndSpace(var s: string);
var
	i: Integer;
begin
	for i := Length(s) downto 1 do
	begin
		if s[i] <> ' ' then
		begin
			SetLength(s, i);
			Exit;
		end;
	end;
end;

function ReadToChar(const Line: string; var InLineIndex: Integer;
	const C: Char): string;
var NumStart: Integer;
begin
	NumStart := InLineIndex;
	while (InLineIndex <= Length(Line)) and (Line[InLineIndex] <> C) do
		Inc(InLineIndex);
	Result := Copy(Line, NumStart, InLineIndex - NumStart);
	Inc(InLineIndex);
end;

function PosWW(Str, SubStr: string): Integer; // Str is word
begin
	Result := Pos(Str, SubStr);
end;

function IsSubStr(SubStr: string; Str: string): Boolean;
var
	i: Integer;
	Wor: string;
	LastWordBegin: Integer;
begin
	Result := False;
	i := 1;
	LastWordBegin := 0;
	while True do
	begin
		if (i > Length(Str)) or (Str[i] = ' ') or (Str[i] = ',') or
			(Str[i] = '.') or (Str[i] = '-') or (Str[i] = '/') or
			(Str[i] = ';') or (Str[i] = '(') or (Str[i] = ')') then
		begin
			if (LastWordBegin <> 0) then
			begin
				Wor := Copy(Str, LastWordBegin, i - LastWordBegin);
				if PosWW(Wor, SubStr) <> 0 then
				begin
					Result := True;
					Exit;
				end;
				LastWordBegin := 0;
			end;
		end
		else
		begin
			if LastWordBegin = 0 then LastWordBegin := i;
		end;
		if i > Length(Str) then Exit;
		Inc(i);
	end;
end;

{
function IsSubStr(SubStr: string; Str: string): Boolean;
var
	i: Integer;
	Wor: string;
	LastWordBegin: Integer;
begin
	Result := False;
	i := 1;
	LastWordBegin := 0;
	while True do
	begin
		if (i > Length(Str)) or (Str[i] = ' ') or (Str[i] = ',') or
			(Str[i] = '.') or (Str[i] = '-') or (Str[i] = '/') or
			(Str[i] = ';') or (Str[i] = '(') or (Str[i] = ')') then
		begin
			if (LastWordBegin <> 0) then
			begin
				Wor := Copy(Str, LastWordBegin, i - LastWordBegin);
				if Pos(Wor, SubStr) <> 0 then // ? Whole words only
				begin
					Result := True;
					Exit;
				end;
				LastWordBegin := 0;
			end;
		end
		else
		begin
			if LastWordBegin = 0 then LastWordBegin := i;
		end;
		if i > Length(Str) then Exit;
		Inc(i);
	end;
end;

function IsSubStr(SubStr: string; Str: string): Boolean;
var
	i: Integer;
	Wor: string;
	LastWordBegin: Integer;
begin
	Result := False;
	i := 1;
	LastWordBegin := 0;
	while True do
	begin
		if (i > Length(Str)) or (Str[i] = ' ') or (Str[i] = ',') or
			(Str[i] = '.') or (Str[i] = '-') or (Str[i] = '/') or
			(Str[i] = ';') or (Str[i] = '(') or (Str[i] = ')') then
		begin
			if (LastWordBegin <> 0) then
			begin
				Wor := Copy(Str, LastWordBegin, i - LastWordBegin);
				if Pos(Wor, SubStr) <> 0 then // ? Whole words only
				begin
					Result := True;
					Exit;
				end;
				LastWordBegin := 0;
			end;
		end
		else
		begin
			if LastWordBegin = 0 then LastWordBegin := i;
		end;
		if i > Length(Str) then Exit;
		Inc(i);
	end;
end;

}

initialization
	FillCharsTable;
end.
