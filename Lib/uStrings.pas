// Build: 08/2000-08/2000 Author: Safranek David

unit uStrings;

interface

uses uAdd;

function DelCharsF(const s: string; const C: Char): string;
procedure DelChars(var s: string; const C: Char);

function DelQuoteF(const s: string): string;
procedure DelQuote(var s: string);

function CharCount(const s: string; const C: Char): UG;

function DelBeginSpaceF(const s: string): string;
procedure DelBeginSpace(var s: string);
function DelEndSpaceF(const s: string): string;
procedure DelEndSpace(var s: string);
function DelBESpaceF(s: string): string;
procedure DelBESpace(var s: string);

function ReadToChar(const Line: string; var InLineIndex: SG;
	const C: Char): string;
function ReadToSingleChar(const Line: string; var InLineIndex: Integer;
	const C: Char): string;
function PosWW(Str, SubStr: string): Integer;
function IsSubStr(SubStr: string; Str: string): Boolean;

function InsChar(const CharCount: Integer; C: Char): string;

implementation

var
	TableWordSep: array[0..7] of Char = (' ', ',', '.', '-', '/', ';', '(', ')');

function DelCharsF(const s: string; const C: Char): string;
var i: Integer;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		if s[i] <> C then Result := Result + s[i];
	end;
end;

procedure DelChars(var s: string; const C: Char);
var i: Integer;
begin
	i := 1;
	while i <= Length(s) do
	begin
		if s[i] = C then
			Delete(s, i, 1)
		else
			Inc(i);
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

function CharCount(const s: string; const C: Char): UG;
var i: SG;
begin
	Result := 0;
	for i := 1 to Length(s) do
	begin
		if s[i] = #9 then Inc(Result);
	end;
end;

function DelBeginSpaceF(const s: string): string;
var i: Integer;
begin
	Result := s;
	for i := 1 to Length(Result) do
	begin
		if Result[i] <> ' ' then
		begin
			if i <> 1 then
				Delete(Result, 1, i - 1);
			Break;
		end;
	end;
end;

procedure DelBeginSpace(var s: string);
var i: Integer;
begin
	for i := 1 to Length(s) do
	begin
		if s[i] <> ' ' then
		begin
			if i <> 1 then
				Delete(s, 1, i - 1);
			Break;
		end;
	end;
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

function DelBESpaceF(s: string): string;
var i: Integer;
begin
	for i := Length(s) downto 1 do
	begin
		if s[i] <> ' ' then
		begin
			if i <> Length(s) then
				SetLength(s, i);
			Break;
		end;
	end;
	for i := 1 to Length(s) do
	begin
		if s[i] <> ' ' then
		begin
			if i <> 1 then
				Delete(s, 1, i - 1);
			Break;
		end;
	end;
	Result := s;
end;

procedure DelBESpace(var s: string);
var i: Integer;
begin
	for i := Length(s) downto 1 do
	begin
		if s[i] <> ' ' then
		begin
			if i <> Length(s) then
				SetLength(s, i);
			Break;
		end;
	end;
	for i := 1 to Length(s) do
	begin
		if s[i] <> ' ' then
		begin
			if i <> 1 then
				Delete(s, 1, i - 1);
			Break;
		end;
	end;
end;

function ReadToChar(const Line: string; var InLineIndex: SG;
	const C: Char): string;
var NumStart: SG;
begin
	NumStart := InLineIndex;
	while (InLineIndex <= Length(Line)) and (Line[InLineIndex] <> C) do
		Inc(InLineIndex);
	Result := Copy(Line, NumStart, InLineIndex - NumStart);
	Inc(InLineIndex);
end;

function ReadToSingleChar(const Line: string; var InLineIndex: Integer;
	const C: Char): string;
var NumStart: Integer;
begin
	NumStart := InLineIndex;
	while (InLineIndex <= Length(Line)) do
	begin
		if (Line[InLineIndex] = C) then
		begin
			if InLineIndex + 1 <= Length(Line) then
			begin
				if (Line[InLineIndex + 1] = C) then
					Inc(InLineIndex)
				else
				begin
					Break;
				end;
			end
			else
				Break;
		end;
		Inc(InLineIndex);
	end;
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

function InsChar(const CharCount: Integer; C: Char): string;
begin
	if CharCount <= 0 then
	begin
		Result := '';
		Exit;
	end;
	SetLength(Result, CharCount);
	FillChar(Result[1], CharCount, C);
end;

end.
