//* File:     Lib\uStrings.pas
//* Created:  2000-08-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uStrings;

interface

uses uAdd;

const
	CharNul = #$0;
	CharTab = #$9;
	CharHT = CharTab;
	CharLF = #$A;// #10;
	CharCR = #$D;// #13;
	// DOS: CharCR + CharLF; Linux/C++: CharLF; CharCR: NLP
	LineSep = CharLF;
	FullSep = CharCR + CharLF;

type
	TCharSet = set of Char;

function DelCharsF(const s: string; const SubChar: Char): string;
procedure DelChars(var s: string; const SubChar: Char);

function DelStrF(s: string; const SubStr: string): string;
procedure DelStr(var s: string; const SubStr: string);

function DelQuoteF(const s: string): string;
procedure DelQuote(var s: string);

function CharCount(const s: string; const C: Char): UG;

function DelBeginSpaceF(const s: string): string;
procedure DelBeginSpace(var s: string);
function DelEndSpaceF(const s: string): string;
procedure DelEndSpace(var s: string);
function DelBESpaceF(s: string): string;
procedure DelBESpace(var s: string);

function ReadToChar(const Line: string; var LineIndex: SG;
	const C: Char): string;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet): string;
function ReadToSingleChar(const Line: string; var LineIndex: Integer;
	const C: Char): string;
function PosWW(Str, SubStr: string): Integer;
function IsSubStr(SubStr: string; Str: string): Boolean;

function InsChar(const CharCount: Integer; C: Char): string;

function ReplaceF(s: string; const WhatS, ToS: string): string;
procedure Replace(var s: string; const WhatS, ToS: string);

implementation

uses uFind;
var
	TableWordSep: array[0..7] of Char = (' ', ',', '.', '-', '/', ';', '(', ')');

function DelCharsF(const s: string; const SubChar: Char): string;
var i: Integer;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		if s[i] <> SubChar then Result := Result + s[i];
	end;
end;

procedure DelChars(var s: string; const SubChar: Char);
var i: Integer;
begin
	i := 1;
	while i <= Length(s) do
	begin
		if s[i] = SubChar then
			Delete(s, i, 1)
		else
			Inc(i);
	end;
end;

function DelStrF(s: string; const SubStr: string): string;
var i, F: SG;
begin
	Result := '';
	F := 1;
	while True do
	begin
		i := Find(SubStr, s, F);
		if i = 0 then
		begin
			Break;
		end;
		F := i;
		Delete(s, i, Length(SubStr));
	end;
	Result := s;
end;

procedure DelStr(var s: string; const SubStr: string);
var i, F: SG;
begin
	F := 1;
	while True do
	begin
		i := Find(SubStr, s, F);
		if i = 0 then
		begin
			Break;
		end;
		F := i;
		Delete(s, i, Length(SubStr));
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
		if s[i] = C then Inc(Result);
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

function ReadToChar(const Line: string; var LineIndex: SG;
	const C: Char): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while (LineIndex <= Length(Line)) and (Line[LineIndex] <> C) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
end;

function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while (LineIndex <= Length(Line)) and (not (Line[LineIndex] in C)) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
end;

function ReadToSingleChar(const Line: string; var LineIndex: Integer;
	const C: Char): string;
var StartIndex: Integer;
begin
	StartIndex := LineIndex;
	while (LineIndex <= Length(Line)) do
	begin
		if (Line[LineIndex] = C) then
		begin
			if LineIndex + 1 <= Length(Line) then
			begin
				if (Line[LineIndex + 1] = C) then
					Inc(LineIndex)
				else
				begin
					Break;
				end;
			end
			else
				Break;
		end;
		Inc(LineIndex);
	end;
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
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

function ReplaceF(s: string; const WhatS, ToS: string): string;
var Po: SG;
begin
	Result := '';
	while True do
	begin
		Po := Pos(WhatS, s);
		if Po <> 0 then
		begin
			Result := Result + Copy(s, 1, Po - 1) + ToS;
			Delete(s, 1, Po - 1 + Length(WhatS));
		end
		else
			Break;
	end;
	Result := Result + s;
end;

procedure Replace(var s: string; const WhatS, ToS: string);
var Po, Index: SG;
begin
	Index := 1;
	while Index < Length(s) do
	begin
//		Po := Pos(WhatS, s);
		Po := Find(WhatS, s, Index);
		if Po <> 0 then
		begin
			Delete(s, Po, Length(WhatS));
			Insert(ToS, s, Po);
			Index := Po + Length(ToS);
		end
		else
			Break;
	end;
end;

end.
