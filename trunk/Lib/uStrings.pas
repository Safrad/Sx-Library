//* File:     Lib\uStrings.pas
//* Created:  2000-08-01
//* Modified: 2005-10-12
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uStrings;

interface

uses uTypes;

const
	CharNul = #$00;
	CharTab = #$09;
	CharSpace = #$20;
	CharHT = CharTab;
	CharVT = #$0B;
	CharLF = #$0A;// #10;
	CharCR = #$0D;// #13;
	// DOS: CharCR + CharLF; Linux/C++: CharLF; CharCR: NLP
	LineSep = CharLF;
	FullSep = CharCR + CharLF;
	HTMLSep = LineSep;
	CharBackspace = #$08;
	CharFormfeed = #$0C;
	CharBell = #$07;
	CharTimes = '×';
	Space = [CharNul, CharTab, CharLF, CharCR, CharSpace];

	FalseTrue: array[0..1] of string = ('false', 'true');

var
	HexValue: array[Char] of U1;

type
	TCharSet = set of Char;

// Strings
function PosEx(SubStr, Str: string): SG; overload;
function PosEx(SubStr, Str: string; FromPos: SG): SG; overload;
function PosEx(SubStr, Str: string; FromPos, ToPos: SG): SG; overload;

function CharCount(const s: string; const C: Char): UG;
function LowCase(ch: Char): Char;

procedure DelChars(var s: string; const SubChar: Char);
function DelCharsF(const s: string; const SubChar: Char): string;

procedure DelStr(var s: string; const SubStr: string);
function DelStrF(const s: string; const SubStr: string): string;

procedure AddQuote(var s: string);
procedure DelQuote(var s: string);
function DelQuoteF(const s: string): string;

procedure DelBeginSpace(var s: string);
function DelBeginSpaceF(const s: string): string;

procedure DelEndSpace(var s: string);
function DelEndSpaceF(const s: string): string;

procedure DelBESpace(var s: string);
function DelBESpaceF(const s: string): string;

function DelLastChar(const s: string; Right: SG = 1): string;
function DelLastNumber(const s: string): string;

function ReadToChar(const Line: string; const C: Char): string; overload;
function ReadToChar(const Line: string; var LineIndex: SG; const C: Char): string; overload;
function ReadToNewLine(const Line: string; var LineIndex: SG): string;
function ReadSGFast(const Line: string; var LineIndex: SG): SG; overload;
function ReadS8Fast(const Line: string; var LineIndex: SG): S8; overload;
function ReadFAFast(const Line: string; var LineIndex: SG): FA; overload;
function ReadSGFast(const Line: string): SG; overload;
function ReadS8Fast(const Line: string): S8; overload;
function ReadFAFast(const Line: string): FA; overload;
procedure SkipSpace(const Line: string; var LineIndex: SG);
function ReadToString(const Line: string; var LineIndex: SG;
	const S: string): string;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet): string; overload;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet; out LastChar: Char): string; overload;
function ReadToSingleChar(const Line: string; var LineIndex: Integer;
	const C: Char): string;
function StartStr(const SubStr: string; const Str: string): BG;
function IsSubStr(const SubStr: string; const Str: string): BG;

function ReplaceF(const s: string; const WhatS, ToS: string): string;
procedure Replace(var s: string; const WhatS, ToS: string);

function Code(const s: string; const Decode: BG): string;

function AddSpace(const s: string): string;
procedure CorrectDir(var s: string);
function RandomString(Size: SG): string;

implementation

uses Math;

function PosEx(SubStr, Str: string): SG;
begin
	Result := PosEx(SubStr, Str, 1, Length(Str));
end;

function PosEx(SubStr, Str: string; FromPos: SG): SG;
begin
	Result := PosEx(SubStr, Str, FromPos, Length(Str));
end;

function PosEx(SubStr, Str: string; FromPos, ToPos: SG): SG;
label LNFound;
var
	i, j: SG;
	Dir: SG;
	SubStrLen, StrLen: SG;
begin
	Result := 0;
	// Check parameters
	SubStrLen := Length(SubStr);
	if SubStrLen = 0 then Exit;
	StrLen := Length(Str);
	if SubStrLen > StrLen then Exit;

	if FromPos < 1 then FromPos := 1;
	if ToPos < 1 then ToPos := 1;

	if FromPos = ToPos then
		Dir := 0
	else if FromPos > ToPos then
	begin
		Dir := -1;
		if FromPos > StrLen - SubStrLen + 1 then FromPos := StrLen - SubStrLen + 1;
		if FromPos <= ToPos  then
		begin
			FromPos := ToPos;
			Dir := 0;
		end
		else
			Dec(ToPos);
	end
	else // if FromPos < ToPos then
	begin
		Dir := 1;
		if ToPos > StrLen - SubStrLen + 1 then ToPos := StrLen - SubStrLen + 1;
		if FromPos >= ToPos  then
		begin
			FromPos := ToPos;
			Dir := 0;
		end
		else
			Inc(ToPos);
	end;

	i := FromPos;
	repeat
		if SubStr[1] = Str[i] then
		begin
			for j := 2 to SubStrLen do
				if  Str[i - 1 + j] <> SubStr[j] then goto LNFound;
			Result := i;
			Exit;
			LNFound:
		end;
		Inc(i, Dir);
	until i = ToPos;
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

function LowCase(ch : Char): Char;
{$IFDEF PUREPASCAL}
begin
	Result := ch;
	case Result of
	'A'..'Z':  Inc(Result, Ord('a') - Ord('A'));
	end;
end;
{$ELSE}
asm
{ ->    AL      Character       }
{ <-    AL      Result          }

	CMP AL, 'A'
	JB  @@exit
	CMP AL, 'Z'
	JA  @@exit
	ADD AL, 'a' - 'A'
@@exit:
end;
{$ENDIF}

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

function DelCharsF(const s: string; const SubChar: Char): string;
begin
	Result := s;
	DelChars(Result, SubChar);
end;

procedure DelStr(var s: string; const SubStr: string);
var i, F: SG;
begin
	F := 1;
	while True do
	begin
		i := PosEx(SubStr, s, F);
		if i = 0 then
		begin
			Break;
		end;
		F := i;
		Delete(s, i, Length(SubStr));
	end;
end;

function DelStrF(const s: string; const SubStr: string): string;
begin
	Result := s;
	DelStr(Result, SubStr);
end;

procedure AddQuote(var s: string);
begin
	s := '"' + s + '"';
end;

procedure DelQuote(var s: string);
begin
	if s = '' then Exit;
	if s[1] = '"' then Delete(s, 1, 1);
	if s = '' then Exit;
	if s[Length(s)] = '"' then SetLength(s, Length(s) - 1);
end;

function DelQuoteF(const s: string): string;
begin
	Result := s;
	DelQuote(Result);
end;

procedure DelBeginSpace(var s: string);
var i: Integer;
begin
	for i := 1 to Length(s) do
	begin
		if not (s[i] in Space) then
		begin
			if i > 1 then
				Delete(s, 1, i - 1);
			Exit;
		end;
	end;
end;

function DelBeginSpaceF(const s: string): string;
begin
	Result := s;
	DelBeginSpace(Result);
end;

procedure DelEndSpace(var s: string);
var
	i: Integer;
begin
	for i := Length(s) downto 1 do
	begin
		if not (s[i] in Space) then
		begin
			SetLength(s, i);
			Exit;
		end;
	end;
end;

function DelEndSpaceF(const s: string): string;
begin
	Result := s;
	DelEndSpace(Result);
end;

procedure DelBESpace(var s: string);
begin
	DelEndSpace(s);
	DelBeginSpace(s);
end;

function DelBESpaceF(const s: string): string;
begin
	Result := s;
	DelBESpace(Result);
end;

function DelLastChar(const s: string; Right: SG = 1): string;
begin
	Right := Min(Length(s), Right);
	if Right > 0 then
	begin
		Result := s;
		SetLength(Result, Length(s) - Right);
	end
	else
		Result := '';
end;

function DelLastNumber(const s: string): string;
var i: SG;
begin
	i := Length(s);
	while i > 0 do
	begin
		case s[i] of
		'0'..'9':
		begin

		end;
		'_':
		begin
			Dec(i);
			Break;
		end
		else
			Break;
		end;
		Dec(i);
	end;
	Result := Copy(s, 1, i);
end;

function ReadToChar(const Line: string; const C: Char): string;
var LineIndex: SG;
begin
	LineIndex := 1;
	while (LineIndex <= Length(Line)) and (Line[LineIndex] <> C) do
		Inc(LineIndex);
	Result := Copy(Line, 1, LineIndex - 1);
end;

function ReadToChar(const Line: string; var LineIndex: SG; const C: Char): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while (LineIndex <= Length(Line)) and (Line[LineIndex] <> C) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
end;

function ReadToNewLine(const Line: string; var LineIndex: SG): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while (LineIndex <= Length(Line)) and (Line[LineIndex] <> CharCR) and (Line[LineIndex] <> CharLF)do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
	while (LineIndex <= Length(Line)) and (Line[LineIndex] = CharLF)do
		Inc(LineIndex);
end;

function ReadSGFast(const Line: string; var LineIndex: SG): SG; overload;
{$i StrToNum.inc}

function ReadS8Fast(const Line: string; var LineIndex: SG): S8; overload;
{$i StrToNum.inc}

function ReadFAFast(const Line: string; var LineIndex: SG): FA; overload;
{$i StrToNum.inc}

function ReadSGFast(const Line: string): SG; overload;
var LineIndex: SG;
begin
	LineIndex := 1;
	Result := ReadSGFast(Line, LineIndex);
end;

function ReadS8Fast(const Line: string): S8; overload;
var LineIndex: SG;
begin
	LineIndex := 1;
	Result := ReadS8Fast(Line, LineIndex);
end;

function ReadFAFast(const Line: string): FA; overload;
var LineIndex: SG;
begin
	LineIndex := 1;
	Result := ReadFAFast(Line, LineIndex);
end;

procedure SkipSpace(const Line: string; var LineIndex: SG);
begin
	while (LineIndex <= Length(Line)) and (Line[LineIndex] in [CharSpace, CharTab]) do
	begin
		Inc(LineIndex);
	end;
end;

function ReadToString(const Line: string; var LineIndex: SG;
	const S: string): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while True do
	begin
		if (LineIndex + Length(S) > Length(Line)) then
		begin
			LineIndex := Length(Line) + 1;
			Result := Copy(Line, StartIndex, LineIndex - StartIndex);
			Exit;
		end;
		if (Copy(Line, LineIndex, Length(S)) = S) then
		begin
			Result := Copy(Line, StartIndex, LineIndex - StartIndex);
			Inc(LineIndex, Length(S));
			Exit;
		end;
		Inc(LineIndex);
	end;
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

function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet; out LastChar: Char): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while (LineIndex <= Length(Line)) and (not (Line[LineIndex] in C)) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	if LineIndex <= Length(Line) then
		LastChar := Line[LineIndex]
	else
		LastChar := #0;
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

function StartStr(const SubStr: string; const Str: string): BG;
var i: SG;
begin
	for i := 1 to Min(Length(SubStr), Length(Str)) do
	begin
		if SubStr[i] <> Str[i] then
		begin
			Result := False;
			Exit;
		end;
	end;
	Result := True;
end;

function IsSubStr(const SubStr: string; const Str: string): BG;
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
		if (i > Length(Str)) or (Str[i] in [' ', ',', '.', '-', '/', ';', '(', ')']) then
		begin
			if (LastWordBegin <> 0) then
			begin
				Wor := Copy(Str, LastWordBegin, i - LastWordBegin);
				if Pos(Wor, SubStr) <> 0 then
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

function ReplaceF(const s: string; const WhatS, ToS: string): string;
begin
	Result := s;
	Replace(Result, WhatS, ToS);
end;

procedure Replace(var s: string; const WhatS, ToS: string);
var
	Po, Index: SG;
	WhatSLen, ToSLen: SG;
begin
	Index := 1;
	WhatSLen := Length(WhatS);
	ToSLen := Length(ToS);
	while Index + WhatSLen <= Length(s) + 1 do
	begin
		Po := PosEx(WhatS, s, Index);
		if Po <> 0 then
		begin
			Delete(s, Po, WhatSLen);
			Insert(ToS, s, Po);
			Index := Po + ToSLen;
		end
		else
			Break;
	end;
end;

function Code(const s: string; const Decode: BG): string;
var i: Integer;
begin
	SetLength(Result, Length(s));
	for i := 1 to Length(s) do
	begin
		if Decode then
			Result[i] := Chr(((Ord(s[i]) xor $ff) + $f) and $ff)
		else
			Result[i] := Chr(((Ord(s[i]) - $f) and $ff) xor $ff);
	end;
end;

function AddSpace(const s: string): string;
var Index: SG;
begin
	Result := s;
	for Index := 2 to Length(Result) do
		if Result[Index] in ['A'..'Z'] then
			if Result[Index - 1] in ['a'..'z'] then
				Insert(' ', Result, Index);
end;

procedure CorrectDir(var s: string);
var i: SG;
begin
	i := Length(s);
	if (i > 0) and (s[i] <> '\') then s := s + '\';
end;

function RandomString(Size: SG): string;
var i: SG;
begin
	SetLength(Result, Size);
	for i := 1 to Size do
		Result[i] := Char(Random(256));
end;
{
var
	i: SG;
	s, s2: string;
begin
	for i := 0 to 32767 do
	begin
		s := RandomString(i);
		s2 := AddEscape(s);
		s2 := RemoveEscape(s2);
		Assert(s = s2);
	end;

end;
}

procedure FillHexValue;
var
	c: Char;
begin
	for c := Low(Char) to High(Char) do
		case c of
		'0'..'9': HexValue[c] := Ord(c) - Ord('0');
		'A'..'Z': HexValue[c] := Ord(c) - Ord('A') + 10;
		'a'..'z': HexValue[c] := Ord(c) - Ord('a') + 10;
		else
			HexValue[c] := 0;
		end;
end;

initialization
	FillHexValue;
end.
