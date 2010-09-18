//* File:     Lib\uStrings.pas
//* Created:  2000-08-01
//* Modified: 2005-06-26
//* Version:  X.X.34.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@centrum.cz
//* Web:      http://safrad.webzdarma.cz

unit uStrings;

interface

uses uAdd;

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
	HTMLSep = FullSep;
	CharBackspace = #$08;
	CharFormfeed = #$0C;
	CharBell = #$07;
	CharTimes = '×';

var
	HexValue: array[Char] of U1;

type
	TCharSet = set of Char;

function LowCase(ch: Char): Char;

function DelCharsF(const s: string; const SubChar: Char): string;
procedure DelChars(var s: AnsiString; const SubChar: Char); overload;
procedure DelChars(var s: ShortString; const SubChar: Char); overload;

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
function RemoveBlanks(s: string): string;
function DeleteLastEnter(s: string): string;
function DeleteLastChar(s: string): string;

function ReadToChar(const Line: string; var LineIndex: SG;
	const C: Char): string;
function ReadToString(const Line: string; var LineIndex: SG;
	const S: string): string;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet): string; overload;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet; out LastChar: Char): string; overload;
function ReadToSingleChar(const Line: string; var LineIndex: Integer;
	const C: Char): string;
function PosWW(Str, SubStr: string): Integer;
function StartStr(SubStr: string; Str: string): Boolean;
function IsSubStr(SubStr: string; Str: string): Boolean;

function InsChar(const CharCount: Integer; C: Char): string;

function ReplaceF(s: string; const WhatS, ToS: string): string;
procedure Replace(var s: string; const WhatS, ToS: string);

function Code(s: ShortString; Decode: Boolean): ShortString; overload
function Code(s: AnsiString; Decode: Boolean): AnsiString; overload;

function RemoveEscape(s: string): string;
function AddEscape(s: string): string; // 2.8x larger for random data

function RandomString(Size: SG): string;

implementation

uses
	Math,
	uFind, uError;
var
	TableWordSep: array[0..7] of Char = (' ', ',', '.', '-', '/', ';', '(', ')');

function        LowCase( ch : Char ) : Char;
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

				CMP     AL,'A'
				JB      @@exit
				CMP     AL,'Z'
				JA      @@exit
				ADD     AL,'a' - 'A'
@@exit:
end;
{$ENDIF}

function DelCharsF(const s: string; const SubChar: Char): string;
var i: Integer;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		if s[i] <> SubChar then Result := Result + s[i];
	end;
end;

procedure DelChars(var s: AnsiString; const SubChar: Char);
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

procedure DelChars(var s: ShortString; const SubChar: Char);
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

function RemoveBlanks(s: string): string;
begin
	Result := s;
	Replace(Result, ' ', '');
	Replace(Result, CharTab, '');
	Replace(Result, CharCR, '');
	Replace(Result, CharLF, '');
end;

function DeleteLastEnter(s: string): string;
begin
	Result := s;
	if Length(Result) >= 1 then
	begin
		if s[Length(Result)] = CharLF then
			SetLength(Result, Length(Result) - 1);
	end;
	if Length(Result) >= 1 then
	begin
		if s[Length(Result)] = CharCR then
			SetLength(Result, Length(Result) - 1);
	end;
end;

function DeleteLastChar(s: string): string;
begin
	if Length(s) > 1 then
		Result := Copy(s, 1, Length(s) - 1)
	else
		Result := '';
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

function ReadToString(const Line: string; var LineIndex: SG;
	const S: string): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	while (LineIndex + Length(S) <= Length(Line) + 1) and (Copy(Line, LineIndex, Length(S)) <> S) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex, Length(S));
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

function PosWW(Str, SubStr: string): Integer; // Str is word
begin
	Result := Pos(Str, SubStr);
end;

function StartStr(SubStr: string; Str: string): Boolean;
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
	while Index + Length(WhatS) <= Length(s) + 1 do
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

function Code(s: ShortString; Decode: Boolean): ShortString;
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

function Code(s: AnsiString; Decode: Boolean): AnsiString;
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

procedure FillHexValue;
var
	c: Char;
begin
	for c := Low(Char) to High(Char) do
		case c of
		'0'..'9': HexValue[c] := Ord(c) - Ord('0');
		'a'..'z': HexValue[c] := Ord(c) - Ord('a') + 10;
		'A'..'Z': HexValue[c] := Ord(c) - Ord('A') + 10;
		else
			HexValue[c] := 255;
		end;
end;

{
// Standard Escape Sequences:
\b       backspace
\f       formfeed
\n       new line
\r       carriage return
\t       horizontal tab
\'       single quote
\0       null


Sequence	Value	Char	What it does
\a	0x07	BEL	Audible bell
\b	0x08	BS	Backspace
\f	0x0C	FF	Formfeed
\n	0x0A	LF	Newline (linefeed)
\r	0x0D	CR	Carriage return
\t	0x09	HT	Tab (horizontal)
\v	0x0B	VT	Vertical tab
\\	0x5c	\	Backslash
\'	0x27	'	Single quote (apostrophe)
\"	0x22	"	Double quote
\?	0x3F	?	Question mark
\O		any	O=a string of up to three octal digits
\xH		any	H=a string of hex digits
\XH		any	H=a string of hex digits
}

function RemoveEscape(s: string): string;
var
	i, j: SG;
	x, v: U1;
	Special: BG;
begin
	Result := '';
	i := 1;
	Special := False;
	while i <= Length(s) do
	begin
		if s[i] = '\' then
		begin
			if Special then
			begin
				Result := Result + s[i];
				Special := False;
			end
			else
				Special := True;
		end
		else
			if Special then
			begin
				case s[i] of
				'a': Result := Result + CharBell;
				'b': Result := Result + CharBackspace;
				'e', 'E': Result := Result + #$1B;
				'f': Result := Result + CharFormfeed;
				'n': Result := Result + CharLF;
				'r': Result := Result + CharCR;
				't': Result := Result + CharHT;
{				'u', 'U':
				begin
				end;}
				'v': 	Result := Result + CharVT;
				'x':
				begin
					Inc(i);
					x := 0;
					while True do
					begin
						if i <= Length(s) then
							v := HexValue[s[i]]
						else
							v := 16;
						if (v < 16) then
						begin
							x := (x shl 4) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
					end;
				end;
				'''': Result := Result + '''';
				'"': Result := Result + '"';
				'?': Result := Result + '?';
				'0'..'7': //Result := Result + Char(Ord(s[i]) - Ord('0'));//CharNull;
				begin
					x := 0;
					j := 0;
					while True do
					begin
						if (i <= Length(s)) and (j < 3) then
							v := HexValue[s[i]]
						else
							v := 8;
						if (v < 8) then
						begin
							x := (x shl 3) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
						Inc(j);
					end;
				end;
				else
					Result := Result + s[i];
					{$ifopt d+}
					IE(535);
					{$endif}
				end;
				Special := False;
			end
			else
				Result := Result + s[i];
		Inc(i);
	end;

end;

function AddEscape(s: string): string;
var i: SG;
begin
	Result := '';
	i := 1;
	while i <= Length(s) do
	begin
		case s[i] of
		'\': Result := Result + '\\';
		CharBell: Result := Result + '\a';
		CharBackspace: Result := Result + '\b';
		#$1B: Result := Result + '\e'; // 'E'
		CharFormfeed: Result := Result + '\f';
		CharLF: Result := Result + '\n';
		CharCR: Result := Result + '\r';
		CharHT: Result := Result + '\t';
		CharVT: Result := Result + '\v';
//		'''': Result := Result + '\''';
//		#0..#6: Result := Result + '\' + Char(Ord(s[i]) + Ord('0'));//CharNull;
		#$20..#$5B, #$5D..#$7F: // ASCII
			Result := Result + s[i];
		else
		begin
			NumericBase := 8;
			Result := Result + '\' + NToS(Ord(s[i]), '000');  // NumToStr(Ord(s[i]), 8);
			NumericBase := 10;
		end;
		end;
		Inc(i);
	end;
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
	for i := 0 to 20000 do
	begin
		s := RandomString(i);
		s2 := AddEscape(s);
		s2 := RemoveEscape(s2);
		if s <> s2 then
			Nop;
	end;

end;
}
initialization
	FillHexValue;
end.
