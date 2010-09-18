//* File:     Lib\uStrings.pas
//* Created:  2000-08-01
//* Modified: 2005-06-26
//* Version:  X.X.34.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@centrum.cz
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
function DelEndSpaceF(const s: string): string; overload;
function DelEndSpaceF(const s: ShortString): ShortString; overload;
procedure DelEndSpace(var s: string);
function DelBESpaceF(s: string): string;
procedure DelBESpace(var s: string);
function RemoveBlanks(s: string): string;
function DeleteLastEnter(s: string): string;
function DeleteLastChar(s: string): string;

function ReadToChar(const Line: string; const C: Char): string; overload;
function ReadToChar(const Line: string; var LineIndex: SG; const C: Char): string; overload;
function ReadToNewLine(const Line: string; var LineIndex: SG): string;
function ReadNum(const Line: string; var LineIndex: SG): SG;
procedure SkipSpace(const Line: string; var LineIndex: SG);
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

implementation

uses
	Math,
	uFind;
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

function DelEndSpaceF(const s: ShortString): ShortString;
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

function ReadNum(const Line: string; var LineIndex: SG): SG;
begin
	Result := 0;
	while (LineIndex <= Length(Line)) and (Line[LineIndex] in ['0'..'9']) do
	begin
		Result := Result * 10;
		Result := Result + Ord(Line[LineIndex]) - Ord('0');
		Inc(LineIndex);
	end;
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

initialization
	FillHexValue;
end.
