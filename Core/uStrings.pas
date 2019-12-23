unit uStrings;

{$ZEROBASEDSTRINGS OFF}

interface

uses
  SysUtils, TypInfo,
  uTypes, uChar;

const
	WindowsSep = CharCR + CharLF; // Windows, DOS
  LinuxSep = CharLF; // Linux, Macintosh

	LineSep = LinuxSep; // Default
  FullSep = WindowsSep; // Required by some Windows components
	FileSep = WindowsSep;

  CharSpace = uChar.CharSpace; // Backward compatibility

	CharTab = CharHT;
  CharTimes = CharMultiplicationSign;

	Space = [CharNull, CharHT, CharLF, CharVT, CharCR, CharSpace, CharUnbrokableSpace];
 	BreakableSpace = [CharNull, CharHT, CharLF, CharVT, CharCR, CharSpace];
	cDialogSuffix = CharHorizontalEllipsis;

	EnumPrefixLength = 2;
	NAStr = 'N/A';
	CharNone = CharEmDash;
	CharError = '~';
	FalseTrue: array[0..1] of string = ('false', 'true');
	NoYes: array[0..1] of string = ('No', 'Yes');

  WikipediaURLPrefix = 'https://en.wikipedia.org/wiki/';

var
	HexValue: array[AnsiChar] of U1;

type
	TCharSet = TSysCharSet; // set of AnsiChar;

// Strings
function PosEx(const SubStr, Str: string): SG; overload;
function PosEx(const SubStr, Str: string; FromPos: SG): SG; overload;
function PosEx(const SubStr, Str: string; FromPos, ToPos: SG): SG; overload;

function CharCount(const s: string; const C: Char): UG;
function LowCase(ch: AnsiChar): AnsiChar; overload;
function LowCase(ch: WideChar): WideChar; overload;

procedure DelChars(var s: string; const SubChar: Char);
procedure DelStrings(var s: string; const SubChar: array of string);
function DelCharsF(const s: string; const SubChar: Char): string;
function DelStringsF(const s: string; const SubChar: array of string): string;

procedure DelStr(var s: string; const SubStr: string);
function DelStrF(const s: string; const SubStr: string): string;

function Indent(const Level: SG): string;

function AddSingleQuoteF(const s: string): string;
procedure AddQuote(var s: string);
function AddQuoteF(const s: string): string;
//function AddCharLFAfterCRF(const s: string): string;
function AddFullEnter(const s: string): string;
function CSVCell(const s: string): string;
procedure DelQuote(var s: string);
function DelQuoteF(const s: string): string;

function DelPathSep(const s: string): string;

procedure DelBeginChars(var s: string; const C: TCharSet);
procedure DelEndChar(var s: string; const C: Char);
procedure DelEndChars(var s: string; const C: TCharSet);

procedure DelBeginSpace(var s: string);
function DelBeginSpaceF(const s: string): string;

procedure DelEndSpace(var s: string);
function DelEndSpaceF(const s: string): string;

procedure DelBESpace(var s: string);
function DelBESpaceF(const s: string): string;

function FirstChar(const s: string): Char;
function LastChar(const s: string): Char;
function CharAt(const s: string; const Index: SG): Char;
function DelFirstChar(const s: string; Left: SG = 1): string;
function DelLastChar(const s: string; Right: SG = 1): string;
function DelLastNumber(const s: string): string;


function ReadToChar(const Line: string; const C: Char): string; overload;
function ReadToChar(const Line: string; var LineIndex: SG; const C: Char): string; overload;
function ReadToCharQuoted(const Line: string; var LineIndex: SG; const C: Char; const QuoteChar: Char): string;
function ReadToNewLine(const Line: string; var LineIndex: SG): string;
function ReadSGFast(const Line: string; var LineIndex: SG): SG; overload;
function ReadS8Fast(const Line: string; var LineIndex: SG): S8; overload;
function ReadSGFast(const Line: string): SG; overload;
function ReadS8Fast(const Line: string): S8; overload;
procedure SkipSpace(const Line: string; var LineIndex: SG);
function ReadToString(const Line: string; var LineIndex: SG;
	const S: string): string; overload;
function ReadToString(const Line: string;	const S: string): string; overload;
function ReadToStrings(const Line: string; var ALineIndex: SG;
	const APatterns: TArrayOfString): string;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet): string; overload;
function ReadToCharsEx(const Line: string; var LineIndex: SG;
	const C: TCharSet): string; overload;
function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet; out LastChar: Char): string; overload;
function ReadToSingleChar(const Line: string; var LineIndex: Integer;
	const C: Char): string;
function StartStr(const SubStr: string; const Str: string; const AStrOffset: SG = 1): BG;
function EndStr(const SubStr: string; const Str: string): BG;
procedure RemoveSuffix(const SubStr: string; var Str: string);
function RemoveSuffixF(const SubStr: string; const Str: string): string;
function IsSubStr(const SubStr: string; const Str: string): BG;
function OneLine(const s: string): string;
procedure RemoveComment(var s: string);
function CapitalCase(const s: string): string;

function ReplaceF(const s: string; const WhatS, ToS: string): string; overload;
function ReplaceF(const s: string; const WhatS, ToS: array of string): string; overload;
function ReplacePatternF(const s: string; const Pattern: array of TStringPair): string;
procedure Replace(var s: string; const WhatS, ToS: string); overload;
procedure Replace(var s: string; const WhatS, ToS: array of string); overload;
procedure ReplacePattern(var s: string; const Pattern: array of TStringPair);

function ReplaceParam(const Text: string; const Param: array of string): string; overload;

function DoubleBackSlash(const s: string): string;
function RemoveSingleAmp(const s: string): string;

function AddSpace(const s: string): string;
procedure AppendStr(var Dest: TFileName; const Source: string); overload;
procedure AppendStr(var Dest: string; const Source: string); overload;
procedure AppendStrSeparator(var Dest: string; const Source: string; const ASeparator: string);
procedure AppendLine(var Dest: string; const Source: string);
procedure AddPrefix(var Dest: string; const Source: string);
function Plural(const Number: SG): string;
procedure CorrectDir(var s: string);
function CorrectDirF(const s: string): string;
function RandomString(const Size: SG; const FromChar: U1 = 1; const ToChar: U1 = 255): string;
function RandomText(const Size: SG): string;

procedure EnumToStr(const TypeInfo: PTypeInfo; out AString: array of string; const EnumPrefixLength: SG = 2);
procedure EnumToStrEx(const TypeInfo: PTypeInfo; out AString: array of string; const EnumPrefixLength: SG = 2);
function ButtonNameToFileName(const Name: string): string;
function ComponentName(const Name: string): string;

function HashCode(const s: string): U4;

function OddEven(const Index: SG): string;

function IsNumber(const AText: string): Boolean;
function Match(const Text: string; const Filter: string): BG;
function FileMatch(const FileName: string; const Filter: string): BG;

function SplitStringEx(const AText, ADelimiter: string): TArrayOfString;

function PadLeft(const AText: string; const ACharCount: SG; const AFillChar: Char = CharSpace): string;
function PadCenter(const AText: string; const ACharCount: SG; const AFillChar: Char = CharSpace): string;
function PadRight(const AText: string; const ACharCount: SG; const AFillChar: Char = CharSpace): string;

function PropertiesToString(const Keys: array of string; const Values: array of string; const ASeparator: string = ';'): string;

function CompareStringLogical(AValue1, AValue2: string): TCompareResult;

function U1ToOctalString(AValue: U1): string;
function CharToDigit(const AChar: Char): SG;

function ProgramInput(const AText: string): string;
function ProgramOutput(const AText: string): string;

function JoinFileNameAndParameters(const AFileName: TFileName; const AParameters: string): string;

implementation

uses
	Math,
	uMath, uCharTable;

function PosEx(const SubStr, Str: string): SG;
begin
	Result := PosEx(SubStr, Str, 1, Length(Str));
end;

function PosEx(const SubStr, Str: string; FromPos: SG): SG;
begin
	Result := PosEx(SubStr, Str, FromPos, Length(Str));
end;

function PosEx(const SubStr, Str: string; FromPos, ToPos: SG): SG;
var
	i: SG;
	Dir: SG;
	SubStrLen, StrLen: SG;
begin
	Result := 0;
	// Check parameters
	SubStrLen := Length(SubStr);
	if SubStrLen = 0 then
    Exit;
	StrLen := Length(Str);
	if SubStrLen > StrLen then
    Exit;

	if FromPos < 1 then
    FromPos := 1;
	if ToPos < 1 then
    ToPos := 1;

	if FromPos = ToPos then
		Dir := 0
	else if FromPos > ToPos then
	begin
		Dir := -1;
		if FromPos > StrLen - SubStrLen + 1 then
      FromPos := StrLen - SubStrLen + 1;
		if FromPos <= ToPos then
		begin
			FromPos := ToPos;
			Dir := 0;
      if FromPos >= StrLen then
        Exit;
		end
		else
			Dec(ToPos);
	end
	else // if FromPos < ToPos then
	begin
		Dir := 1;
		if ToPos > StrLen - SubStrLen + 1 then
       ToPos := StrLen - SubStrLen + 1;
		if FromPos >= ToPos then
		begin
			FromPos := ToPos;
			Dir := 0;
      if FromPos >= StrLen then
        Exit;
		end
		else
			Inc(ToPos);
	end;

	i := FromPos;
	repeat
		if SubStr[1] = Str[i] then // Optimization
		begin
      if StartStr(SubStr, Str, i) then
      begin
  			Result := i;
  			Exit;
      end;
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

function LowCase(ch : AnsiChar): AnsiChar;
{$IF not (defined(ASSEMBLER) and defined(CPUX86)) }
begin
	Result := ch;
	case Result of
	AnsiChar('A')..AnsiChar('Z'):
    Inc(Result, Ord('a') - Ord('A'));
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

function LowCase(Ch: WideChar): WideChar;
begin
	Result := Ch;
	case Ch of
		'A'..'Z':
			Result := WideChar(Word(Ch) or $0020);
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

procedure DelStrings(var s: string; const SubChar: array of string);
var
	Empty: array of string;
begin
	SetLength(Empty, Length(SubChar));
	Replace(s, SubChar, Empty);
end;

function DelCharsF(const s: string; const SubChar: Char): string;
begin
	Result := s;
	DelChars(Result, SubChar);
end;

function DelStringsF(const s: string; const SubChar: array of string): string;
var
	Empty: array of string;
begin
	SetLength(Empty, Length(SubChar));
	Result := ReplaceF(s, SubChar, Empty);
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

function Indent(const Level: SG): string;
begin
	Result := StringOfChar(CharTab, Level);
{	if Level = 0 then
		Result := ''
	else
	begin
		SetLength(Result, Level);
		FillChar(Result[1], Level * SizeOf(Result[1]), CharTab);
	end;}
end;

function AddSingleQuoteF(const s: string): string;
begin
	Result := '''' + s + '''';
end;

procedure AddQuote(var s: string);
begin
	s := '"' + s + '"';
end;

function AddQuoteF(const s: string): string;
begin
	Result := '"' + s + '"';
end;

function AddCharLFAfterCRF(const s: string): string;
var
	i: SG;
begin
	Result := s;
	i := 1;
	while i < Length(Result) do
	begin
		if Result[i] = CharCR then
		begin
			if CharAt(Result, i + 1) <> CharLF then
			begin
				Insert(CharLF, Result, i + 1);
				Inc(i);
			end;
		end;
		Inc(i);
	end;
end;

function AddCharCRBeforeLFF(const s: string): string;
var
	i: SG;
begin
	Result := s;
	i := 1;
	while i < Length(Result) do
	begin
		if Result[i] = CharLF then
		begin
			if CharAt(Result, i - 1) <> CharCR then
			begin
				Insert(CharCR, Result, i);
				Inc(i);
			end;
		end;
		Inc(i);
	end;
end;

function AddFullEnter(const s: string): string;
begin
	Result := AddCharLFAfterCRF(s);
	Result := AddCharCRBeforeLFF(Result);
end;

function CSVCell(const s: string): string;
begin
	if
		(PosEx('"', s) = 0) and
		(PosEx(';', s) = 0) and
		(PosEx(',', s) = 0) and
		(PosEx(CharTab, s) = 0) then
	begin
		Result := s
	end
	else
	begin
		Result := AddQuoteF(ReplaceF(s, '"', '""'));
	end;
end;

procedure DelQuote(var s: string);
begin
	if s = '' then Exit;
	if s[1] = '"' then Delete(s, 1, 1);
	if s = '' then Exit;
	if LastChar(s) = '"' then SetLength(s, Length(s) - 1);
end;

function DelQuoteF(const s: string): string;
begin
	Result := s;
	DelQuote(Result);
end;

function DelPathSep(const s: string): string;
begin
	if CharInSet(LastChar(s), ['/', '\']) then
		Result := DelLastChar(s)
	else
		Result := s;
end;

procedure DelBeginChars(var s: string; const C: TCharSet);
var i: Integer;
begin
	i := 1;
	while i <= Length(s) do
	begin
		if not CharInSet(s[i], C) then
		begin
			Break;
		end;
		Inc(i);
	end;
	if i > 1 then
		Delete(s, 1, i - 1);
end;

procedure DelEndChar(var s: string; const C: Char);
var
	i: SG;
begin
	i := Length(s);
	while i > 0 do
	begin
		if s[i] <> C then
		begin
			Break;
		end;
		Dec(i);
	end;
	SetLength(s, i);
end;

procedure DelEndChars(var s: string; const C: TCharSet);
var
	i: SG;
begin
	i := Length(s);
	while i > 0 do
	begin
		if not CharInSet(s[i], C) then
		begin
			Break;
		end;
		Dec(i);
	end;
	SetLength(s, i);
end;

procedure DelBeginSpace(var s: string);
begin
	DelBeginChars(s, Space);
end;

function DelBeginSpaceF(const s: string): string;
begin
	Result := s;
	DelBeginSpace(Result);
end;

procedure DelEndSpace(var s: string);
begin
	DelEndChars(s, Space);
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

function DelFirstChar(const s: string; Left: SG = 1): string;
begin
	Result := Copy(s, Left + 1, MaxInt);
{	Left := Min(Length(s), Left);
	if Left > 0 then
	begin
		Result := Copy(s, Left, MaxInt);
	end
	else
		Result := '';}
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

function FirstChar(const s: string): Char;
begin
	if Length(s) <= 0 then
		Result := CharNull
	else
		Result := s[1];
end;

function LastChar(const s: string): Char;
begin
	if Length(s) <= 0 then
		Result := CharNull
	else
		Result := s[Length(s)];
end;

function CharAt(const s: string; const Index: SG): Char;
begin
	if (Index <= 0) or (Index > Length(s)) then
		Result := CharNull
	else
		Result := s[Index];
end;

function ReadToChar(const Line: string; const C: Char): string;
var
	LineIndex: SG;
	LineLength: SG;
begin
	LineIndex := 1;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and (Line[LineIndex] <> C) do
		Inc(LineIndex);
	Result := Copy(Line, 1, LineIndex - 1);
end;

function ReadToChar(const Line: string; var LineIndex: SG; const C: Char): string;
var
	StartIndex: SG;
	LineLength: SG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and (Line[LineIndex] <> C) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
end;

function ReadToCharQuoted(const Line: string; var LineIndex: SG; const C: Char; const QuoteChar: Char): string;
var
	StartIndex: SG;
	LineLength: SG;
  InQuote: BG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
  InQuote := False;
	while (LineIndex <= LineLength) do
  begin
    if Line[LineIndex] = QuoteChar then
      InQuote := not InQuote;
    if (InQuote = False) and (Line[LineIndex] = C) then
      Break;
		Inc(LineIndex);
  end;
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
end;

function ReadToNewLine(const Line: string; var LineIndex: SG): string;
var
	StartIndex: SG;
	LineLength: SG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and (Line[LineIndex] <> CharCR) and (Line[LineIndex] <> CharLF) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	if (LineIndex <= LineLength) then
	begin
		if (Line[LineIndex] = CharCR) then
		begin
			Inc(LineIndex); // Accept CR
			if (LineIndex <= LineLength) and (Line[LineIndex] = CharLF) then
				Inc(LineIndex); // Accept LF
		end
		else // if (Line[LineIndex] = CharLF) then
			Inc(LineIndex); // Accept LF
	end;
end;

function ReadSGFast(const Line: string; var LineIndex: SG): SG; overload;
{$i StrToNum.inc}

function ReadS8Fast(const Line: string; var LineIndex: SG): S8; overload;
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

procedure SkipSpace(const Line: string; var LineIndex: SG);
var
	LineLength: SG;
begin
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and CharInSet(Line[LineIndex], [CharSpace, CharTab]) do
	begin
		Inc(LineIndex);
	end;
end;

function ReadToString(const Line: string; var LineIndex: SG;
	const S: string): string;
var StartIndex: SG;
begin
	StartIndex := LineIndex;
	LineIndex := PosEx(S, Line, LineIndex);
	if LineIndex = 0 then
	begin
		LineIndex := Length(Line) + 1;
		Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	end
	else
	begin
		Result := Copy(Line, StartIndex, LineIndex - StartIndex);
		Inc(LineIndex, Length(S));
	end;

(*	StartIndex := LineIndex;
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
	end; *)
end;

function ReadToString(const Line: string;	const S: string): string;
var LineIndex: SG;
begin
	LineIndex := 1;
	Result := ReadToString(Line, LineIndex, S);
end;

function ReadToStrings(const Line: string; var ALineIndex: SG;
	const APatterns: TArrayOfString): string;
var
  BestFindLineIndex, FindLineIndex: SG;
  BestFindStr: string;
  i: SG;
begin
	BestFindLineIndex := High(BestFindLineIndex);
  for i := 0 to Length(APatterns) - 1 do
  begin
  	FindLineIndex := PosEx(APatterns[i], Line, ALineIndex);
    if (FindLineIndex <> 0) and (FindLineIndex < BestFindLineIndex) then
    begin
      BestFindStr := APatterns[i];
      BestFindLineIndex := FindLineIndex;
    end;
  end;

	if BestFindLineIndex = High(FindLineIndex) then
	begin
    // Not found
		Result := Copy(Line, ALineIndex);
		ALineIndex := Length(Line) + 1; // One char after end of input
	end
	else
	begin
    // Found
		Result := Copy(Line, ALineIndex, BestFindLineIndex - ALineIndex);
		ALineIndex := BestFindLineIndex; // First char of pattern
	end;
end;

function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet): string;
var
	StartIndex: SG;
	LineLength: SG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and (not CharInSet(Line[LineIndex], C)) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	Inc(LineIndex);
end;

function ReadToCharsEx(const Line: string; var LineIndex: SG;
	const C: TCharSet): string;
var
	StartIndex: SG;
	LineLength: SG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and (not CharInSet(Line[LineIndex], C)) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex + 1);
	Inc(LineIndex);
end;

function ReadToChars(const Line: string; var LineIndex: SG;
	const C: TCharSet; out LastChar: Char): string;
var
	StartIndex: SG;
	LineLength: SG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) and (not CharInSet(Line[LineIndex], C)) do
		Inc(LineIndex);
	Result := Copy(Line, StartIndex, LineIndex - StartIndex);
	if LineIndex <= LineLength then
		LastChar := Line[LineIndex]
	else
		LastChar := CharNull;
	Inc(LineIndex);
end;

function ReadToSingleChar(const Line: string; var LineIndex: Integer;
	const C: Char): string;
var
	StartIndex: Integer;
	LineLength: SG;
begin
	StartIndex := LineIndex;
	LineLength := Length(Line);
	while (LineIndex <= LineLength) do
	begin
		if (Line[LineIndex] = C) then
		begin
			if LineIndex + 1 <= LineLength then
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

function StartStr(const SubStr: string; const Str: string; const AStrOffset: SG = 1): BG;
var i: SG;
begin
	Result := False;
	if Length(SubStr) > Length(Str) then
		Exit;
	for i := 1 to Length(SubStr) do
	begin
		if SubStr[i] <> Str[AStrOffset + i - 1] then
		begin
			Exit;
		end;
	end;
	Result := True;
end;

function EndStr(const SubStr: string; const Str: string): BG;
var i, j: SG;
begin
	Result := False;
	j := Length(Str);
	if Length(SubStr) > j then
		Exit;
	for i := Length(SubStr) downto 1 do
	begin
		if SubStr[i] <> Str[j] then
		begin
			Exit;
		end;
		Dec(j);
	end;
	Result := True;
end;

procedure RemoveSuffix(const SubStr: string; var Str: string);
begin
	if EndStr(SubStr, Str) then
		SetLength(Str, Length(Str) - Length(SubStr));
end;

function RemoveSuffixF(const SubStr: string; const Str: string): string;
begin
	Result := Str;
	RemoveSuffix(SubStr, Result);
end;

function IsSubStr(const SubStr: string; const Str: string): BG;
var
	i: Integer;
	Wor: string;
	LastWordBegin: Integer;
	StrLength: SG;
begin
	Result := False;
	i := 1;
	LastWordBegin := 0;
	StrLength := Length(Str);
	while True do
	begin
		if (i > StrLength) or CharInSet(Str[i], [' ', ',', '.', '-', '/', ';', '(', ')']) then
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
		if i > StrLength then Exit;
		Inc(i);
	end;
end;

function OneLine(const s: string): string;
const
  SplitStr = CharSpace + CharEnDash + CharSpace;
begin
	Result := s;
	Replace(Result, FullSep, SplitStr);
	Replace(Result, CharCR, SplitStr);
	Replace(Result, CharLF, SplitStr);
	DelBESpace(Result);
end;

procedure RemoveComment(var s: string);
var i: SG;
begin
	i := Pos(';', s);
	if i <> 0 then SetLength(s, i - 1);
end;

function CapitalCase(const s: string): string;
begin
	if Length(s) > 1 then
		Result := UpCase(s[1]) + LowerCase(Copy(s, 2, MaxInt));
end;

function ReplaceF(const s: string; const WhatS, ToS: string): string;
begin
	Result := s;
	Replace(Result, WhatS, ToS);
end;

function ReplaceF(const s: string; const WhatS, ToS: array of string): string;
begin
	Result := s;
	Replace(Result, WhatS, ToS);
end;

function ReplacePatternF(const s: string; const Pattern: array of TStringPair): string;
begin
	Result := s;
	ReplacePattern(Result, Pattern);
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

procedure Replace(var s: string; const WhatS, ToS: array of string);
var
	WhatSLen: SG;
	ActPos: array of SG;

	procedure Clear;
	begin
		FillUG(ActPos[0], WhatSLen, 1);
	end;

var
	Index, j: SG;
begin
	WhatSLen := Length(WhatS);
	if WhatSLen = 0 then Exit;
	Assert(WhatSLen = Length(ToS));

	SetLength(ActPos, WhatSLen);
	Clear;
	Index := 1;
	while Index <= Length(s) do
	begin
		for j := 0 to WhatSLen - 1 do
		begin
			if s[Index] = WhatS[j][ActPos[j]] then
			begin
				if ActPos[j] = Length(WhatS[j]) then
				begin // Found
{					if LongestCandidate then
					begin
						for k := 0 to WhatSLen - 1 do
						begin
							if (j <> k) and (ActPos[k] >= ActPos[j]) then
							begin
								for l := ActPos[k] to Length(WhatS[k])
								begin
									if ActPos[k]
								end;
							end;
						end;
					end;}
					Delete(s, Index - ActPos[j] + 1, ActPos[j]);
					Insert(ToS[j], s, Index - ActPos[j] + 1);
//					Index := ActPos[j] + Length(ToS[j]);
					Inc(Index, Length(ToS[j]) - ActPos[j]);
					Clear;
//					ActPos[j] := 1;
					Break;
				end
				else
					Inc(ActPos[j]);
			end
			else
      begin
        if ActPos[j] > 1 then
        begin
				ActPos[j] := 1;
          if s[Index] = WhatS[j][ActPos[j]] then
            Inc(ActPos[j]);
        end;
      end;
		end;
		Inc(Index);
	end;
end;

procedure ReplacePattern(var s: string; const Pattern: array of TStringPair);
var
	WhatS, ToS: array of string;
	PatternLength: SG;
	i: SG;
begin
	PatternLength := Length(Pattern);
	SetLength(WhatS, PatternLength);
	SetLength(ToS, PatternLength);
	for i := 0 to PatternLength - 1 do
	begin
		WhatS[i] := Pattern[i].Name;
		ToS[i] := Pattern[i].Value;
	end;
	Replace(s, WhatS, ToS);
end;

function ReplaceParam(const Text: string; const Param: array of string): string; overload;
var
  i: SG;
begin
  Result := Text;
  for i := 1 to Length(Param) do
  begin
    if IsDebug and (Pos('%' + IntToStr(i), Text) = 0) then
      Result := Result + LineSep + Param[i - 1]
    else
      Replace(Result, '%' + IntToStr(i), '''' + Param[i - 1] + '''');
  end;
end;

function DoubleBackSlash(const s: string): string;
var i: SG;
begin
	Result := '';
	for i := 1 to Length(s) do
	begin
		Result := Result + s[i];
		if s[i] = '\' then Result := Result + '\';
	end;
end;

function RemoveSingleAmp(const s: string): string;
var
	SourceLength: Integer;
	LastAmp: Boolean;
	ResultLength: Integer;
	i: Integer;
begin
	SourceLength := Length(s);
	SetLength(Result, SourceLength);
	ResultLength := 0;
	LastAmp := False;
	for i := 1 to SourceLength do
	begin
		if s[i] = '&' then
		begin
			if not LastAmp then
			begin
				LastAmp := True;
				Continue;
			end;
		end;
		LastAmp := False;
		Inc(ResultLength);
		Result[ResultLength] := s[i];
	end;
	SetLength(Result, ResultLength);
end;

function AddSpace(const s: string): string;
var Index: SG;
begin
	Result := s;
  Replace(Result, '_', CharSpace);
	Index := 2;
	while Index <= Length(Result) do
	begin
		if CharInSet(Result[Index], ['A'..'Z']) then
			if CharInSet(Result[Index - 1], ['a'..'z']) then
				Insert(CharSpace, Result, Index)
			else if CharInSet(Result[Index - 1], ['A'..'Z']) then
	  		if CharInSet(CharAt(Result, Index + 1), ['a'..'z']) then
  				Insert(CharSpace, Result, Index);
		Inc(Index);
	end;
end;

procedure AppendStr(var Dest: TFileName; const Source: string); overload;
begin
	Dest := Dest + Source;
end;

procedure AppendStr(var Dest: string; const Source: string); overload;
begin
	Dest := Dest + Source;
end;

procedure AppendStrSeparator(var Dest: string; const Source: string; const ASeparator: string);
begin
  if Dest = '' then
    Dest := Source
  else
    Dest := Dest + ASeparator + Source;
end;

procedure AppendLine(var Dest: string; const Source: string);
begin
	Dest := Dest + Source + LineSep;
end;

procedure AddPrefix(var Dest: string; const Source: string);
begin
	Dest := Source + Dest;
end;

function Plural(const Number: SG): string;
begin
	if Abs(Number) <> 1 then
		Result := 's';
end;

procedure CorrectDir(var s: string);
var i: SG;
begin
	i := Length(s);
	if (i > 0) and (s[i] <> PathDelim) then
		s := s + PathDelim;
end;

function CorrectDirF(const s: string): string;
begin
	Result := s;
	CorrectDir(Result);
end;

function RandomString(const Size: SG; const FromChar: U1 = 1; const ToChar: U1 = 255): string;
var i: SG;
begin
	SetLength(Result, Size);
	for i := 1 to Size do
		Result[i] := Char(FromChar + Random(ToChar - FromChar + 1));
end;

function RandomText(const Size: SG): string;
const
  FirstASCIIChar = Ord(' '); // 32
var i: SG;
begin
	SetLength(Result, Size);
	for i := 1 to Size do
		Result[i] := Char(FirstASCIIChar + Random(128 - FirstASCIIChar));
end;

procedure EnumToStr(const TypeInfo: PTypeInfo; out AString: array of string; const EnumPrefixLength: SG = 2);
var
	i: SG;
	TypeData: PTypeData;
begin
	TypeData := GetTypeData(TypeInfo);
	for i := TypeData.MinValue to TypeData.MaxValue do
	begin
		AString[i] := AddSpace(Copy(GetEnumName(TypeInfo, i), 1 + EnumPrefixLength, MaxInt));
	end;
end;

procedure EnumToStrEx(const TypeInfo: PTypeInfo; out AString: array of string; const EnumPrefixLength: SG = 2);
var
	i: SG;
	TypeData: PTypeData;
begin
	TypeData := GetTypeData(TypeInfo);
	for i := TypeData.MinValue to TypeData.MaxValue do
	begin
		AString[i] := Copy(GetEnumName(TypeInfo, i), 1 + EnumPrefixLength, MaxInt);
	end;
end;

function ButtonNameToFileName(const Name: string): string;
var
	Index, i: SG;
	Found: BG;
const
	Names: array[0..4] of string = ('DBUTTON', 'BUTTON', 'COMBOBOX', 'EDIT', 'MEMO');
begin
	Result := Name;
	Found := False;
	for i := 0 to Length(Names) - 1 do
	begin
		Index := Pos(Names[i], UpperCase(Result));
		if Index = 1 then
		begin
			Delete(Result, Index, Length(Names[i]));
			Found := True;
			Break;
		end;
	end;

	if Found = False then
		Result := DelLastNumber(Result);
end;

function ComponentName(const Name: string): string;
var i: SG;
begin
	Result := Name;
	i := 1;
	while i <= Length(Result) do
	begin
		if not (CharType(Result[i], StdCharTable) in [ctLetter, ctNumber]) then
			Delete(Result, i, 1)
		else
			Inc(i);
	end;
	if Result = '' then
		Result := 'N'
	else
	begin
		if CharType(Result[1], StdCharTable) <> ctLetter then
			Result := 'N' + Result;
	end;
end;

{$IFOPT Q+}
	{$DEFINE Q_PLUS}
	{$OVERFLOWCHECKS OFF}
{$ENDIF}
// Java algorithm
function HashCode(const s: string): U4;
var
	c: Char;
begin
	Result := 0;
	for c in s do
	begin
		Result := 31 * Result + Ord(c);
	end;
end;
{$IFDEF Q_PLUS}
	{$OVERFLOWCHECKS ON}
	{$UNDEF Q_PLUS}
{$ENDIF}

function OddEven(const Index: SG): string;
begin
	if Index mod 2 = 0 then
		Result := 'even'
	else
		Result := 'odd';
end;

function IsNumber(const AText: string): Boolean;
var
	i: SG;
begin
	Result := False;
	if Length(AText) = 0 then Exit;
	for i := 1 to Length(AText) do
	begin
		if not ((AText[i] = '.') or (CharInSet(AText[i], ['0'..'9']))) then
			Exit
	end;
	Result := True;
end;

function Match(const Text: string; const Filter: string): BG;
begin
  if Filter = '' then
    Result := False
  else if Filter = '*' then
    Result := True
  else if (FirstChar(Filter) = '*') and (LastChar(Filter) = '*') then
  begin
    Result := Pos(DelFirstChar(DelLastChar(Filter)), Text) <> 0;
  end
  else if LastChar(Filter) = '*' then
  begin
    Result := Pos(DelLastChar(Filter), Text) = 1;
  end
  else
    Result := Filter = Text;
end;

function FileMatch(const FileName: string; const Filter: string): BG;
var
  FileNamePoint, FilterPoint: SG;
begin
  FilterPoint := PosEx('.', Filter, MaxInt, 1);
  if FilterPoint = 0 then
  begin
  	Result := Match(FileName, Filter);
    Exit;
  end;
  FileNamePoint := PosEx('.', FileName, MaxInt, 1);
  if FileNamePoint = 0 then
    FileNamePoint :=Length(FileName) + 1;
	Result :=
  	Match(Copy(FileName, 1, FileNamePoint - 1), Copy(Filter, 1, FilterPoint - 1)) and
  	Match(Copy(FileName, FileNamePoint + 1, MaxInt), Copy(Filter, FilterPoint + 1, MaxInt));

end;

procedure FillHexValue;
var
	c: AnsiChar;
begin
	for c := Low(c) to High(c) do
		case c of
		AnsiChar('0')..AnsiChar('9'): HexValue[c] := Ord(c) - Ord('0');
		AnsiChar('A')..AnsiChar('Z'): HexValue[c] := Ord(c) - Ord('A') + 10;
		AnsiChar('a')..AnsiChar('z'): HexValue[c] := Ord(c) - Ord('a') + 10;
		else
			HexValue[c] := 0;
		end;
end;

function SplitStringEx(const AText, ADelimiter: string): TArrayOfString;
var
  dx : integer;
  ns : string;
  txt : string;
  delta : integer;
begin
  delta := Length(ADelimiter);
  txt := AText + ADelimiter;
  while Length(txt) > 0 do
  begin
    dx := Pos(ADelimiter, txt);
    ns := Copy(txt,0,dx-1);
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := ns;
    txt := Copy(txt,dx+delta,MaxInt);
  end;
end;

function PadLeft(const AText: string; const ACharCount: SG; const AFillChar: Char = CharSpace): string;
var
  restLength: SG;
begin
  restLength := ACharCount - Length(AText);
  if (restLength >= 1) then
  begin
    Result := StringOfChar(AFillChar, restLength) + AText;
  end
  else
    Result  := AText;
end;

function PadCenter(const AText: string; const ACharCount: SG; const AFillChar: Char = CharSpace): string;
var
  restLength: SG;
begin
  restLength := ACharCount - Length(AText);
  if (restLength >= 1) then
  begin
    Result := StringOfChar(AFillChar, restLength div 2) + AText + StringOfChar(AFillChar, restLength - restLength div 2);
  end
  else
    Result  := AText;
end;

function PadRight(const AText: string; const ACharCount: SG; const AFillChar: Char = CharSpace): string;
var
  restLength: SG;
begin
  restLength := ACharCount - Length(AText);
  if (restLength >= 1) then
  begin
    Result := AText + StringOfChar(AFillChar, restLength);
  end
  else
    Result  := AText;
end;

function PropertiesToString(const Keys: array of string; const Values: array of string; const ASeparator: string = ';'): string;
var
  i: Integer;
begin
  Assert(Length(Keys) = Length(Values));
  Result := '';
  for i := 0 to Length(Keys) - 1 do
  begin
    Result := Result + Keys[i] + '=' + Values[i];
    if i < Length(Keys) - 1 then
      Result := Result + ASeparator;
  end;
end;

{$ifdef MSWINDOWS}
function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';
{$endif}

function CompareStringLogical(AValue1, AValue2: string): TCompareResult; overload;
begin
{$ifdef MSWINDOWS}
  Result := TCompareResult(StrCmpLogicalW(PWideChar(WideString(AValue1)), PWideChar(WideString(AValue2))));
{$else}
  if AValue1 = AValue2 then
    Result := crBothSame
  else if AValue1 < AValue2 then
    Result := crFirstLess
  else
    Result :=  crFirstGreater;
{$endif}
end;

function U1ToOctalString(AValue: U1): string;
var
  R: array[0..2] of U1;
begin
  R[0] := AValue and 7;
  AValue := AValue shr 3;
  R[1] := AValue and 7;
  AValue := AValue shr 3;
  R[2] := AValue;

  Result :=
    Char(Ord(R[2]) + Ord('0')) +
    Char(Ord(R[1]) + Ord('0')) +
    Char(Ord(R[0]) + Ord('0'));
end;

function CharToDigit(const AChar: Char): SG;
begin
  Result := Ord(AChar) - Ord('0');
end;

function ProgramInput(const AText: string): string;
begin
  Result := '<�: ' + AText;
end;

function ProgramOutput(const AText: string): string;
begin
  Result := '�>: ' + AText;
end;

function JoinFileNameAndParameters(const AFileName: TFileName; const AParameters: string): string;
begin
  Result := AddQuoteF(AFileName) + CharSpace + AParameters;
end;

initialization
{$IFNDEF NoInitialization}
	FillHexValue;
{$ENDIF NoInitialization}
end.
