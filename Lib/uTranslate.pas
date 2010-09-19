unit uTranslate;

interface

type
	TLanguage = (laEnglish, laCzech);
var
	Language: TLanguage = laEnglish; // {$ifopt d+}laCzech{$else}laEnglish{$endif};

function Translate(const s: string): string;

implementation

uses
	SysUtils,
	uTypes, uStrings, uFiles, uMath;

var
	LoadedDictionary: BG = False;
type
	TWord = array[TLanguage] of string;
var
	Words: array of TWord;
	WordCount: SG;

procedure ReadLine(const Line: string);
var InLineIndex: SG;
begin
	InLineIndex := 1;
	Words[WordCount][laEnglish] := ReadToChar(Line, InLineIndex, CharTab);
	Words[WordCount][laCzech] := ReadToChar(Line, InLineIndex, CharTab);
end;

procedure ReadFile;
var
	InLineIndex: SG;
	s: string;
	NewSize: SG;
begin
	LoadedDictionary := True;

	s := ReadStringFromFile(DataDir + 'Dictionary.txt');

	InLineIndex := 1;
	while InLineIndex <= Length(s) do
	begin
		NewSize := WordCount + 1;
		if AllocByExp(Length(Words), NewSize) then
			SetLength(Words, NewSize);

		ReadLine(ReadToNewLine(s, InLineIndex));
		Inc(WordCount);
	end;
end;

function FindExact(const InWord: string; var InLineIndex: SG; out OutWord: string): BG;
var
	i: SG;
	lc: string;
begin
	Result := False;
	OutWord := InWord;

	lc := InWord;

	for i := 0 to WordCount - 1 do
	begin
		if Copy(lc, 1, Length(Words[i][laEnglish])) = Words[i][laEnglish] then
		begin
			OutWord := Words[i][laCzech];
			Inc(InLineIndex, Length(Words[i][laEnglish]));
			Result := True;
		end;
	end;

	if AnsiLowerCase(InWord[1]) <> InWord[1] then
		OutWord[1] := AnsiUpperCase(OutWord[1])[1];
end;

function Translate(const s: string): string;
var
	InLineIndex: SG;
	OutWord: string;
begin
	case Language of
	laEnglish: Result := s;
	laCzech:
	begin
		InLineIndex := 1;
		while InLineIndex <= Length(s) do
		begin
//			ReadToChars(s, InLineIndex, BlankChars);

			if FindExact(s, InLineIndex, OutWord) then ;
			Result := Result + OutWord;
		end;

	end;
	end;
end;

end.
