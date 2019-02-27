unit uCharTable;

interface

type
	TCharType = (
		ctLetter, ctBlank, ctReturn, ctDollar, ctIllegal, ctNumber, ctNumberPrefix,
		ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose,
		ctPoint, ctComma, ctSemicolon);

	TCharTable = array[AnsiChar] of TCharType;
var
	StdCharTable: TCharTable;

function CharType(const c: Char; const CharTable: TCharTable): TCharType;

implementation

uses uChar;

// Fill CharTable with standard values.
procedure FillStdCharTable(out CharTable: TCharTable);
var
	c: AnsiChar;
begin
	// Make Char Table
	for c := Low(c) to High(c) do
		case c of
		CharSpace, CharHT: CharTable[c] := ctBlank;
		CharCR, CharLF: CharTable[c] := ctReturn;
		'a'..'z', 'A'..'Z', '_'{, #$80..#$ff}: CharTable[c] := ctLetter;
		'0'..'9': CharTable[c] := ctNumber;
		{'!',} '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharTable[c] := ctNumberPrefix;
		'+': CharTable[c] := ctPlus;
		'-': CharTable[c] := ctMinus;
		'^': CharTable[c] := ctExp;
		'*': CharTable[c] := ctMul;
		'/': CharTable[c] := ctDiv;
		'(': CharTable[c] := ctOpen;
		')': CharTable[c] := ctClose;
		'.': CharTable[c] := ctPoint;
		',': CharTable[c] := ctComma;
		';': CharTable[c] := ctSemicolon;
		else
			CharTable[c] := ctIllegal;
		end;
end;

function CharType(const c: Char; const CharTable: TCharTable): TCharType;
begin
	{$ifdef UNICODE}
	if Ord(c) <= $ff then
	{$endif}
		Result := CharTable[AnsiChar(c)]
	{$ifdef UNICODE}
	else
		Result := ctLetter; // i. e. Greece alphabet
	{$endif}
end;

initialization
{$IFNDEF NoInitialization}
	FillStdCharTable(StdCharTable);
{$ENDIF NoInitialization}
end.
