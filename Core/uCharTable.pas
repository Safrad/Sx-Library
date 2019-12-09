unit uCharTable;

interface

type
	TCharType = (
		ctLetter, ctBlank, ctReturn, ctDollar, ctIllegal, ctNumber, ctNumberPrefix,
		ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose,
		ctPoint, ctComma, ctSemicolon);

	TCharTable = array[Byte] of TCharType;
var
	StdCharTable: TCharTable;

function CharType(const c: Char; const CharTable: TCharTable): TCharType;

implementation

uses uChar;

// Fill CharTable with standard values.
procedure FillStdCharTable(out CharTable: TCharTable);
var
	b: Byte;
  c: Char;
begin
	// Make Char Table
	for b := Low(b) to High(b) do
  begin
    c := Char(b);
		case c of
		CharSpace, CharHT: CharTable[b] := ctBlank;
		CharCR, CharLF: CharTable[b] := ctReturn;
		'a'..'z', 'A'..'Z', '_'{, #$80..#$ff}: CharTable[b] := ctLetter;
		'0'..'9': CharTable[b] := ctNumber;
		{'!',} '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharTable[b] := ctNumberPrefix;
		'+': CharTable[b] := ctPlus;
		'-': CharTable[b] := ctMinus;
		'^': CharTable[b] := ctExp;
		'*': CharTable[b] := ctMul;
		'/': CharTable[b] := ctDiv;
		'(': CharTable[b] := ctOpen;
		')': CharTable[b] := ctClose;
		'.': CharTable[b] := ctPoint;
		',': CharTable[b] := ctComma;
		';': CharTable[b] := ctSemicolon;
		else
			CharTable[b] := ctIllegal;
		end;
  end;
end;

function CharType(const c: Char; const CharTable: TCharTable): TCharType;
begin
	{$ifdef UNICODE}
	if Ord(c) <= $ff then
	{$endif}
		Result := CharTable[Byte(c)]
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
