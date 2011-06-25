//* File:     Lib\uCharTable.pas
//* Created:  2000-05-01
//* Modified: 2007-05-13
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uCharTable;

interface

type
	TCharTable = array[Char] of (
		{ctSpace, ctTab,} ctLetter, ctLastLetter, ctBlank, ctReturn, ctDollar, ctIllegal, ctNumber, ctNumber2,
		ctPlus, ctMinus, ctExp, ctMul, ctDiv, ctOpen, ctClose,
		ctPoint, ctComma, ctComma2);
var
	StdCharTable: TCharTable;

implementation

uses uStrings;

// Fill CharTable with standard values.
procedure FillStdCharTable(out CharTable: TCharTable);
var
	c: Char;
begin
	// Make Char Table
	for c := Low(Char) to High(Char) do
		case c of
		CharSpace, CharTab: CharTable[c] := ctBlank;
		CharCR, CharLF: CharTable[c] := ctReturn;
		'a'..'z', 'A'..'Z', '_'{, #$80..#$ff}: CharTable[c] := ctLetter;
		'0'..'9': CharTable[c] := ctNumber;
		{'!',} '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharTable[c] := ctNumber2;
		'+': CharTable[c] := ctPlus;
		'-': CharTable[c] := ctMinus;
		'^': CharTable[c] := ctExp;
		'*': CharTable[c] := ctMul;
		'/': CharTable[c] := ctDiv;
		'(': CharTable[c] := ctOpen;
		')': CharTable[c] := ctClose;
		'.': CharTable[c] := ctPoint;
		',': CharTable[c] := ctComma;
		';': CharTable[c] := ctComma2;
		else
			CharTable[c] := ctIllegal;
		end;
end;

(*
procedure FillCharsTable;
var c: Char;
begin
	// Make Char Table
	for c := Low(Char) to High(Char) do
		case c of
		' ': CharsTable[c] := ctSpace;
		'a'..'z', 'A'..'Z', '_': CharsTable[c] := ctLetter;
		'0'..'9', '!', '#', '$', '%' {'a'..'z', 'A'..'Z'}: CharsTable[c] := ctNumber;
		'+': CharsTable[c] := ctPlus;
		'-': CharsTable[c] := ctMinus;
		'^': CharsTable[c] := ctExp;
		'*': CharsTable[c] := ctMul;
		'/': CharsTable[c] := ctDiv;
		'(': CharsTable[c] := ctOpen;
		')': CharsTable[c] := ctClose;
		'.', ',': CharsTable[c] := ctNumber;
		else
			if (c = DecimalSeparator[1]) or (c = ThousandSeparator[1]) then
				CharsTable[c] := ctNumber
			else
				CharsTable[c] := ctIllegal;
		end;
end;*)

initialization
	FillStdCharTable(StdCharTable);
end.
