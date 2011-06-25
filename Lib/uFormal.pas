//* File:     Lib\uFormal.pas
//* Created:  1999-06-01
//* Modified: 2008-02-23
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uFormal;

interface

uses
	uTypes, uParserMsg;

// Replaces spaces by tab in the begin of line.
function RepairText(var Text: string; const ParserMessages: TParserMessages): BG;

implementation

uses
	SysUtils,
	uLog,
	uStrings;

function RepairText(var Text: string; const ParserMessages: TParserMessages): BG;
const
	TabSize = 2;
var
	Index,
	ColumnIndex, LineIndex: SG;
//	LastLineLevel, LineLevel: SG;

{	procedure ReadInput;
	begin


	end;}

	procedure AddMes(const MsgText: string);
	begin
		if Assigned(ParserMessages) then
			ParserMessages.Add(0, LineIndex, ColumnIndex, ColumnIndex + 1{TODO}, MsgText, mlInformation);
	end;


var
	NumSp: SG;
begin
	Result := True;

{
	_______T E X T
	______^ ^     ^
	Index 1 2     5
}

//	Text := Text + FullSep;

	// File begin
	Index := 1;
//	LastLineLevel := 0;

	LineIndex := -1;
	while Index <= Length(Text) do
	begin
		// Line begin
		ColumnIndex := 1;
		Inc(LineIndex);

		// Ident
//		LineLevel := 0;
		while (Index <= Length(Text)) and (Text[Index] = CharTab) do
		begin
			Inc(Index);
			Inc(ColumnIndex, TabSize);
//			Inc(LineLevel);
		end;

		NumSp := 0;
		while (Index <= Length(Text)) and (Text[Index] = ' ') do
		begin
			Inc(Index);
			Inc(ColumnIndex);
			Inc(NumSp);
			if NumSp = TabSize then
			begin
				Dec(Index, TabSize);
				Delete(Text, Index, TabSize);
				Insert(CharTab, Text, Index);
				Inc(Index, 1);
//				Inc(LineLevel);
				NumSp := 0;
				AddMes('Replacing spaces by tab.');
			end;
		end;
		if NumSp <> 0 then
			AddMes('Invalid ident.');

//		if Abs(LineLevel - LastLineLevel) > 1 then
//		if LineLevel > LastLineLevel + 1 then
//			AddMes('Invalid ident.');
//		LastLineLevel := LineLevel;

		// Line continue
		while Index <= Length(Text) do
		begin
			case Text[Index] of
			CharTab:
			begin
//				AddMes('Char "Tab" not alowed here.');
				Inc(Index);
				Inc(ColumnIndex, TabSize);
				Continue;
			end;
			CharCR, CharLF:
			begin
				Inc(Index);
				if (Index <= Length(Text)) and (Text[Index] = CharLF) then
					Inc(Index); // Skip
				Break;
			end;
			else
			begin
				Inc(Index);
				Inc(ColumnIndex);
			end;
			end;
		end;
	end;
end;

end.

