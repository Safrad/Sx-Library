//* File:     Lib\uLang.pas
//* Created:  1999-11-01
//* Modified: 2007-05-12
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uLang;

interface

uses SysUtils;

//Alphabet
procedure ReadAlphabet(const FileName: TFileName);
function AlphaStrToWideStr(const Line: string): WideString;

// Dictionary
procedure ReadDictionary(const FileName: TFileName);
function Translate(Line: string): string; overload;
procedure TranslateFile(FileName: TFileName); overload;

implementation

uses
	Windows,
	uTypes, uStrings, uFiles, uSorts, uFind, uDParser, uMath, uCharset, uCharTable;

// Alphabet
var
	Alpha: array of string;
	AlphaCount: Integer;

procedure ReadAlphabet(const FileName: TFileName);
var
	F: TFile;
	s: string;
	NewSize: SG;
begin
	F := TFile.Create;
	try
		AlphaCount := 0; SetLength(Alpha, 0);
		if F.Open(FileName, fmReadOnly) then
		begin
			while not F.Eof do
			begin
				F.Readln(s);
				if Length(s) > 0 then
				begin
					NewSize := AlphaCount + 1;
					if AllocByExp(Length(Alpha), NewSize) then
						SetLength(Alpha, NewSize);
					Alpha[AlphaCount] := s;
					Inc(AlphaCount);
				end;
			end;
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

function AlphaCharToWord(const Line: string; var InLineIndex: Integer): U2;
var
	i: Integer;
	Found: Integer;
begin
	Result := Ord(Line[InLineIndex]);
	Found := 0;
	for i := 0 to AlphaCount - 1 do
	begin
		if Copy(Line, InLineIndex, Length(Alpha[i])) = Alpha[i] then
		begin
			if Length(Alpha[i]) > Found then
			begin
				Result := 65 + i;
				Found := Length(Alpha[i]);
			end;
		end;
	end;
	if Found > 0 then
	begin
		Inc(InLineIndex, Found);
	end
	else
	begin
		if Ord(Line[InLineIndex]) >= 123 then
			Result := Ord(Line[InLineIndex]) + 256
		else
			Result := Ord(Line[InLineIndex]);
		Inc(InLineIndex);
	end;
end;

function AlphaStrToWideStr(const Line: string): WideString;
var i, j: Integer;
begin
	SetLength(Result, Length(Line));
	i := 1;
	while i <= Length(Line) do
	begin
		j := i;
		Result[j] := WideChar(AlphaCharToWord(Line, i));
	end;
end;

// Dictionary
type
	TDict = packed record
		Cz, En: string;
	end;
var
	Dict: array of TDict;
	DictCount: SG;
	AIndex: array of SG;
	AValue: array of U4;

procedure ReadDictionary(const FileName: TFileName);
var
	Line: string;
	InLineIndex: SG;
	i: SG;
begin
	Line := ReadStringFromFile(FileName);
	InLineIndex := 1;
	DictCount := 0;
	while InLineIndex < Length(Line) do
	begin
		Inc(DictCount);
		SetLength(Dict, DictCount);

		Dict[DictCount - 1].Cz := ReadToChar(Line, InLineIndex, CharTab);
		Dict[DictCount - 1].En := ReadToChar(Line, InLineIndex, CharCR);
		ReadToChar(Line, InLineIndex, CharLF);
	end;
	// Read dictionary
	SetLength(AIndex, DictCount);
	SetLength(AValue, DictCount);
	for i := 0 to DictCount - 1 do
	begin
		AIndex[i] := i;
		AValue[i] := Length(Dict[i].Cz);
	end;

	SortU4(False, True, PArraySG(@AIndex[0]), PArrayU4(@AValue[0]), DictCount);
end;

function Translate(Line: string): string; overload;
var
	i, j, Index, Po: SG;
	WhatS, WhatS2, ToS: string;
begin
	for j := 0 to DictCount - 1 do
	begin
		i := AIndex[j];
		Index := 1;
		WhatS := UpCaseCz(Dict[i].Cz);
		while Index < Length(Line) do
		begin
			Po := PosEx(WhatS, UpCaseCz(Line), Index);

			if (Po <> 0) then
			begin
				if (StdCharTable[Line[Po - 1]] <> ctLetter)
				and (StdCharTable[Line[Po + Length(WhatS)]] <> ctLetter)
				and (Line[Po - 1] <> '<')
				and (Line[Po - 1] <> '/')
				and (Ord(Line[Po - 1]) < 128)
				and (Ord(Line[Po + Length(WhatS)]) < 128) then
				begin
					if Line[Po] = Dict[i].Cz[1] then
						ToS := Dict[i].En
					else
					begin
						ToS := Dict[i].En;
						ToS[1] := UpCase(ToS[1]);
					end;

					WhatS2 := Dict[i].Cz;
					Delete(Line, Po, Length(WhatS2));
					Insert(ToS, Line, Po);
					Index := Po + Length(ToS);
				end
				else
					Index := Po + Length(WhatS);
			end
			else
				Break;
		end;

//		Replace(Line, Dict[i].Cz, Dict[i].En);
	end;
//	ConvertCharset(Line, cp1250, cpISO88592);
	Result := Line;
end;

procedure TranslateFile(FileName: TFileName); overload;
var
	FileNameEn: TFileName;
	Line: string;
begin
	Line := Translate(ReadStringFromFile(FileName));
	FileNameEn := Translate(ExtractFileName(FileName));
	if FileNameEn <> FileName then
		FileName := AddAfterName(FileName, 'En')
	else
		FileName := AddAfterName(FileName, 'En');
	WriteStringToFile(FileName, Line, False);
end;

procedure Finalize;
begin
	SetLength(Alpha, 0);
	SetLength(AIndex, 0);
	SetLength(AValue, 0);
	SetLength(Dict, 0);
end;

initialization

finalization
	Finalize;
end.
