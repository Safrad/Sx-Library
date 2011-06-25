//* File:     Lib\uDictionary.pas
//* Created:  2007-05-12
//* Modified: 2007-10-21
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDictionary;

interface

uses SysUtils, Menus;

type
	TLanguage = (laEnglish, laCzech);
var
	Language: TLanguage;

procedure ReadDictionary(const FileName: TFileName);
function Translate(Line: string): string; overload;
procedure TranslateTexts(var s: array of string);
procedure TranslateMenu(const Src: TMenuItem);
procedure TranslateFile(FileName: TFileName); overload;

implementation

uses uTypes, uFiles, uStrings, uSorts, uCharset, uCharTable, uCSVFile, uMath;

type
	TDict = packed record
		Cz, En: string;
	end;
var
	Dict: array of TDict;
	DictCount: SG;
	Loaded: BG;
	AIndex: array of SG;
	AValue: array of U4;

procedure ReadDictionary(const FileName: TFileName);
var
	CSVFile: TCSVFile;
	Row: TArrayOfString;
	NewSize: SG;
	i: SG;
begin
	if Language = laEnglish then Exit;
	Loaded := True;
	DictCount := 0;
	Row := nil;
	CSVFile := TCSVFile.Create(2);
	try
		if CSVFile.Open(FileName) then
		begin
			while not CSVFile.EOF do
			begin
				Row := CSVFile.ReadLine;
{				if (CSV.LineIndex < Length(MonthColors)) then
					MonthColors[CSV.LineIndex - 1] := StringToColor(Row[0]);}
				NewSize := DictCount + 1;
				if AllocByExp(Length(Dict), NewSize) then
					SetLength(Dict, NewSize);
				Dict[DictCount].Cz := Row[0];
				Dict[DictCount].En := Row[1];
				Inc(DictCount);
			end;
			CSVFile.Close;
		end;
	finally
		CSVFile.Free;
	end;

(*Line := ReadStringFromFile(FileName);
	InLineIndex := 1;}
	while InLineIndex < Length(Line) do
	begin
		SetLength(Dict, DictCount);

		Dict[DictCount - 1].Cz := ReadToChar(Line, InLineIndex, CharTab);
		Dict[DictCount - 1].En := ReadToChar(Line, InLineIndex, CharCR);
		ReadToChar(Line, InLineIndex, CharLF);
	end; *)
	// Sort dictionary.
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
	if Language <> laEnglish then
	begin
		if Loaded = False then
			ReadDictionary(WorkDir + 'Language' + PathDelim + 'Dictionary.csv');
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
					if (StdCharTable[CharAt(Line, Po - 1)] <> ctLetter)
					and (StdCharTable[CharAt(Line, Po + Length(WhatS))] <> ctLetter)
					and (CharAt(Line, Po - 1) <> '<')
					and (CharAt(Line, Po - 1) <> '/')
					and (Ord(CharAt(Line, Po - 1)) < 128)
					and (Ord(CharAt(Line, Po + Length(WhatS))) < 128) then
					begin
						if Line[Po] = Dict[i].Cz[1] then
							ToS := Dict[i].En
						else
						begin
							ToS := Dict[i].En;
							ToS[1] := UpCaseCz(ToS[1])[1];
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
	end;
//	ConvertCharset(Line, cp1250, cpISO88592);
	Result := Line;
end;

procedure TranslateTexts(var s: array of string);
var
	i: SG;
begin
	for i := 0 to Length(s) - 1 do
	begin
		s[i] := Translate(s[i]);
	end;
end;

procedure TranslateMenu(const Src: TMenuItem);
var
	i: SG;
begin
	for i := 0 to Src.Count - 1 do
	begin
		Src[i].Caption := Translate(Src[i].Caption);
		if Src[i].Count > 0 then
		begin
			TranslateMenu(Src[i]);
		end;
	end;
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

{procedure LoadWords(const TypeInfo: TTypeInfo; const array of string;
begin

//	sRound := Translate(
end;}

procedure _Finalize;
begin
	SetLength(AIndex, 0);
	SetLength(AValue, 0);
	SetLength(Dict, 0);
end;

initialization

finalization
	_Finalize;
end.
