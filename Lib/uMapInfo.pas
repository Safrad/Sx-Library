//* File:     Lib\uMapInfo.pas
//* Created:  1998-01-01
//* Modified: 2008-04-05
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uMapInfo;

interface

uses uTypes;

type
	TFlo = Single;
	TPoint = record
		X, Y: TFlo;
	end;

	TFieldM = record
		Point: TPoint;
		Kod: S4; { Decimal(7, 0)}
		Nazev: string[30];
		PSC: string[10];
		Typ: S4;{Decimal(3, 0)}
		Prior: S4; {Decimal(5, 0)}
		GNazev: string[30];
		Vojvod: string[5];
		Reserved: array[0..23] of U1;
	end;

var
	FieldsM: array of TFieldM;
	FieldMCount: Integer;

	CharKind: array[Char] of (ckOthers, ckNum, ckDecimalSeparator, ckDelimeter);

procedure ReadMidMif(const MiName: string);
procedure WriteMid(const MiName: string);

implementation

uses
	SysUtils, Windows,
	uFile, uFiles, uStrings, uMath;

const
	Delimiter = ',';

	function StrToFlo(s: string): TFlo;
	var ErrorPos: Integer;
	begin
		Val(s, Result, ErrorPos);
		if ErrorPos <> 0 then
			Result := 0;
	end;

	function GetNextFloat(Line: string; var InLineIndex: Integer): TFlo;
	begin
		Result := StrToFlo(ReadToChar(Line, InLineIndex, ' '));
	end;

procedure ReadMidMif(const MiName: string);
var
	FIn: TFile;
	DBFileName: TFileName;
	Line: string;
	InLineIndex: Integer;
	NewSize: Integer;
	FileIndex: Integer;
	WhereMif: (wmNone, wmType, wmData);
	Po: Integer;
	FieldMIndex: Integer;
begin
	FileIndex := 0;
	FieldMCount := 0; SetLength(FieldsM, 0);
	while FileIndex <= 1 do
	begin
		if FileIndex = 0 then
			DBFileName := DataDir + MiName + '.mif'
		else
			DBFileName := DataDir + MiName + '.mid';
		FIn := TFile.Create;
		try
			if FIn.Open(DbFileName, fmReadOnly) then
			begin
				FieldMIndex := 0;
				WhereMif := wmNone;
				while not FIn.Eof do
				begin
					FIn.Readln(Line);
					RemoveComment(Line);
					if Line = '' then Continue;
					InLineIndex := 1;
					if FileIndex and 1 = 0 then
					begin // MIF
						case WhereMif of
						wmNone:
						begin
							Po := Pos('Columns', Line);
							if Po = 1 then
							begin
								WhereMif := wmType;
	{							InLineIndex := Po + 7;
								FormatCount := GetNextInt(Line, InLineIndex);
								SetLength(Formats, FormatCount);
								FormatIndex := 0;}
							end;
						end;
						wmType:
						begin
							if Pos('Data', Line) = 1 then
							begin
								WhereMif := wmData;
							end;
						end;
						wmData:
						begin
							Po := Pos('Point', Line);
							if Po <> 0 then
							begin
								NewSize := FieldMCount + 1;
								if AllocByExp(Length(FieldsM), NewSize) then
									SetLength(FieldsM, NewSize);
								FieldsM[FieldMCount].Point.X := GetNextFloat(Line, InLineIndex);
								FieldsM[FieldMCount].Point.Y := GetNextFloat(Line, InLineIndex);

								Inc(FieldMCount);
							end;
						end;
						end;
					end
					else
					begin
						FieldsM[FieldMIndex].Kod := StrToInt(ReadToChar(Line, InLineIndex, Delimiter));

						FieldsM[FieldMIndex].Nazev := DelQuoteF(ReadToChar(Line, InLineIndex, Delimiter));

						FieldsM[FieldMIndex].PSC := DelCharsF(DelCharsF(DelQuoteF(ReadToChar(Line, InLineIndex, Delimiter)), '-'), ' ');
						if (Length(FieldsM[FieldMIndex].PSC) <> 0) then
							SetLength(FieldsM[FieldMIndex].PSC, 5);
						FieldsM[FieldMIndex].Typ := StrToInt(ReadToChar(Line, InLineIndex, Delimiter));
						FieldsM[FieldMIndex].Prior := StrToInt(ReadToChar(Line, InLineIndex, Delimiter));

						FieldsM[FieldMIndex].GNazev := DelQuoteF(ReadToChar(Line, InLineIndex, Delimiter));
						FieldsM[FieldMIndex].Vojvod := DelQuoteF(ReadToChar(Line, InLineIndex, Delimiter));


	(*					s := ReadToChar(Line, InLineIndex, Delimiter);
						DelQuote(s);
						FieldsM[FieldMIndex].NazevCo := s;

						s := ReadToChar(Line, InLineIndex, Delimiter);
						DelQuote(s);
						FieldsM[FieldMIndex].NazevObc := s;
						ReadToChar(Line, InLineIndex, Delimiter);
						ReadToChar(Line, InLineIndex, Delimiter);
						ReadToChar(Line, InLineIndex, Delimiter);

						s := ReadToChar(Line, InLineIndex, Delimiter);
						DelQuote(s);
						FieldsM[FieldMIndex].PSC99 := s;*)

	//				FieldsM[FieldMIndex].KodObc := StrToInt(ReadToChar(Line, InLineIndex, Delimiter));}
						Inc(FieldMIndex);
	//				Inc(LineIndex);
					end;
				end;
				FIn.Close;
			end;
		finally
			FIn.Free;
		end;
		Inc(FileIndex);
	end;
end;

procedure WriteMid(const MiName: string);
var
	FIn: TFile;
	FieldMIndex: Integer;
begin
	FIn := TFile.Create;
	try
		if FIn.Open(DataDir + MiName + '.mid', fmRewrite) then
		begin
			for FieldMIndex := 0 to FieldMCount - 1 do
			begin
				FIn.Write(
					IntToStr(FieldsM[FieldMIndex].Kod) + ',' +
					'"' + FieldsM[FieldMIndex].Nazev + '",' +
					'"' + FieldsM[FieldMIndex].PSC + '",' +
					IntToStr(FieldsM[FieldMIndex].Typ) + ',' +
					IntToStr(FieldsM[FieldMIndex].Prior) + ',' +
					'"' + FieldsM[FieldMIndex].GNazev + '",' +
					'"' + FieldsM[FieldMIndex].Vojvod + '"' + FileSep);
			end;
			FIn.Truncate;
			FIn.Close;
		end;
	finally
		FIn.Free;
	end;
end;

procedure FillData;
var c: Char;
begin
	for c := Low(c) to High(c) do
		case c of
		'0'..'9': CharKind[c] := ckNum;
		'.': CharKind[c] := ckDecimalSeparator;
		Delimiter: CharKind[c] := ckDelimeter;
		else CharKind[c] := ckOthers;
		end;
end;

initialization
	FillData;
end.
