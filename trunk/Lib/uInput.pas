//* File:     Lib\uInput.pas
//* Created:  2004-03-07
//* Modified: 2005-11-29
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uInput;

interface

uses
	uTypes, uVector;

// Str To Data
function StrToMs(Line: AnsiString; const MinVal, DefVal, MaxVal: UG): UG;

function StrToVector(Line: AnsiString; const UseWinFormat: BG): TVector;
function StrToValE(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended;
{function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended; overload;}

function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG; overload;
function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG; overload;

{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG; overload;}
{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; out ErrorMsg: string): UG; overload;}

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8): S8;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1): U1;

function SToDate(const Str: string): TDateTime;
function SToDateTime(const Str: string): TDateTime;

implementation

uses
	SysUtils,
	uParser, uFormat, uStrings, uMsg;

function StrToMs(Line: AnsiString; const MinVal, DefVal, MaxVal: UG): UG;
var Parser: TDParser;
begin
	Parser := TDParser.Create(Pointer(Line), Length(Line));
	Result := Parser.ReadMs(MinVal, DefVal, MaxVal);
	if Parser.InputType <> itEOI then Parser.AddMes(mtEUnusedChars, []);
	Parser.Free;
end;

function StrToVector(Line: AnsiString; const UseWinFormat: BG): TVector;
var Parser: TDParser;
begin
	Parser := TDParser.Create(Line);
	if UseWinFormat then
	begin
		Parser.DecimalSep := DecimalSeparator;
		Parser.ThousandSep := ThousandSeparator;
	end
	else
	begin
		Parser.DecimalSep := '.';
		Parser.ThousandSep := ',';
	end;
	Parser.ReadInput;
	FreeTree(Root);
	Root := Parser.NodeE(nil);
	if Root <> nil then
	begin
		Result := Calc(Root);
	end;
	Parser.Free;
end;

function StrToValE(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended;
var Parser: TDParser;
begin
	Parser := TDParser.Create(Line);
	if UseWinFormat then
	begin
		Parser.DecimalSep := DecimalSeparator;
		Parser.ThousandSep := ThousandSeparator;
	end
	else
	begin
		Parser.DecimalSep := '.';
		Parser.ThousandSep := ',';
	end;
//	CompileMesClear;
	Result := Parser.ReadFA(MinVal, DefVal, MaxVal);
	Parser.Free;
end;
{
function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended;
var
	InStr: string;
	LineIndex: SG;
begin
	Result := StrToValExt(Line, UseWinFormat, MinVal, DefVal, MaxVal, ErrorMsg, InStr, LineIndex);
end;}

{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator);
end;

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator);
end;}

function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator));
end;

function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator));
end;

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8): S8;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator));
end;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1): U1;
begin
	Result := StrToValI(Line, UseWinFormat, 0, UG(DefVal), 255, 1);
end;

function SToDate(const Str: string): TDateTime;
var
	DateSep: Char;
	Year, Month, Day: U2;
	InLineIndex: SG;
begin
	if Str = '' then
	begin
		Result := 0;
		Exit;
	end;

	InLineIndex := 1;
	if Pos('/', Str) <> 0 then
	begin
		DateSep := '/';
		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
	end
	else if Pos('-', Str) <> 0 then
	begin
		DateSep := '-';
		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
	end
	else if Pos('.', Str) <> 0 then
	begin
		DateSep := '.';
		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
	end
	else if Length(Str) = 6 then
	begin
		Year := StrToValI(Copy(Str, 1, 2), False, 00, UG(00), 99, 1);
		if Year < ICentury then Inc(Year, 2000) else Inc(Year, 1900);
		Month := StrToValI(Copy(Str, 3, 2), False, 1, UG(1), 99, 1);
		Day := StrToValI(Copy(Str, 5, 2), False, 1, UG(1), 31, 1);
	end
	else if Length(Str) = 8 then
	begin
		Year := StrToValI(Copy(Str, 1, 4), False, 1900, UG(1900), 9999, 1);
		Month := StrToValI(Copy(Str, 5, 2), False, 1, UG(1), 12, 1);
		Day := StrToValI(Copy(Str, 7, 2), False, 1, UG(1), 31, 1);

	end
	else
//	if (Pos(',', Str) <> 0) or (Str[1] = '3') then
	begin
		Result := StrToValI(Str, False, 0, 0, MaxInt, 1);
		Exit;
	end;
	if Month > 50 then Dec(Month, 50); // Female offset
	if TryEncodeDate(Year, Month, Day, TDateTime(Result)) = False then
	begin
		ErrorMsg('Invalid date' + LineSep + Str + '.');
		Result := 0;
	end;
end;

function SToDateTime(const Str: string): TDateTime;
var InLineIndex: SG;
begin
	InLineIndex := 1;
	Result := SToDate(ReadToChar(Str, InLineIndex, ' ')) +
		SToTime(ReadToChar(Str, InLineIndex, CharCR));
end;

end.
