//* File:     Lib\uInputFormat.pas
//* Created:  2004-03-07
//* Modified: 2007-11-25
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uInputFormat;

interface

uses
	uTypes, uVector, uParserMsg;

type
	TInputFormat = (ifIO{Disk File Input/Output}, ifDisplay{Windows Locale});

function StrToF4(const Str: string; const InputFormat: TInputFormat): F4;
function StrToF8(const Str: string; const InputFormat: TInputFormat): F8;
function StrToFA(const Str: string; const InputFormat: TInputFormat): FA;
function StrToSG(const Str: string; const InputFormat: TInputFormat): SG;
function StrToBG(const Str: string; const InputFormat: TInputFormat): BG;
function SToTime(const Str: string; const InputFormat: TInputFormat): TDateTime;
function SToMs(const Str: string; const InputFormat: TInputFormat): SG; // MsToStr<-

// Str To Data
function StrToMs(Line: AnsiString; const MinVal, DefVal, MaxVal: UG; const Messages: TParserMessages = nil): UG;

function StrToVector(Line: AnsiString; const UseWinFormat: BG; const Messages: TParserMessages = nil): TVector;
function StrToValE(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; const Messages: TParserMessages = nil): Extended;
{function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended; out ErrorMsg: string): Extended; overload;}

function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG; const Messages: TParserMessages = nil): SG; overload;
function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; const Messages: TParserMessages = nil): UG; overload;

{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG): SG; overload;}
{function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; out ErrorMsg: string): UG; overload;}

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8; const Messages: TParserMessages = nil): S8;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1; const Messages: TParserMessages = nil): U1;

function SToDate(const Str: string; const InputFormat: TInputFormat): TDateTime;
function SToDateTime(const Str: string; const InputFormat: TInputFormat): TDateTime;

implementation

uses
	SysUtils,
	uDParser, uStrings, uMsg;

function StrToMs(Line: AnsiString; const MinVal, DefVal, MaxVal: UG; const Messages: TParserMessages = nil): UG;
var Parser: TDParser;
begin
	Parser := TDParser.Create(Pointer(Line), Length(Line));
	Parser.Messages := Messages;
	Result := Parser.ReadMs(MinVal, DefVal, MaxVal);
	if Parser.InputType <> itEOI then Parser.AddMes(mtEUnusedChars, []);
	Parser.Free;
end;

function StrToVector(Line: AnsiString; const UseWinFormat: BG; const Messages: TParserMessages = nil): TVector;
var Parser: TDParser;
begin
	Parser := TDParser.Create(Line);
	Parser.Messages := Messages;
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
	const MinVal, DefVal, MaxVal: Extended; const Messages: TParserMessages = nil): Extended;
var
	Parser: TDParser;
	s: string;
begin
	Parser := TDParser.Create(Line);
	try
		if Messages = nil then
			Parser.Messages := TParserMessages.Create
		else
			Parser.Messages := Messages;
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
		Result := Parser.ReadFA(MinVal, DefVal, MaxVal);
		if Messages = nil then
		begin
			if Parser.Messages.Messages.Count > 0 then
			begin
				s := Parser.Messages.ToString;
				FreeAndNil(Parser.Messages);
				raise EConvertError.Create(s);
			end
			else
			begin
				FreeAndNil(Parser.Messages);
			end;
		end
	finally
		Parser.Free;
	end;
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
	const MinVal, DefVal, MaxVal, Denominator: SG; const Messages: TParserMessages = nil): SG;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; const Messages: TParserMessages = nil): UG;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8; const Messages: TParserMessages = nil): S8;
begin
	Result := Round(Denominator * StrToValE(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1; const Messages: TParserMessages = nil): U1;
begin
	Result := StrToValI(Line, UseWinFormat, 0, UG(DefVal), 255, 1, Messages);
end;

{$ifdef VER130} // Delphi 5
function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
	I: Integer;
	DayTable: PDayTable;
begin
	Result := False;
	DayTable := @MonthDays[IsLeapYear(Year)];
	if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
		(Day >= 1) and (Day <= DayTable^[Month]) then
	begin
		for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
		I := Year - 1;
		Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
		Result := True;
	end;
end;
{$endif}

function SToDate(const Str: string; const InputFormat: TInputFormat): TDateTime;
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

	case InputFormat of
	ifDisplay:
	begin
		Result := StrToDateTime(Str);
	end
	else
	begin
		InLineIndex := 1;
		if Pos('/', Str) <> 0 then
		begin
			DateSep := '/';
			// US format, not British!
			Month := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Day := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Year := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
	{		Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
			Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
			Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);}
		end
		else if Pos('-', Str) <> 0 then
		begin
			DateSep := '-';
			Year := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Month := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Day := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
	{		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
			Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
			Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);}
		end
		else if Pos('.', Str) <> 0 then
		begin
			DateSep := '.';
			Day := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Month := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Year := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
	{		Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);
			Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
			Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);}
		end
		else if Length(Str) = 6 then
		begin
			Year := ReadSGFast(Copy(Str, 1, 2));
			if Year < 30{TODO : Get From API for ifDisplay} then Inc(Year, 2000) else Inc(Year, 1900);
			Month := ReadSGFast(Copy(Str, 3, 2));
			Day := ReadSGFast(Copy(Str, 5, 2));
	{		Year := StrToValI(Copy(Str, 1, 2), False, 00, UG(00), 99, 1);
			if Year < ICentury then Inc(Year, 2000) else Inc(Year, 1900);
			Month := StrToValI(Copy(Str, 3, 2), False, 1, UG(1), 99, 1);
			Day := StrToValI(Copy(Str, 5, 2), False, 1, UG(1), 31, 1);}
		end
		else if Length(Str) = 8 then
		begin
			Year := ReadSGFast(Copy(Str, 1, 4));
			Month := ReadSGFast(Copy(Str, 5, 2));
			Day := ReadSGFast(Copy(Str, 7, 2));
	{		Year := StrToValI(Copy(Str, 1, 4), False, 1900, UG(1900), 9999, 1);
			Month := StrToValI(Copy(Str, 5, 2), False, 1, UG(1), 12, 1);
			Day := StrToValI(Copy(Str, 7, 2), False, 1, UG(1), 31, 1);}
		end
		else
	//	if (Pos(',', Str) <> 0) or (Str[1] = '3') then
		begin
			Result := 0; //StrToValI(Str, False, 0, 0, MaxInt, 1);
			Exit;
		end;
		if Month > 50 then Dec(Month, 50); // Female offset
		if TryEncodeDate(Year, Month, Day, TDateTime(Result)) = False then
		begin
			ErrorMsg('Invalid date %1' + '.', [Str]);
			Result := 0;
		end;
	end;
	end;
end;

function SToDateTime(const Str: string; const InputFormat: TInputFormat): TDateTime;
var InLineIndex: SG;
begin
	case InputFormat of
	ifDisplay:
	begin
		Result := StrToDateTime(Str);
	end
	else
	begin
		InLineIndex := 1;
		Result := SToDate(ReadToChar(Str, InLineIndex, ' '), InputFormat) +
			SToTime(ReadToChar(Str, InLineIndex, CharCR), InputFormat);
	end;
	end;
end;

function SToMs(const Str: string; const InputFormat: TInputFormat): SG;
var
	V: S4;
	Mul: S4;
	W, F, DP: U1;
begin
	case InputFormat of
	ifDisplay:
	begin
		Result := Round(StrToTime(Str) * MSecsPerDay);
	end
	else
	begin
		V := 0;
		if Length(Str) > 0 then
		begin
			F := 0;
			for W := Length(Str) - 1 downto 1 do
			begin
				if Str[W] in ['.', ','] then
				begin
					F := W;
					Break;
				end;
			end;
			Mul := 1000 div 10;
			if F > 0 then
			for W := F + 1 to Length(Str) do
			begin
				case Str[W] of
				'0'..'9':
				begin
					V := V + Mul * (Ord(Str[W]) - Ord('0'));
					Mul := Mul div 10;
				end;
				end;
			end;
			Mul := 1000;
			DP := 0;
			if F = 0 then F := Length(Str) + 1;
			for W := F - 1 downto 1 do
			begin
				case Str[W] of
				'0'..'9':
				begin
					V := V + Mul * (Ord(Str[W]) - Ord('0'));
					if V > 100000000 then
					begin
						Result := V;
						Exit;
					end;
					if Mul < 100000000 then Mul := Mul * 10;
				end;
				':':
				begin
					case DP of
					0: Mul := Minute;
					1: Mul := Hour;
					2: Mul := Hour;
					end;
					Inc(DP);
				end;
				end;
			end;
		end;
		Result := V;
	end;
	end;
end;

function StrToF4(const Str: string; const InputFormat: TInputFormat): F4;
var
	E: Integer;
begin
	case InputFormat of
	ifDisplay:
		Result := StrToFloat(Str);
	else
		Val(Str, Result, E);
	end;
end;

function StrToF8(const Str: string; const InputFormat: TInputFormat): F8;
var
	E: Integer;
begin
	case InputFormat of
	ifDisplay:
		Result := StrToFloat(Str);
	else
		Val(Str, Result, E);
	end;
end;

function StrToFA(const Str: string; const InputFormat: TInputFormat): FA;
var
	E: Integer;
begin
	case InputFormat of
	ifDisplay:
		Result := StrToFloat(Str);
	else
		Val(Str, Result, E);
	end;
end;

function StrToSG(const Str: string; const InputFormat: TInputFormat): SG;
var
	E: Integer;
begin
	case InputFormat of
	ifDisplay:
		Result := StrToInt(Str);
	else
		Val(Str, Result, E);
	end;
end;

function StrToBG(const Str: string; const InputFormat: TInputFormat): BG;
begin
	Result := StrToSG(Str, InputFormat) <> 0;
end;

function SToTime(const Str: string; const InputFormat: TInputFormat): TDateTime;
begin
	case InputFormat of
	ifDisplay:
	begin
		Result := StrToTime(Str);
	end
	else
	begin
		Result := SToMs(Str, InputFormat) / MSecsPerDay;
	end;
	end;
end;

end.
