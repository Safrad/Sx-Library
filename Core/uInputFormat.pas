unit uInputFormat;

interface

uses
  Velthuis.BigDecimals,
	uTypes,
  uVector,
  uParserMsg,
  uTimeSpan,
  uLapStopwatch;

type
	TInputFormat = (ifIO{Disk File Input/Output}, ifDisplay{Windows Locale});

function StrToF4(const Str: string; const InputFormat: TInputFormat): F4;
function StrToF8(const Str: string; const InputFormat: TInputFormat): F8;
{$ifndef CPUX64}
function StrToFA(const Str: string; const InputFormat: TInputFormat): FA;
{$endif}
function StrToSG(const Str: string; const InputFormat: TInputFormat): SG;
function StrToBG(const Str: string; const InputFormat: TInputFormat): BG;
function SToTime(const Str: string; const InputFormat: TInputFormat): TDateTime;
function SToMs(const Str: string; const InputFormat: TInputFormat): SG; // MsToStr<-

// Str To Data
function StrToMs(Line: string; const MinVal, DefVal, MaxVal: TTimeSpan; const UseWinFormat: BG; const Messages: TParserMessages = nil): TTimeSpan;

function StrToVector(const Line: string; const UseWinFormat: BG; const Messages: TParserMessages = nil; const LapStopwatch: TLapStopwatch = nil): TVector;
function StrToValBD(const Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: BigDecimal; const Messages: TParserMessages = nil): BigDecimal; overload;
function StrToValBD(const Line: string; const UseWinFormat: BG;
	const DefVal: BigDecimal; const Messages: TParserMessages = nil): BigDecimal; overload;

(*
function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: FG; const Messages: TParserMessages = nil): FG; overload;
function StrToValE(Line: string; const UseWinFormat: BG;
	const DefVal: FG; const Messages: TParserMessages = nil): FG; overload;
*)

function StrToValI(Line:string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG; const Messages: TParserMessages = nil): SG; overload;
function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; const Messages: TParserMessages = nil): UG; overload;

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8; const Messages: TParserMessages = nil): S8;

function StrToValU8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: U8; const Messages: TParserMessages = nil): U8;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1; const Messages: TParserMessages = nil): U1;

function SToDate(const Str: string; const InputFormat: TInputFormat): TDateTime;
function SToDateTime(const Str: string; const InputFormat: TInputFormat): TDateTime;

implementation

uses
	SysUtils,
  Math,
  uMathExpressionParser,
	uTimeExpressionParser,
  uStrings, uMath, uOutputFormat,
  uSxStringParser,
  uExpressionTreeEvaluator;

function StrToMs(Line: string; const MinVal, DefVal, MaxVal: TTimeSpan; const UseWinFormat: BG; const Messages: TParserMessages = nil): TTimeSpan;
var Parser: TTimeExpressionParser;
begin
	Parser := TTimeExpressionParser.Create;
	try
    Parser.SxParser := TSxStringParser.Create;
    TSxStringParser(Parser.SxParser).Text := Line;

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
    Parser.Parse;
		Result := Parser.Value;
	finally
    Parser.SxParser.Free;
		Parser.Free;
	end;
end;

function StrToVector(const Line: string; const UseWinFormat: BG; const Messages: TParserMessages = nil; const LapStopwatch: TLapStopwatch = nil): TVector;
var
  Parser: TMathExpressionParser;
  ExpressionTreeEvaluator: TExpressionTreeEvaluator;
begin
	Parser := TMathExpressionParser.Create;
	try
    Parser.SxParser := TSxStringParser.Create;
    TSxStringParser(Parser.SxParser).Text := Line;

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

    LapStopwatch.Restart;
    Parser.ReadInput;
    ExpressionTreeEvaluator := Parser.CreateExpressionTreeEvaluator;
    try
      LapStopwatch.StoreLap;
      if ExpressionTreeEvaluator <> nil then
      begin
        Result := ExpressionTreeEvaluator.EvaluateRoot;
      end
      else
        Result := nil;
    finally
      ExpressionTreeEvaluator.Free;
    end;
    LapStopwatch.Stop;
    LapStopwatch.StoreLap;
	finally
    Parser.SxParser.Free;
		Parser.Free;
	end;
end;

function StrToValBD(const Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: BigDecimal; const Messages: TParserMessages = nil): BigDecimal;
var
	Parser: TMathExpressionParser;
	s: string;
begin
	Parser := TMathExpressionParser.Create;
	try
    Parser.SxParser := TSxStringParser.Create;
    TSxStringParser(Parser.SxParser).Text := Line;

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
		Result := Parser.ReadNum(MinVal, DefVal, MaxVal);
		if Messages = nil then
		begin
			if Parser.Messages.Count > 0 then
			begin
				s := Parser.Messages.ToString;
				Parser.Messages.Free;
				raise EConvertError.Create(s);
			end
			else
			begin
				Parser.Messages.Free;
			end;
		end
	finally
    Parser.SxParser.Free;
		Parser.Free;
	end;
end;

function StrToValBD(const Line: string; const UseWinFormat: BG;
	const DefVal: BigDecimal; const Messages: TParserMessages = nil): BigDecimal;
var
	Parser: TMathExpressionParser;
	s: string;
begin
	Parser := TMathExpressionParser.Create;
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
		Result := Parser.ReadNum(DefVal);
		if Messages = nil then
		begin
			if Parser.Messages.Count > 0 then
			begin
				s := Parser.Messages.ToString;
				Parser.Messages.Free;
				raise EConvertError.Create(s);
			end
			else
			begin
				Parser.Messages.Free;
			end;
		end
	finally
		Parser.Free;
	end;
end;

(*
function StrToValE(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: FG; const Messages: TParserMessages = nil): FG;
begin
	Result := StrToValBD(Line, UseWinFormat, MinVal, DefVal, MaxVal, Messages);
end;

function StrToValE(Line: string; const UseWinFormat: BG;
	const DefVal: FG; const Messages: TParserMessages = nil): FG;
begin
	Result := StrToValBD(Line, UseWinFormat, DefVal, Messages);
end;
*)

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: SG; const Messages: TParserMessages = nil): SG;
begin
	Result := BigDecimal.Round(Denominator * StrToValBD(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG; const Messages: TParserMessages = nil): UG;
begin
	Result := BigDecimal.Round(Denominator * StrToValBD(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValS8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: S8; const Messages: TParserMessages = nil): S8;
begin
	Result := BigDecimal.Round(Denominator * StrToValBD(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValU8(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: U8; const Messages: TParserMessages = nil): U8;
begin
	Result := BigDecimal.Round(Denominator * StrToValBD(Line, UseWinFormat, MinVal / Denominator, DefVal / Denominator, MaxVal / Denominator, Messages));
end;

function StrToValU1(Line: string; const UseWinFormat: BG;
	const DefVal: U1; const Messages: TParserMessages = nil): U1;
begin
	Result := StrToValI(Line, UseWinFormat, 0, UG(DefVal), 255, 1, Messages);
end;

{$if CompilerVersion < 14}
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
{$ifend}

function SToDate(const Str: string; const InputFormat: TInputFormat): TDateTime;
const
  FutureYears: array[TInputFormat] of SG = (0, 30{Get From API for ifDisplay});
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
			// preffered
			DateSep := '-';
			Year := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Month := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
			Day := ReadSGFast(ReadToChar(Str, InLineIndex, DateSep));
	{		Year := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1900, UG(1900), 9999, 1);
			Month := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 12, 1);
			Day := StrToValI(ReadToChar(Str, InLineIndex, DateSep), False, 1, UG(1), 31, 1);}
		end
		else if Pos(':', Str) <> 0 then
		begin
			// exif
			DateSep := ':';
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
			Year := 100 * ((CurrentYear + FutureYears[InputFormat]) div 100) + ReadSGFast(Copy(Str, 1, 2));
      if Year > CurrentYear + FutureYears[InputFormat] then
        Dec(Year, 100);
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
		if Month > 50 then
      Dec(Month, 50); // Female offset
    if (Day = 0) and (Month = 0) then
    begin
      Day := 1;
      Month := 1;
    end;
		if TryEncodeDate(Year, Month, Day, TDateTime(Result)) = False then
		begin
//			raise EConvertError.Create('Invalid date ' + AddSingleQuoteF(Str) + '.');
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
		Result := SToDate(ReadToChar(Str, InLineIndex, CharSpace), InputFormat) +
			SToTime(ReadToChar(Str, InLineIndex, CharSpace), InputFormat);
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
		Result := RoundN(StrToTime(Str) * MSecsPerDay);
	end
	else
	begin
		V := 0;
		if Length(Str) > 0 then
		begin
			F := 0;
			for W := Length(Str) - 1 downto 1 do
			begin
				if CharInSet(Str[W], ['.', ',']) then
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

{$ifndef CPUX64}
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
{$endif}

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
