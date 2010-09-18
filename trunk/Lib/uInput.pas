//* File:     Lib\uInput.pas
//* Created:  2004-03-07
//* Modified: 2005-02-15
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uInput;

interface

uses
	uAdd;

// Str To Data
function StrToMs(Line: AnsiString; const MinVal, DefVal, MaxVal: SG): SG;

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

procedure ShowAndClearErrors;

implementation

uses
	Dialogs,
	uParser, uError;

function StrToMs(Line: AnsiString; const MinVal, DefVal, MaxVal: SG): SG;
var Parser: TDParser;
begin
	Parser := TDParser.Create(Pointer(Line), Length(Line));
	Result := Parser.ReadMs(MinVal, DefVal, MaxVal);
	if Parser.InputType <> itEOI then Parser.AddMes2(mtUnusedChars, []);
	Parser.Free;
end;

function StrToValE(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal: Extended): Extended;
label LNext;
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
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator);
end;

function StrToValI(Line: string; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: UG): UG;
begin
	Result := StrToValI(Line, UseWinFormat, MinVal, DefVal, MaxVal, Denominator);
end;}

function StrToValI(Line: AnsiString; const UseWinFormat: BG;
	const MinVal, DefVal, MaxVal, Denominator: Integer): Integer;
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

(*
function StrToI(s: AnsiString; Decimals: SG): SG;
var
	Code: Integer;
	e: Extended;
	i: Integer;
	Point: SG;
begin
	if s = '' then
	begin
		Result := 0;
		Exit;
	end;
	// Disk Format 2,456,454,546.42454
//	if CharCount(s, '.') > 0 then IE(431);

	DelChars(s, ',');

	if Decimals > 0 then
	begin
		Val(s, e, Code);
		if Code <> 0 then
			Result := 0
		else
		begin
			Point := 10;
			for i := 2 to Decimals do
				Point := Point * 10;

			Result := Round(Point * e);
		end;
	end
	else
	begin
		Val(s, Result, Code);
		if Code <> 0 then
			Result := 0;
	end;
end;*)

procedure ShowAndClearErrors;
begin
	if CompileMes.Count > 0 then
	begin
		MessageD(MesToStrings, mtWarning, [mbOk]);
		CompileMes.Clear;
	end;
end;

end.
