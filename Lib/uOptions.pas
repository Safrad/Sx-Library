//* File:     Lib\uOptions.pas
//* Created:  1997-01-01
//* Modified: 2007-08-20
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uOptions;

interface

uses uTypes, uDIniFile, TypInfo;

type
	TVisualStyle = (vsCheck, vsSpin, vsCombo, vsButton, vsString, vsFilename, vsDirectory, vsTime, {, vsDateTime TODO }vsColor, vsUnknown);
var
	VisualStyleStr: array[TVisualStyle] of string;
type
	POption = ^TOption;
	TOption = record
		Name: string;
		Typ: TVisualStyle;
		Reserved: array[0..2] of U1;
		Default: S4;
		Minimum: S4;
		Maximum: S4;
		DefaultStr: string;
	end;
	POptions = ^TOptions;
	TOptions = array[0..1023] of TOption;

	PParam = ^TParam;
	TParam = packed record // 8
		Str: string;
		case Integer of
		0: (Num: S4);
		1: (Bool: B4);
// TODO		2: (ExNum: U8);
	end;
	PParams = ^TParams;
	TParams = array[0..1023] of TParam;

	TOptionChanged = procedure(const OptionIndex: SG) of object;

procedure InitOptionNames(TypeInfo: PTypeInfo; var Options: array of TOption);
procedure DefaultOptions(const Options: POptions; const OptionCount: SG; const Params: PParams);
procedure RWOptions(const Options: POptions; const OptionCount: SG; const Params: PParams; const IniFile: TDIniFile; const Section: string; const Save: BG);
function StrToVisualStyle(const s: string): TVisualStyle;
function ParamToStr(E: POption; P: PParam): string;
function StrToParam(E: POption; const s: string): TParam;

implementation

uses
	SysUtils,
	uInputFormat,
	uMath, uStrings, Math;

procedure InitOptionNames(TypeInfo: PTypeInfo; var Options: array of TOption);
var i: SG;
begin
	for i := 0 to Length(Options) - 1 do
	begin
		Options[i].Name := Copy(GetEnumName(TypeInfo, i), 3, MaxInt);
		if Options[i].Typ = vsColor then
		begin
			Options[i].Minimum := MinInt;
			Options[i].Maximum := MaxInt;
		end;
	end;
end;

procedure DefaultOptions(const Options: POptions; const OptionCount: SG; const Params: PParams);
var
	i: SG;
	E: POption;
	P: PParam;
begin
	for i := 0 to OptionCount - 1 do
	begin
		E := @Options[i];
		P := @Params[i];
		case E.Typ of
		vsCheck, vsSpin, vsCombo, vsTime, vsColor:
		begin
			P.Num := E.Default;
			{$ifdef Debug}
			if E.Typ in [vsSpin, vsCombo, vsTime] then
			begin
				Assert(E.Default >= E.Minimum);
				Assert(E.Default <= E.Maximum);
			end;
			{$endif}
		end;
		vsString, vsFileName, vsDirectory:
			P.Str := E.DefaultStr;
		vsButton:
		begin
      // No data
		end
		else
			raise Exception.Create('Invalid option type.');
		end;
	end;
end;

procedure RWOptions(const Options: POptions; const OptionCount: SG; const Params: PParams; const IniFile: TDIniFile; const Section: string; const Save: BG);
var
	OptionIndex: SG;
	Option: POption;
	Param: PParam;
begin
	for OptionIndex := 0 to OptionCount - 1 do
	begin
		Option := @Options[OptionIndex];
		Param := @Params[OptionIndex];
		case Option.Typ of
		vsCheck,
		vsSpin,
		vsCombo,
		vsTime,
		vsColor:
		begin
			if Save = False then Param.Num := Option.Default;
			IniFile.RWNum(Section, Option.Name, Param.Num, Save);
			if Save = False then
				if Option.Typ <> vsCheck then
					Param.Num := Range(Option.Minimum, Param.Num, Option.Maximum)
				else
					Param.Num := Param.Num and 1;
		end;
		vsString, vsFileName, vsDirectory:
		begin
			if Save = False then Param.Str := Option.DefaultStr;
			IniFile.RWString(Section, Option.Name, Param.Str, Save);
		end;
		vsButton:
		begin
			// Nothing to read/write.
		end;
		else
			raise Exception.Create('Invalid option visual style.');
		end;
	end;
end;

function StrToVisualStyle(const s: string): TVisualStyle;
var
	i: TVisualStyle;
begin
	Result := vsUnknown;
	for i := Low(i) to High(i) do
	begin
		if UpperCase(s) = UpperCase(VisualStyleStr[i]) then
		begin
			Result := i;
			Break;
		end;
	end;
end;

function ParamToStr(E: POption; P: PParam): string;
var
	i: SG;
	InLineIndex: SG;
	s: string;
begin
	case E.Typ of
	vsCheck:
	begin
		Result := FalseTrue[SG(SG(P.Bool) <> 0)];
	end;
	vsSpin, vsTime, vsColor:
	begin
		Result := IntToStr(P.Num);
	end;
	vsCombo:
	begin
		i := 0;
		InLineIndex := 1;
		while InLineIndex <= Length(E.DefaultStr) do
		begin
			s := ReadToChar(E.DefaultStr, InLineIndex, CharTab);
			if i = P.Num then
			begin
				Result := s;
				Break;
			end;
			Inc(i);
		end;
	end;
	vsString, vsFileName, vsDirectory:
	begin
		Result := P.Str;
	end;
	vsUnknown, vsButton:
		
	end;
end;

function StrToParam(E: POption; const s: string): TParam;
begin
	case E.Typ of
	vsCheck:
	begin
		Result.Bool := False;
		if s = FalseTrue[0] then
			Result.Bool := False
		else if s = FalseTrue[1] then
			Result.Bool := True
		else
			Exception.Create('Invalid parameter value.');
	end;
	vsSpin, vsTime, vsColor:
	begin
		Result.Num := StrToValI(s, False, E.Minimum, E.Default, E.Maximum, 1);
	end;
	vsCombo:
	begin
// TODO		Result.Num :=
	end;
	vsString, vsFileName, vsDirectory:
	begin
		Result.Str := s;
	end;
	vsUnknown, vsButton:
		
	end;
end;

initialization
	EnumToStr(TypeInfo(TVisualStyle), VisualStyleStr);
end.
