//* File:     Lib\uOptions.pas
//* Created:  1997-01-01
//* Modified: 2007-08-20
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uOptions;

interface

uses uTypes, uDIniFile;

type
	TVisualStyle = (vsCheck, vsSpin, vsCombo, vsButton, vsString); // vsColor, vsFilename, vsDirectory 
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
	end;
	PParams = ^TParams;
	TParams = array[0..1023] of TParam;

	TOptionChanged = procedure(const OptionIndex: SG);

procedure DefaultOptions(const Options: POptions; const OptionCount: SG; const Params: PParams);
procedure RWOptions(const Options: POptions; const OptionCount: SG; const Params: PParams; const IniFile: TDIniFile; const Section: string; const Save: BG);

implementation

uses
	SysUtils,
	uMath, uStrings;

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
		vsCheck, vsSpin, vsCombo:
		begin
			P.Num := E.Default;
			{$ifdef Debug}
			if E.Typ in [vsSpin, vsCombo] then
			begin
				Assert(E.Default >= E.Minimum);
				Assert(E.Default <= E.Maximum);
			end;
			{$endif}
		end;
		vsString:
			P.Str := E.DefaultStr;
		end;
	end;
end;

procedure RWOptions(const Options: POptions; const OptionCount: SG; const Params: PParams; const IniFile: TDIniFile; const Section: string; const Save: BG);
var
	OptionIndex: SG;
//	Player: SG;
	Option: POption;
	Param: PParam;
begin
//	for Player := 0 to Length(Game.Params) - 1 do
//	begin
		for OptionIndex := 0 to OptionCount - 1 do
		begin
			Option := @Options[OptionIndex];
			Param := @Params[OptionIndex];
			case Option.Typ of
			vsCheck,
			vsSpin,
			vsCombo:
			begin
				if Save = False then Param.Num := Option.Default;
				IniFile.RWNum(Section, Option.Name, Param.Num, Save);
				if Save = False then
					if Option.Typ <> vsCheck then
						Param.Num := Range(Option.Minimum, Param.Num, Option.Maximum)
					else
						Param.Num := Param.Num and 1;
			end;
			vsString:
			begin
				if Save = False then Param.Str := Option.DefaultStr;
				IniFile.RWString(Section, Option.Name, Param.Str, Save);
			end;
			vsButton:
			begin
				// Nothing to save.
			end;
			else
				raise Exception.Create('Invalid option visual style.');
			end;
		end;
//	end;
end;

initialization
	EnumToStr(TypeInfo(TVisualStyle), VisualStyleStr);
end.
