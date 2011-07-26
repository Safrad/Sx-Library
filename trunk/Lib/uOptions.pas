unit uOptions;

interface

uses uTypes, uDIniFile, TypInfo;

type
	TVisualStyle = (vsCheck, vsSpin, vsFloat, vsCombo, vsButton, vsString, vsFilename, vsDirectory,
		vsStrings, vsTime, { , TODO :  vsDateTime } vsColor, vsUnknown);

var
	VisualStyleStr: array [TVisualStyle] of string;

type
	POption = ^TOption;

	TOption = record
		Name: string;
		Typ: TVisualStyle;
		Reserved: array [0 .. 2] of U1;
		Default: S4;
		Minimum: S4;
		Maximum: S4;
		DefaultStr: string;
	end;

	POptions = ^TOptions;
	TOptions = array [0 .. 1023] of TOption;

	PParam = ^TParam;

	TParam = packed record // 8
		Str: string;
		case Integer of
		0:
			(Num: S4);
		1:
			(Bool: B4);
		2:
			(ExNum: U8);
		3:
			(C: PChar);
		4:
			(Float: Double);
	end;

	PParams = ^TParams;
	TParams = array [0 .. 1023] of TParam;

	TOptionChanged = procedure(const OptionIndex: SG) of object;

procedure InitOptionNames(TypeInfo: PTypeInfo; var Options: array of TOption);
function IsDefaultOption(const Option: POption; const Param: PParam): BG;
procedure DefaultOption(const Option: POption; const Param: PParam);
procedure DefaultOptions(const Options: POptions; const OptionCount: SG; const Params: PParams);
procedure RWOptions(const Options: POptions; const OptionCount: SG; const Params: PParams;
	const IniFile: TDIniFile; const Section: string; const Save: BG);
function StrToVisualStyle(const s: string): TVisualStyle;
function ParamToStr(E: POption; P: PParam; const ComboNumber: BG = False): string;
function StrToParam(E: POption; const s: string): TParam;

implementation

uses
	SysUtils, Math,
	uInputFormat, uOutputFormat,
	uMath, uStrings, uColor;

procedure InitOptionNames(TypeInfo: PTypeInfo; var Options: array of TOption);
var
	i: SG;
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

function IsDefaultOption(const Option: POption; const Param: PParam): BG;
begin
	case Option.Typ of
	vsCheck:
		Result := Param.Bool = (Option.Default <> 0);
	vsSpin, vsCombo, vsTime, vsColor:
			Result := Param.Num = Option.Default;
	vsFloat:
		Result := Param.Float = Option.Default;
	vsString, vsFilename, vsDirectory, vsStrings:
		Result := Param.Str = Option.DefaultStr;
	vsButton:
		Result := True;
	else
		raise Exception.Create('Invalid option type.');
	end;
end;

procedure DefaultOption(const Option: POption; const Param: PParam);
begin
	case Option.Typ of
	vsCheck:
		Param.Bool := Option.Default <> 0;
	vsSpin, vsCombo, vsTime, vsColor:
		begin
			Param.Num := Option.Default;
{$IFOPT d+}
			if Option.Typ in [vsSpin, vsCombo, vsTime] then
			begin
				Assert(Option.Default >= Option.Minimum);
				Assert(Option.Default <= Option.Maximum);
			end;
{$ENDIF}
		end;
	vsFloat:
		Param.Float := Option.Default;
	vsString, vsFilename, vsDirectory, vsStrings:
		Param.Str := Option.DefaultStr;
	vsButton:
		begin
			// No data
		end
	else
		raise Exception.Create('Invalid option type.');
	end;
end;

procedure DefaultOptions(const Options: POptions; const OptionCount: SG; const Params: PParams);
var
	i: SG;
begin
	for i := 0 to OptionCount - 1 do
	begin
		DefaultOption(@Options[i], @Params[i]);
	end;
end;

procedure RWOptions(const Options: POptions; const OptionCount: SG; const Params: PParams;
	const IniFile: TDIniFile; const Section: string; const Save: BG);
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
		vsCheck, vsSpin, vsTime, vsColor:
			begin
				if Save = False then
					Param.Num := Option.Default;
				IniFile.RWNum(Section, Option.Name, Param.Num, Save);
				if Save = False then
					if Option.Typ <> vsCheck then
						Param.Num := Range(Option.Minimum, Param.Num, Option.Maximum)
					else
						Param.Num := Param.Num and 1;
			end;
		vsCombo:
		begin
			if Save = False then
			begin
				Params[OptionIndex] := StrToParam(Option, IniFile.ReadString(Section, Option.Name, ''));
			end
			else
			begin
				IniFile.WriteString(Section, Option.Name, ParamToStr(Option, Param));
			end;
		end;
		vsFloat:
			begin
				if Save = False then
					Param.Float := Option.Default;
				IniFile.RWNum(Section, Option.Name, Param.Float, Save);
			end;
		vsString, vsFilename, vsDirectory:
			begin
				if Save = False then
					Param.Str := Option.DefaultStr;
				IniFile.RWString(Section, Option.Name, Param.Str, Save);
			end;
		vsStrings:
			begin
				if Save = False then
					Param.Str := Option.DefaultStr;
				IniFile.RWMultilineString(Section, Option.Name, Param.Str, Save);
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

function ParamToStr(E: POption; P: PParam; const ComboNumber: BG = False): string;
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
	vsSpin, vsTime:
		begin
			Result := IntToStr(P.Num);
		end;
	vsColor:
		begin
			try
				NumericBase := 16;
				UseThousandSeparator := False;
				Result := NToS(ColorRB(P.Num), '00000000');
			finally
				NumericBase := 10;
				UseThousandSeparator := True;
			end;
		end;
	vsFloat:
		Result := FToS(P.Float, ofIO);
	vsCombo:
		begin
			if ComboNumber = False then
			begin
				i := E.Minimum;
				InLineIndex := 1;
				while InLineIndex <= Length(E.DefaultStr) do
				begin
					s := ReadToChar(E.DefaultStr, InLineIndex, CharTab);
					if i = P.Num then
					begin
						Result := s;
						Exit;
					end;
					Inc(i);
				end;
			end;
			Result := IntToStr(P.Num);
		end;
	vsString, vsFilename, vsDirectory, vsStrings:
		begin
			Result := P.Str;
		end;
	vsUnknown, vsButton:
		;
	end;
end;

function StrToParam(E: POption; const s: string): TParam;
var
	i: SG;
	InLineIndex: SG;
	item: string;
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
			if E.Typ = vsColor then
				Result.Num := ColorRB(Result.Num);
		end;
	vsFloat:
		begin
			Result.Float := StrToValE(s, False, E.Minimum, E.Default, E.Maximum);
		end;
	vsCombo:
		begin
			i := 0;
			InLineIndex := 1;
			while InLineIndex <= Length(E.DefaultStr) do
			begin
				item := ReadToChar(E.DefaultStr, InLineIndex, CharTab);
				if item = s then
				begin
					Result.Num := i + E.Minimum;
					Exit;
				end;
				Inc(i);
			end;
      try
  			Result.Num := StrToValI(s, False, E.Minimum, E.Default, E.Maximum, 1);
      except

      end;
		end;
	vsString, vsFilename, vsDirectory, vsStrings:
		begin
			Result.Str := s;
		end;
	vsUnknown, vsButton:

	end;
	end;

initialization

EnumToStr(TypeInfo(TVisualStyle), VisualStyleStr);

end.
