unit uParams;

interface

uses
	uTypes,
	SysUtils;

const
	paFile = '--file';
var
	AcceptFile: BG;
	ParamFile: TFileName;
type
	TParamProcedure = procedure(const Value: string);

procedure RegisterParam(const Param: string; const DesParam: string; const ParamProcedure: TParamProcedure);
//function GetParamValue(const ParamName: string): string;
//function UsedAllParams: BG;

//procedure AddParams(const AParams: array of string; const ADesParams: array of string);
{function CompareParams: SG;
procedure CloseParams;}
procedure HelpParams(const Value: string = '');
procedure ReadCommandLine(const CmdLine: string);

implementation

uses
	uFiles, uStrings, uMsg{$ifndef Console}, Forms{$endif};

var
	UsedParams: array of BG;
{
function GetParamValue(const ParamName: string): string;
var
	i: SG;
	s: string;
begin
	Result := '';
	for i := 1 to ParamCount do
	begin
		s := ParamStr(i);
		if Length(s) = 0 then Continue;
		if s[1] in SwitchChars then Delete(s, 1, 1);
		if StartStr(UpperCase(s), ParamName) then
		begin
			UsedParams[i - 1] := True;
			Result := Copy(s, Length(ParamName) + 1, MaxInt);
		end;
	end;
end;
}
{
function UsedAllParams: BG;
var i: SG;
begin
	for i := 1 to ParamCount do
	begin
		if UsedParams[i] = False then
		begin
			Result := False;
			Exit;
		end;
	end;
	Result := True;
end;}

var
	Params: array of string;
	DesParams: array of string;
	ParamProcedures: array of TParamProcedure;
	paFileIndex: SG;
	IllegalParam: BG = False;

procedure RegisterParam(const Param: string; const DesParam: string; const ParamProcedure: TParamProcedure);
var
	j: SG;
begin
	Assert(Param <> '');

	j := Length(Params);
	if Param = paFile then
		paFileIndex := j;
	SetLength(Params, j + 1);
	Params[j] := DelCharsF(Param, ' ');

	j := Length(DesParams);
	SetLength(DesParams, j + 1);
	DesParams[j] := DesParam;

	j := Length(ParamProcedures);
	SetLength(ParamProcedures, j + 1);
	ParamProcedures[j] := ParamProcedure;
end;

{
procedure AddParams(const AParams: array of string; const ADesParams: array of string);
var
	i: SG;
	j: SG;
begin
	Assert(Length(AParams) = Length(ADesParams));

	j := Length(Params);
	SetLength(Params, j + Length(AParams));
	for i := 0 to Length(AParams) - 1 do
		Params[i + j] := DelCharsF(AParams[i], ' ');

	j := Length(DesParams);
	SetLength(DesParams, j + Length(ADesParams));
	for i := 0 to Length(ADesParams) - 1 do
		DesParams[i + j] := ADesParams[i];
end;
}
procedure HelpParams(const Value: string = '');
var
	i: SG;
	s: string;
begin
	s := 'Param.' + CharTab + 'Description' + LineSep;
	s := s + StringOfChar('-', 96) + LineSep;
	for i := 0 to Length(Params) - 1 do
	begin
		s := s + Params[i] + CharTab + DesParams[i] + LineSep;
	end;
	if AcceptFile then
		s := s + 'Filename' + CharTab + 'Open this filename on startup' + LineSep;
	Information(s);
end;

procedure ParamMinimized(const Value: string = '');
begin
	{$ifndef Console}
	Application.ShowMainForm := False;
	{$else}
	// TODO: Minimize
	{$endif}
end;

procedure ParamExit(const Value: string = '');
begin
	{$ifndef Console}
	Application.Terminate;
	{$else}
	Halt;
	{$endif}
end;

procedure ReadParam(Param: string);
var
	AF: BG;
	IsFile: BG;
	i: SG;
begin
	AF := AcceptFile;
	DelQuote(Param);
	if Param[1] = '-' then
	begin
		Delete(Param, 1, 1);
		AF := False;
	end
	else if Param[1] = '/' then
	begin
		Delete(Param, 1, 1);
		AF := False;
	end;
	IsFile := True;
	for i := 0 to Length(Params) - 1 do
	begin
		if StartStr(UpperCase(Param), UpperCase(Params[i])) then
		begin
			IsFile := False;
			ParamProcedures[i]('');
			Break;
		end;
	end;
	if IsFile then
	begin
		if AF then
		begin
			ParamFile := ExpandDir(Param);
			if (not FileExists(ParamFile)) and (not DirectoryExists(ParamFile)) then
			begin
				Warning('Illegal command line parameter' + LineSep +
					Param + LineSep +
					'Command line file not found' + LineSep + ParamFile);
				IllegalParam := True;
				Exit;
			end
			else
				ParamProcedures[paFileIndex](Param);
		end
		else
		begin
			Warning('Illegal command line parameter' + LineSep + Param);
			IllegalParam := True;
			Exit;
		end;
	end;
end;
{
function CompareParams: SG;
label LAgain;
var
	i: SG;
begin
	LAgain:
	if ParamIndex >= ParamCount then
		Result := paExit
	else
	begin
		ReadParam(ParamStr(ParamIndex + 1));
	end;
	Inc(ParamIndex)
end;}

{procedure CloseParams;
begin
	if IllegalParam then HelpParams;
end;}

procedure ReadCommandLine(const CmdLine: string);
var
	InLineIndex: SG;
	Quote: BG;
	LastParam: SG;
	ParamCount: UG;
begin
	ParamCount := 0;
	Quote := False;
	LastParam := 1;
	InLineIndex := 1;
	while InLineIndex <= Length(CmdLine) do
	begin
		case CmdLine[InLineIndex] of
		'"': Quote := not Quote;
		end;
		if ((Quote = False) and (CmdLine[InLineIndex] = ' ')) or (InLineIndex = Length(CmdLine)) then
		begin
			if InLineIndex = Length(CmdLine) then
				Inc(InLineIndex);
			if (ParamCount > 0) and (LastParam < InLineIndex - 1) then
			begin
				ReadParam(Copy(CmdLine, LastParam, InLineIndex - LastParam));
			end;
			LastParam := InLineIndex + 1;
			Inc(ParamCount);
		end;
		Inc(InLineIndex);
	end;
end;

procedure Initialize;
begin
	SetLength(UsedParams, ParamCount);
	RegisterParam('Help', 'Displays this help dialog', HelpParams);
	RegisterParam('Minimized', 'Starts program minimized', ParamMinimized);
	RegisterParam('Exit', 'Exits program', ParamExit);
end;

initialization
	Initialize;
finalization
	SetLength(UsedParams, 0);
	SetLength(Params, 0);
	SetLength(DesParams, 0);
end.

