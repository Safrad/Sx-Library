unit uParams;

interface

uses
	uTypes,
	SysUtils;

function GetParamValue(const ParamName: string): string;
function UsedAllParams: BG;

const
	paExit = -1;
	paFile = -2;
var
	AcceptFile: BG;
	ParamFile: TFileName;
	ParamIndex: SG = 0;

procedure AddParams(const AParams: array of string; const ADesParams: array of string);
function CompareParams: SG;
procedure CloseParams;
procedure HelpParams;

implementation

uses
	uFiles, uStrings, uMsg{$ifndef Console}, Forms{$endif};

var
	UsedParams: array of BG;

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
end;


var
	Params: array of string;
	DesParams: array of string;
	IllegalParam: BG = False;

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

function CompareParams: SG;
label LAgain;
var
	i: SG;
	Par: string;
	AF: BG;
begin
	LAgain:
	if ParamIndex >= ParamCount then
		Result := paExit
	else
	begin
		Par := ParamStr(ParamIndex + 1);
		AF := AcceptFile;
		if Par[1] = '-' then
		begin
			Delete(Par, 1, 1);
			AF := False;
		end
		else if Par[1] = '/' then
		begin
			Delete(Par, 1, 1);
			AF := False;
		end;
		Result := paFile;
		for i := 0 to Length(Params) - 1 do
		begin
			if StartStr(UpperCase(Par), UpperCase(Params[i])) then
			begin
				if i = 0 then
				begin
					HelpParams;
					Inc(ParamIndex);
					goto LAgain;
				end;

				if i = 1 then
				begin
					Inc(ParamIndex);
					{$ifndef Console}
					Application.ShowMainForm := False;
					{$else}
					// TODO: Minimize
					{$endif}
					goto LAgain;
				end;

				Result := i - 2 {Common parameters};
				Break;
			end;
		end;
		if Result = paFile then
		begin
			if AF then
			begin
				ParamFile := FullDir(Par);
				if (not FileExists(ParamFile)) and (not DirectoryExists(ParamFile)) then
				begin
					Warning('Illegal command line parameter' + LineSep +
						Par + LineSep +
						'Command line file not found' + LineSep + ParamFile);
					IllegalParam := True;
					Inc(ParamIndex);
					goto LAgain;
				end;
			end
			else
			begin
				Warning('Illegal command line parameter' + LineSep + Par);
				IllegalParam := True;
				Inc(ParamIndex);
				goto LAgain;
			end;
		end;
	end;
	Inc(ParamIndex)
end;

procedure CloseParams;
begin
	if IllegalParam then HelpParams;
end;

procedure HelpParams;
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

procedure Initialize;
begin
	SetLength(UsedParams, ParamCount);
	AddParams(['Help', 'Minimized'], ['Display this help dialog', 'Start program minimized']);
end;

initialization
	Initialize;
finalization
	SetLength(UsedParams, 0);
	SetLength(Params, 0);
	SetLength(DesParams, 0);
end.

