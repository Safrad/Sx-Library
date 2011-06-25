//* File:     Lib\uParams.pas
//* Created:  2006-02-04
//* Modified: 2008-02-16
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uParams;

interface

uses
	uTypes,
	SysUtils;

const
	paFile = '--file';
	paNumber = '--number';
type
	TParamProcedure = procedure(const Value: string);

procedure RegisterParam(const ParameterName: string; const ParameterDescription: string; const ParameterProcedure: TParamProcedure);
procedure HelpParams(const Value: string = '');
procedure ReadCommandLine(const CmdLine: string);

implementation

uses
	uLog, uFiles, uStrings, uMsg{$ifndef Console}, Forms{$endif};

var
	Params: array of string;
	DesParams: array of string;
	ParamProcedures: array of TParamProcedure;
	paFileIndex: SG = -1;
	paNumberIndex: SG = -1;

procedure RegisterParam(const ParameterName: string; const ParameterDescription: string; const ParameterProcedure: TParamProcedure);
var
	j: SG;
begin
	Assert(ParameterName <> '');

	j := Length(Params);
	if ParameterName = paFile then
		paFileIndex := j
	else if ParameterName = paNumber then
		paNumberIndex := j;

	SetLength(Params, j + 1);
	Params[j] := DelCharsF(ParameterName, ' ');

	j := Length(DesParams);
	SetLength(DesParams, j + 1);
	DesParams[j] := ParameterDescription;

	j := Length(ParamProcedures);
	SetLength(ParamProcedures, j + 1);
	ParamProcedures[j] := ParameterProcedure;
end;

procedure HelpParams(const Value: string = '');
var
	i: SG;
	s: string;
begin
	s := 'Parameter' + CharTab + 'Description' + LineSep;
	s := s + StringOfChar('—', 80) + LineSep;
	for i := 0 to Length(Params) - 1 do
	begin
		s := s + Params[i] + CharTab + CharTab + DesParams[i] + LineSep;
	end;
{	if AcceptFile then
		s := s + 'Filename' + CharTab + 'Open this filename on startup' + LineSep;}
	Information(s + '.');
end;

procedure ParamMinimized(const Value: string = '');
begin
	{$ifndef Console}
	Application.ShowMainForm := False;
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
	AF, AN: BG;
	IsFile: BG;
	i: SG;
	ParamFile: TFileName;
begin
	AF := paFileIndex >= 0;
	AN := paNumberIndex >= 0;
	DelQuote(Param);
	if Param = '' then Exit;
	if Param[1] in SwitchChars then
	begin
		Delete(Param, 1, 1);
		if Param = '' then Exit;
		AF := False;
		AN := False;
	end;
	IsFile := True;
	for i := 0 to Length(Params) - 1 do
	begin
		if StartStr(UpperCase(Param), UpperCase(Params[i])) then
		begin
			IsFile := False;
			try
				ParamProcedures[i](Copy(Param, Length(Params[i]) + 1, MaxInt));
			except
				on E: Exception do
					Fatal(E, nil);
			end;
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
				Warning(//'Illegal "%1" command line parameter.', Param
					'Command line file %1 not found.', [ParamFile]);
				Exit;
			end
			else
			begin
				try
					ParamProcedures[paFileIndex](Param);
				except
					on E: Exception do
						Fatal(E, nil);
				end;
			end;
		end
		else if AN and (Param[1] in ['0'..'9']) then
		begin
			try
				ParamProcedures[paNumberIndex](Param);
			except
				on E: Exception do
					Fatal(E, nil);
			end;
		end
		else
		begin
			Warning('Illegal %1 command line parameter.', [Param]);
			Exit;
		end;
	end;
end;

procedure ReadCommandLine(const CmdLine: string);
var
	InLineIndex: SG;
	Quote: BG;
	LastParam: SG;
	ParamCount: UG;
begin
	MainLogAdd('Reading command line: ' + CmdLine, mlInformation);
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
	// Add common parameters
	RegisterParam('Help', 'Displays this help dialog', HelpParams);
	RegisterParam('Minimized', 'Minimizes application', ParamMinimized);
	RegisterParam('Exit', 'Exits program', ParamExit);
end;

initialization
	Initialize;
finalization
	SetLength(Params, 0);
	SetLength(DesParams, 0);
	SetLength(ParamProcedures, 0);
end.
