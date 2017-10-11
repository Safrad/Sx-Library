unit uParams;

interface

uses
  uTypes, SysUtils;

const
  paFile = '--file';
  paNumber = '--number';

type
  TParamProcedure = procedure(const Value: string);
{$ifndef Console}

var
  MinimizeToTrayIcon: BG = False;
{$endif}

procedure RegisterParam(const ParameterName: string; const ParameterDescription: string; const ParameterProcedure:
  TParamProcedure);

procedure HelpParams(const Value: string = '');

procedure AddCommonParams;

procedure ReadCommandLine(const CmdLine: string);

implementation

uses
  Windows,
  uLog, uFiles, uStrings, uConsole, uTable, uRow, uCell, uMsg{$ifndef Console}, Forms, uCommon{$endif}, Classes;

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

function PreviewTableArgument(const AParamIndex: SG): TRow;
var
  Cell: TCell;
begin
  FillChar(Cell, SizeOf(TCell), 0);
  Result := TRow.Create(2);

  Cell.Text := Params[AParamIndex];
  Cell.HorizontalAlign := taLeftJustify;
  Result.Columns[0] := Cell;

  Cell.Text := DesParams[AParamIndex];
  Cell.HorizontalAlign := taLeftJustify;
  Result.Columns[1] := Cell;
//            row.Columns.Add(new Cell { Text = argument.GetArgumentShortcutAndSyntax() });
//            row.Columns.Add(new Cell { Text = argument.Description });
//            row.Columns.Add(new Cell { Text = argument.GetRequiredOrOptional() + argument.GetRequireList() });
end;

procedure HelpParamsAsMessage;
const
	CONSOLE_WINDOW_WIDTH = 78;
var
  i: SG;
  s: string;
begin
	s := 'Parameter' + CharTab + 'Description' + LineSep;
	s := s + StringOfChar('-', CONSOLE_WINDOW_WIDTH) + LineSep;
	for i := 0 to Length(Params) - 1 do
	begin
		s := s + Params[i] + CharTab + CharTab + DesParams[i] + LineSep;
	end;
	Information(s + '.');
end;

procedure HelpParamsToConsole;
var
  i: SG;
  Table: TTable;
  Row: TRow;
  Cell: TCell;
begin
  FillChar(Cell, SizeOf(TCell), 0);

  Row := TRow.Create(2);

  Cell.Text := 'Parameter';
  Cell.HorizontalAlign := taCenter;
  Row.Columns[0] := Cell;

  Cell.Text := 'Description';
  Cell.HorizontalAlign := taCenter;
  Row.Columns[1] := Cell;

//  Cell.Text := 'Required';
//  Cell.HorizontalAlign := taCenter;
//  Row.Columns[2] := Cell;

  Table := TTable.Create(1 + Length(Params));
  Table.Data[0] := Row;
  for i := 0 to Length(Params) - 1 do
  begin
    Row := PreviewTableArgument(i);
    Table.Data[i + 1] := Row;
  end;

  Table.WriteToConsole
end;

procedure HelpParams(const Value: string = '');
begin
  if IsConsole then
    HelpParamsToConsole
  else
    HelpParamsAsMessage;
end;

procedure ParamMinimized(const Value: string = '');
begin
	{$ifndef Console}
  DisableSplash := True;
  if MinimizeToTrayIcon then
  begin
    Application.ShowMainForm := False
  end
  else
  begin
    if Assigned(Application.MainForm) then
      Application.MainForm.WindowState := wsMinimized;
  end;
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
  Value: string;
  StartIndex: SG;
begin
  AF := paFileIndex >= 0;
  AN := paNumberIndex >= 0;
  DelQuote(Param);
  if Param = '' then
    Exit;
  if CharInSet(Param[1], SwitchChars) then
  begin
    Delete(Param, 1, 1);
    if Param = '' then
      Exit;
    AF := False;
    AN := False;
  end;
  IsFile := True;
  for i := 0 to Length(Params) - 1 do
  begin
    if StartStr(UpperCase(Params[i]), UpperCase(Param)) then
    begin
      IsFile := False;
      StartIndex := Length(Params[i]) + 1;
      if CharAt(Param, StartIndex) = '=' then
        Inc(StartIndex);
      Value := DelQuoteF(Copy(Param, StartIndex, MaxInt));
      try
        ParamProcedures[i](Value);
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
      ParamFile := ExpandFileCmd(Param);
      if (not FileExists(ParamFile)) and (not DirectoryExists(ParamFile)) then
      begin
        Warning(//'Illegal "%1" command line parameter.', Param
          'Command line file %1 not found.', [ParamFile]);
        Exit;
      end
      else
      begin
        try
          if Assigned(ParamProcedures[paFileIndex]) then
            ParamProcedures[paFileIndex](ParamFile);
        except
          on E: Exception do
            Fatal(E, nil);
        end;
      end;
    end
    else if AN and CharInSet(Param[1], ['0'..'9']) then
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

procedure AddCommonParams;
begin
  RegisterParam('Help', 'Displays this help dialog', HelpParams);
  RegisterParam('Minimized', 'Minimizes application', ParamMinimized);
  RegisterParam('Exit', 'Exits program', ParamExit);
end;

procedure ReadCommandLine(const CmdLine: string);
var
  InLineIndex: SG;
  Quote: BG;
  LastParam: SG;
  ParamCount: UG;
begin
  if LogInformation then
    MainLogAdd('Reading command line: ' + CmdLine, mlInformation);
  ParamCount := 0;
  Quote := False;
  LastParam := 1;
  InLineIndex := 1;
  while InLineIndex <= Length(CmdLine) do
  begin
//		if (LastParam = InLineIndex) or Quote then
    begin
      case CmdLine[InLineIndex] of
        '"':
          Quote := not Quote;
      end;
    end;

    if ((Quote = False) and (CmdLine[InLineIndex] = CharSpace)) or (InLineIndex = Length(CmdLine)) then
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

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}



finalization
{$IFNDEF NoFinalization}
  SetLength(Params, 0);
  SetLength(DesParams, 0);
  SetLength(ParamProcedures, 0);
{$ENDIF NoFinalization}

end.

