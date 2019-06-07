unit uArguments;

interface

uses
  Contnrs,
  uTypes,
  uRow,
  uCustomArgument;

type
  TArrayOfStringPair = array of TStringPair;

  TArguments = class
  private
    FArguments: TObjectList;

    function ParseString(ASource: string): TArrayOfStringPair;
    procedure ApplyArguments(const AArguments: TArrayOfStringPair);

    function GetExistsCount: SG;
    function GetRequiredArgumentCount: SG;
    function PreviewTableArgument(const AArgument: TCustomArgument): TRow;
    function PreviewTableArgumentValue(const AArgument: TCustomArgument): TRow;
    function Get(const Index: TIndex): TCustomArgument;
    function GetDefinedCount: SG;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Parse; overload;
    procedure Parse(const ACommandLine: string); overload; virtual;
    function FindByString(const AArgumentShortcut: string): TCustomArgument;

    procedure PreviewToConsole;
    procedure PreviewValuesToConsole;
    function PreviewAsString: string;
    procedure WriteUnused;
    function ShowUnused: string;
    function ShowRequired: string;
    function Check: string;

    procedure Add(const ACustomArgument: TCustomArgument);
    property ExistsCount: SG read GetExistsCount;
    property RequiredArgumentCount: SG read GetRequiredArgumentCount;

		property Items[const Index: TIndex]: TCustomArgument read Get; default; // operator []
    property DefinedCount: SG read GetDefinedCount;
  end;

implementation

uses
  SysUtils,
  Classes,
  uStartState,
  uTextAlignment,
  uTable,
  uFiles,
  uMsg,
  uChar,
  uStrings;

{ TArguments }

procedure TArguments.Add(const ACustomArgument: TCustomArgument);
begin
  FArguments.Add(ACustomArgument);
end;

function TArguments.Check: string;
var
  i: SG;
begin
  for i := 0 to FArguments.Count - 1 do
  begin
    Result := Result + TCustomArgument(FArguments[i]).Check;
  end;
end;

procedure TArguments.Clear;
var
  i: SG;
begin
  for i := 0 to FArguments.Count - 1 do
  begin
    TCustomArgument(FArguments[i]).Exists := False;
    TCustomArgument(FArguments[i]).Used := False;
  end;
end;

constructor TArguments.Create;
begin
  FArguments := TObjectList.Create;
  FArguments.OwnsObjects := False;
end;

destructor TArguments.Destroy;
begin
  FArguments.Free;

  inherited;
end;

function TArguments.FindByString(const AArgumentShortcut: string): TCustomArgument;
var
  i: SG;
begin
  for i := 0 to FArguments.Count - 1 do
  begin
    if SameText(TCustomArgument(FArguments[i]).Shortcut, AArgumentShortcut) then
    begin
      Result := TCustomArgument(FArguments[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TArguments.Get(const Index: TIndex): TCustomArgument;
begin
  Result := TCustomArgument(FArguments[Index]);
end;

function TArguments.GetDefinedCount: SG;
begin
  Result := FArguments.Count;
end;

function TArguments.GetExistsCount: SG;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to FArguments.Count - 1 do
  begin
    if TCustomArgument(FArguments[i]).ExistsNoUsed then // Do not use property Exists because property Used should stay unchanged
    begin
      Inc(Result);
    end;
  end;
end;

function TArguments.GetRequiredArgumentCount: SG;
var
  i: SG;
begin
  Result := 0;
  for i := 0 to FArguments.Count - 1 do
  begin
    if TCustomArgument(FArguments[i]).RequireCheck = rcRequired then
    begin
      Inc(Result);
    end;
  end;
end;

function TArguments.ParseString(ASource: string): TArrayOfStringPair;
var
	i: SG;
	EndIndex: SG;
	ResultCount: SG;
  ExpectedArgumentName: BG;
begin
	ResultCount := 0;
	i := 1;

  ExpectedArgumentName := True;
	while i <= Length(ASource) do
	begin
    // Argument name prefix, argument value must be quoted if starts with - or /
    if CharInSet(ASource[i], ['-', '/']) then
    begin
      ExpectedArgumentName := True;
      Inc(i);
      Continue;
    end;

    // Skip space
		if ASource[i] = ' ' then
		begin
			Inc(i);
			Continue;
		end;

		if ASource[i] = '"' then
		begin
			Inc(i);
			EndIndex := i;
			while True do
			begin
				EndIndex := PosEx('"', ASource, EndIndex);
				if CharAt(ASource, EndIndex - 1) = '\' then
				begin
          // \" -> "
					Delete(ASource, EndIndex - 1, 1);
				end
				else
					Break;
			end;
		end
		else
		begin
			EndIndex := i + 1;
      if ExpectedArgumentName then
        ReadToChars(ASource, EndIndex, [':', ' '])
      else
        ReadToChar(ASource, EndIndex, CharSpace);
      Dec(EndIndex);
		end;

    if ExpectedArgumentName then
    begin
      SetLength(Result, ResultCount + 1);
  		Result[ResultCount].Name := Copy(ASource, i, EndIndex - i);
  		Inc(ResultCount);
      ExpectedArgumentName := False;
    end
    else
    begin
  		Result[ResultCount - 1].Value := Copy(ASource, i, EndIndex - i);
      ExpectedArgumentName := True;
    end;

		i := EndIndex + 1;
	end;
end;

procedure TArguments.Parse(const ACommandLine: string);
var
	Arguments: TArrayOfStringPair;
  CommandLinePair: TStringPair;
begin
	CommandLinePair := SplitCommandLine(ACommandLine);
	Arguments := ParseString(CommandLinePair.Value);
  ApplyArguments(Arguments);
end;

procedure TArguments.Parse;
begin
  Parse(TStartState.CommandLine);
end;

procedure TArguments.ApplyArguments(const AArguments: TArrayOfStringPair);
var
  Index: SG;
  Name: string;
  Argument: TCustomArgument;
begin
  Index := 0;
  while Index < Length(AArguments) do
  begin
    Name := AArguments[Index].Name;

    Argument := FindByString(Name);
    if (Argument = nil) then
    begin
      raise EArgumentException.Create('Unknown command line argument ' + QuotedStr(Name) + '.');
    end;
    Argument.Exists := True;

    Argument.SetValueFromString(AArguments[Index].Value);
    Inc(Index);
  end;
end;

function TArguments.PreviewAsString: string;
const
	LineWidth = 16;
var
  i: SG;
begin
	Result := 'Parameter' + CharSpace + 'Description' + LineSep;
	Result := Result + string(StringOfChar(CharEmDash, LineWidth)) + LineSep;
  for i := 0 to FArguments.Count - 1 do
  begin
    Result := Result + TCustomArgument(FArguments[i]).Preview;
  end;
end;

procedure TArguments.PreviewToConsole;
var
  Table: TTable;
  Row: TRow;
  i: SG;
begin
  Table := TTable.Create(1 + FArguments.Count);
  try
    Row := TRow.Create(3);
    Row.Columns[0].Text := 'Parameter';
    Row.Columns[0].HorizontalAlignment := haCenter;
    Row.Columns[1].Text := 'Description';
    Row.Columns[1].HorizontalAlignment := haCenter;
    Row.Columns[2].Text := 'Required';
    Row.Columns[2].HorizontalAlignment := haCenter;
    Table.Data[0] := Row;

    for i := 0 to FArguments.Count - 1 do
    begin
      Row := PreviewTableArgument(TCustomArgument(FArguments[i]));
      Table.Data[i + 1] := Row;
    end;
    Table.WriteToConsole;
  finally
    Table.Free;
  end;
end;

procedure TArguments.PreviewValuesToConsole;
var
  Table: TTable;
  Row: TRow;
  i: SG;
begin
  Table := TTable.Create(1 + FArguments.Count);
  try
    Row := TRow.Create(2);
    Row.Columns[0].Text := 'Parameter';
    Row.Columns[0].HorizontalAlignment := haCenter;
    Row.Columns[1].Text := 'Value';
    Row.Columns[1].HorizontalAlignment := haCenter;
    Table.Data[0] := Row;

    for i := 0 to FArguments.Count - 1 do
    begin
      Row := PreviewTableArgumentValue(TCustomArgument(FArguments[i]));
      Table.Data[i + 1] := Row;
    end;
    Table.WriteToConsole;
  finally
    Table.Free;
  end;
end;

function TArguments.PreviewTableArgument(const AArgument: TCustomArgument): TRow;
var
  Row: TRow;
begin
  Row := TRow.Create(3);
  Row.Columns[0].Text := AArgument.GetArgumentShortcutAndSyntax;
  Row.Columns[1].Text := AArgument.Description;
  Row.Columns[2].Text := AArgument.GetRequired + AArgument.GetRequireList;
  Result := Row;
end;

function TArguments.PreviewTableArgumentValue(const AArgument: TCustomArgument): TRow;
var
  Row: TRow;
begin
  Row := TRow.Create(2);
  Row.Columns[0].Text := AArgument.Shortcut;
  Row.Columns[1].Text := AArgument.GetValueAsString;
  Result := Row;
end;

function TArguments.ShowRequired: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to FArguments.Count - 1 do
  begin
    if (TCustomArgument(FArguments[i]).RequiredNotFound) then
    begin
      Result := Result + LineSep + '  Argument "' + TCustomArgument(FArguments[i]).Shortcut + '" required';
    end;
  end;
end;

function TArguments.ShowUnused: string;
var
  Argument: TCustomArgument;
  i: SG;
begin
  Result := '';
  for i := 0 to FArguments.Count - 1 do
  begin
    Argument := TCustomArgument(FArguments[i]);
    if ((not Argument.Used) and (Argument.RequireCheck = rcRequired)) then
    begin
      Result := Result + LineSep + '  Argument ''' + Argument.Shortcut + ''' from command line is not used in program';
    end;
  end;
end;

procedure TArguments.WriteUnused;
var
  Text: string;
begin
  Text := ShowUnused;
  if Text <> '' then
    Warning(ShowUnused);
end;

end.
