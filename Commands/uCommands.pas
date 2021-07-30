unit uCommands;

interface

uses
  Generics.Collections,

  uTypes,
  uRow,
  uCustomCommand;

type
  TCommandsList = TObjectList<TCustomCommand>;

  /// <summary>
  /// <see>https://en.wikipedia.org/wiki/Command_pattern</see>
  /// </summary>
  TCommands = class
  private
    FChanged: BG;
    FCommandIndexes: TArrayOfSG;
    FCommandNamesSorted: TArrayOfString;

    FCommands: TCommandsList;

    function PreviewTableCommand(const ACommand: TCustomCommand): TRow;
    procedure SortCommands;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function FindByString(const ACommandShortcut: string): TCustomCommand;
    function FindByStringException(const ACommandShortcut: string): TCustomCommand;

    procedure WriteToCommonOutput;

    procedure Add(const ACustomCommand: TCustomCommand); overload;
    procedure Add(const ACommands: TCommandsList); overload;
    procedure Delete(const ACustomCommand: TCustomCommand);
    procedure Disable(const ACustomCommand: TCustomCommand);

    property List: TCommandsList read FCommands;
  end;

implementation

uses
  SysUtils,
  Classes,

  uUnsupportedCommand,
  uTextAlignment,
  uTable,
  uITable,
  uMath,
  uStrings,
  uSorts,
  uFind,
  uConsoleColor,
  uEParseError,
  uOutputFormat,
  uCommonOutput;

{ TCommands }

procedure TCommands.Add(const ACustomCommand: TCustomCommand);
begin
  FCommands.Add(ACustomCommand);
  FChanged := True;
end;

procedure TCommands.Add(const ACommands: TCommandsList);
var
  i: SG;
begin
  for i := 0 to ACommands.Count - 1 do
  begin
    FCommands.Add(TCustomCommand(ACommands[i]));
    FChanged := True;
  end;
end;

procedure TCommands.Clear;
begin
  FChanged := False;
  SetLength(FCommandIndexes, 0);
  SetLength(FCommandNamesSorted, 0);
  FCommands.Clear;
end;

constructor TCommands.Create;
begin
  inherited;

  FCommands := TCommandsList.Create;
  FCommands.OwnsObjects := True;
end;

destructor TCommands.Destroy;
begin
  try
    Clear;
    FCommands.Free;
  finally
    inherited;
  end;
end;

function TCommands.FindByString(
  const ACommandShortcut: string): TCustomCommand;
var
  FromV, ToV: SG;
begin
  if FChanged then
    SortCommands;

	if not FindS(FCommandNamesSorted, LowerCase(ACommandShortcut), FromV, ToV) then
		Result := nil
  else
    Result := TCustomCommand(FCommands[FCommandIndexes[FromV]]);
end;

function TCommands.FindByStringException(const ACommandShortcut: string): TCustomCommand;
begin
  Result := FindByString(ACommandShortcut);
  if Result = nil then
		raise EParseError.Create(['Valid command'], ACommandShortcut);
  if not Result.Enabled then
		raise EParseError.Create(['Enabled command'], ACommandShortcut);
end;

procedure TCommands.WriteToCommonOutput;
var
  Table: ITable;
  Row: TRow;
  i: SG;
begin
  Table := TTable.Create;
  try
    Row := THeaderRow.Create(3);
    try
      Row.Columns[0].Text := 'Command name and parameters';
      Row.Columns[0].HorizontalAlignment := haCenter;
      Row.Columns[1].Text := 'Description';
      Row.Columns[1].VerticalAlignment := vaCenter;
      Row.Columns[2].Text := 'Used';
      Row.Columns[2].VerticalAlignment := vaCenter;
      TTable(Table).AddHeaderRow(Row);
    finally
      Row.Free;
    end;

    for i := 0 to FCommands.Count - 1 do
    begin
      Row := PreviewTableCommand(TCustomCommand(FCommands[i]));
      Table.AddRow(Row);
    end;
    CommonOutput.AddTable(TTable(Table));
  finally
    Table := nil;
  end;
end;

procedure TCommands.SortCommands;
var
	i: SG;
  FCommandNamesSorted2: TArrayOfString;
begin
  FChanged := False;
  SetLength(FCommandNamesSorted, 0);
  SetLength(FCommandNamesSorted2, 0);
  SetLength(FCommandIndexes, FCommands.Count);
  SetLength(FCommandNamesSorted2, FCommands.Count);
	FillOrderUG(FCommandIndexes[0], Length(FCommandIndexes));

	for i := 0 to FCommands.Count - 1 do
	begin
		FCommandNamesSorted2[i] := LowerCase(DelCharsF(TCustomCommand(FCommands[i]).Shortcut, CharSpace));
	end;
	SortStrBinary(PArraySG(@FCommandIndexes[0]), PArrayString(@FCommandNamesSorted2[0]), Length(FCommandIndexes));

  SetLength(FCommandNamesSorted, Length(FCommandIndexes));
	for i := 0 to Length(FCommandIndexes) - 1 do
	begin
		FCommandNamesSorted[i] := FCommandNamesSorted2[FCommandIndexes[i]];
	end;
end;

procedure TCommands.Delete(const ACustomCommand: TCustomCommand);
var
  Index: SG;
begin
  Index := FCommands.IndexOf(ACustomCommand);
  if Index >= 0 then
  begin
    FChanged := True;
    FCommands.Delete(Index);
  end
  else
    raise EArgumentException.Create('Can not delete command "' + ACustomCommand.Shortcut + '" because not found in command list.');
end;

procedure TCommands.Disable(const ACustomCommand: TCustomCommand);
var
  Index: SG;
begin
  Index := FCommands.IndexOf(ACustomCommand);
  if Index >= 0 then
    ACustomCommand.Enabled := False
  else
    raise EArgumentException.Create('Can not disable command "' + ACustomCommand.Shortcut + '" because not found in command list.');
end;

function TCommands.PreviewTableCommand(const ACommand: TCustomCommand): TRow;
var
  Row: TRow;
begin
  Row := TRow.Create(3);
  Row.Columns[0].Text := ACommand.GetShortcutAndSyntax;
  Row.Columns[1].Text := ACommand.Description;
  Row.Columns[2].Text := NToS(ACommand.ExecuteCount);
  if (not ACommand.Enabled) or (ACommand is TUnsupportedCommand) then
  begin
    Row.Columns[0].TextColor := ccGray;
    Row.Columns[1].TextColor := ccGray;
    Row.Columns[2].TextColor := ccGray;
  end;

  Result := Row;
end;

end.
