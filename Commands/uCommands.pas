unit uCommands;

interface

uses
  Contnrs,
  uTypes,
  uDParser,
  uRow,
  uCustomCommand;

type
  TCommands = class
  private
    FCommands: TObjectList;

    function PreviewTableCommand(const ACommand: TCustomCommand): TRow;
  public
    constructor Create;
    destructor Destroy; override;

    function FindByString(const ACommandShortcut: string): TCustomCommand;
    function FindByStringException(const ACommandShortcut: string): TCustomCommand;

    procedure PreviewToConsole;
    function PreviewAsString: string;

    procedure Add(const ACustomCommand: TCustomCommand);
    property List: TObjectList read FCommands;
  end;

implementation

uses
  Windows,
  SysUtils,
  uTextAlignment,
  uTable,
  uChar,
  uStrings,
  uConsole;

{ TCommands }

procedure TCommands.Add(const ACustomCommand: TCustomCommand);
begin
  FCommands.Add(ACustomCommand);
end;

constructor TCommands.Create;
begin
  inherited;

  FCommands := TObjectList.Create;
  FCommands.OwnsObjects := True;
end;

destructor TCommands.Destroy;
begin
  FCommands.Free;

  inherited;
end;

function TCommands.FindByString(
  const ACommandShortcut: string): TCustomCommand;
var
  i: SG;
begin
  // TODO : optimize
  for i := 0 to FCommands.Count - 1 do
  begin
    if SameText(TCustomCommand(FCommands[i]).Shortcut, ACommandShortcut) then
    begin
      Result := TCustomCommand(FCommands[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TCommands.FindByStringException(const ACommandShortcut: string): TCustomCommand;
begin
  Result := FindByString(ACommandShortcut);
  if Result = nil then
    raise EArgumentException.Create('Unknown command: ' + ACommandShortcut);
end;

function TCommands.PreviewAsString: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to FCommands.Count - 1 do
  begin
    Result := Result + TCustomCommand(FCommands[i]).GetShortcutAndSyntax + ' ' + TCustomCommand(FCommands[i]).Description + LineSep;
  end;
end;

procedure TCommands.PreviewToConsole;
var
  Table: TTable;
  Row: TRow;
  i: SG;
begin
  Table := TTable.Create(1 + FCommands.Count);
  try
    Row := TRow.Create(2);
    Row.Columns[0].Text := 'Parameter';
    Row.Columns[0].HorizontalAlignment := haCenter;
    Row.Columns[1].Text := 'Description';
    Row.Columns[1].VerticalAlignment := vaCenter;
    Table.Data[0] := Row;

    for i := 0 to FCommands.Count - 1 do
    begin
      Row := PreviewTableCommand(TCustomCommand(FCommands[i]));
      Table.Data[i + 1] := Row;
    end;
    Table.WriteToConsole;
  finally
    Table.Free;
  end;
end;

function TCommands.PreviewTableCommand(const ACommand: TCustomCommand): TRow;
var
  Row: TRow;
begin
  Row := TRow.Create(2);
  Row.Columns[0].Text := ACommand.GetShortcutAndSyntax;
  Row.Columns[1].Text := ACommand.Description;
  Result := Row;
end;

end.
