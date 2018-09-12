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

    function FindByString(const ACommandShortcut: string): TCustomCommand;

    function PreviewTableCommand(const ACommand: TCustomCommand): TRow;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(const AText: string);
    procedure PreviewTable;
    function PreviewAsString: string;

    procedure Add(const ACustomCommand: TCustomCommand);
  end;

implementation

uses
  Windows,
  SysUtils,
  Classes,
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

procedure TCommands.Parse(const AText: string);
var
  CommandAsText: string;
  Command: TCustomCommand;
  InLineIndex: SG;
begin
  InLineIndex := 1;
  while InLineIndex <= Length(AText) do
  begin
    CommandAsText := ReadToChars(AText, InLineIndex, [CharSpace, CharCR]);
    Command := FindByString(CommandAsText);
    if Command = nil then
      raise EArgumentException.Create('Unknown command: ' + CommandAsText);

    Command.Execute(ReadToNewLine(AText, InLineIndex));
  end;
end;

function TCommands.PreviewAsString: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to FCommands.Count - 1 do
  begin
    Result := Result + TCustomCommand(FCommands[i]).GetShortcutAndSyntax + LineSep;
  end;
end;

procedure TCommands.PreviewTable;
var
  Table: TTable;
  Row: TRow;
  i: SG;
begin
  Table := TTable.Create(1 + FCommands.Count);
  try
    Row := TRow.Create(2);
    Row.Columns[0].Text := 'Parameter';
    Row.Columns[0].HorizontalAlign := taCenter;
    Row.Columns[1].Text := 'Description';
    Row.Columns[1].HorizontalAlign := taCenter;
    Table.Data[0] := Row;

    for i := 0 to FCommands.Count - 1 do
    begin
      Row := PreviewTableCommand(TCustomCommand(FCommands[i]));
      Table.Data[i + 1] := Row;
    end;
    {$ifdef Console}
    Table.WriteToConsole;
    {$else}
    // TODO
    {$endif}
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
