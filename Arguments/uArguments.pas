unit uArguments;

interface

uses
  Contnrs,
  uTypes,
  uDParser,
  uRow,
  uCustomArgument;

type
  TArguments = class
  private
    FArguments: TObjectList;

    procedure ParseArgument(const AParser: TDParser);
    procedure ParseArguments(const AParser: TDParser);
    function FindByString(const AArgumentShortcut: string): TCustomArgument;

    function GetCount: SG;
    function GetRequiredArgumentCount: SG;
    function PreviewTableArgument(const AArgument: TCustomArgument): TRow;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse; overload;
    procedure Parse(const ACommandLine: string); overload; virtual;
    procedure PreviewTable;
    function PreviewAsString: string;
    procedure WriteUnused;
    function ShowUnused: string;
    function ShowRequired: string;
    function Check: string;

    procedure Add(const ACustomArgument: TCustomArgument);
    property Count: SG read GetCount;
    property RequiredArgumentCount: SG read GetRequiredArgumentCount;
  end;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  uSwitchArgument,
  uTable,
  uMsg,
  uChar,
  uStrings,
  uConsole;

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

constructor TArguments.Create;
begin
  FArguments := TObjectList.Create;
  FArguments.OwnsObjects := True;
end;

destructor TArguments.Destroy;
begin
  FArguments.Free;

  inherited;
end;

function TArguments.FindByString(
  const AArgumentShortcut: string): TCustomArgument;
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

function TArguments.GetCount: SG;
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

procedure TArguments.Parse(const ACommandLine: string);
var
  Parser: TDParser;
begin
  Parser := TDParser.Create(ACommandLine);
  try
    ParseArguments(Parser);
  finally
    Parser.Free;
  end;
end;


procedure TArguments.Parse;
begin
  Parse(GetCommandLine);
end;

procedure TArguments.ParseArgument(const AParser: TDParser);
var
  Name: string;
  Argument: TCustomArgument;
  Value: string;
begin
  AParser.ReadInput;

  AParser.ReadToChar(' ');
  Name := AParser.Id;
  if (Name = '') then
    Exit; // TODO : Hotfix
  if (FirstChar(Name) = '-') then
  begin
    Delete(Name, 1, 1);
  end;

  Argument := FindByString(Name);
  if (Argument = nil) then
  begin
    //raise InvalidArgumentException.Create;
    raise Exception.Create('Unknown command line argument ''' + Name + '''.');
  end;
  Argument.Exists := True;
  if (not (Argument is TSwitchArgument)) then
  begin
    AParser.ReadToChar(' ');
    Value := AParser.Id;
    // TODO : "Program Files"

    Argument.SetValueFromString(Value);
  end;
end;

procedure TArguments.ParseArguments(const AParser: TDParser);
begin
  AParser.ReadToChar(' '); // Skip executable file name

  while (AParser.InputType <> itEOI) do
  begin
    ParseArgument(AParser);
  end;
end;

function TArguments.PreviewAsString: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to FArguments.Count - 1 do
  begin
    Result := Result + TCustomArgument(FArguments[i]).Preview;
  end;
end;

procedure TArguments.PreviewTable;
var
  Table: TTable;
  Row: TRow;
  i: SG;
begin
  Table := TTable.Create(1 + FArguments.Count);
  try
    Row := TRow.Create(3);
    Row.Columns[0].Text := 'Parameter';
    Row.Columns[0].HorizontalAlign := taCenter;
    Row.Columns[1].Text := 'Description';
    Row.Columns[1].HorizontalAlign := taCenter;
    Row.Columns[2].Text := 'Required';
    Row.Columns[2].HorizontalAlign := taCenter;
    Table.Data[0] := Row;

    for i := 0 to FArguments.Count - 1 do
    begin
      Row := PreviewTableArgument(TCustomArgument(FArguments[i]));
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

function TArguments.PreviewTableArgument(
  const AArgument: TCustomArgument): TRow;
var
  Row: TRow;
begin
  Row := TRow.Create(3);
  Row.Columns[0].Text := AArgument.GetArgumentShortcutAndSyntax;
  Row.Columns[1].Text := AArgument.Description;
  Row.Columns[2].Text := AArgument.GetRequiredOrOptional + AArgument.GetRequireList;
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
      Result := Result + 'Error: Argument "' + TCustomArgument(FArguments[i]).Shortcut + '" required' + LineSep;
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
      Result := Result + 'Warning: Argument ''' + Argument.Shortcut + ''' from command line is not used in program' + LineSep;
    end;
  end;
end;

procedure TArguments.WriteUnused;
begin
  TConsole.Write(ShowUnused, ccYellow);
end;

end.
