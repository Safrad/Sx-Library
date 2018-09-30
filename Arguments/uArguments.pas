unit uArguments;

interface

uses
  Contnrs,
  uTypes,
  uRow,
  uCustomArgument;

type
  TArguments = class
  private
    FArguments: TObjectList;

    procedure ParseArguments(const AArguments: TArrayOfString);
    function FindByString(const AArgumentShortcut: string): TCustomArgument;

    function GetCount: SG;
    function GetRequiredArgumentCount: SG;
    function PreviewTableArgument(const AArgument: TCustomArgument): TRow;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse; overload;
    procedure Parse(const ACommandLine: string); overload; virtual;
    procedure PreviewToConsole;
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
  uSwitchArgument,
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
  // TODO : optimize
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
	Arguments: TArrayOfString;
  Remain: string;
begin
	Arguments := SplitStr(ACommandLine, 1024, Remain);
  ParseArguments(Arguments);
end;

procedure TArguments.Parse;
begin
  Parse(GetCommandLine);
end;

procedure TArguments.ParseArguments(const AArguments: TArrayOfString);
var
  Index: SG;
  Name: string;
  Argument: TCustomArgument;
begin
  Index := 1; // Skip executable file name
  while Index < Length(AArguments) do
  begin
    Name := AArguments[Index];
    if CharInSet(FirstChar(Name), ['-', '/']) then
    begin
      Delete(Name, 1, 1);
    end;

    Argument := FindByString(Name);
    if (Argument = nil) then
    begin
      raise EArgumentException.Create('Unknown command line argument ''' + Name + '''.');
    end;
    Argument.Exists := True;
    Inc(Index);

    if (not (Argument is TSwitchArgument)) then
    begin
      if Index >= Length(AArguments) then
      begin
        raise EArgumentException.Create('Argument value expected.');
      end;
      Argument.SetValueFromString(AArguments[Index]);
      Inc(Index);
    end;
  end;
end;

function TArguments.PreviewAsString: string;
const
	LineWidth = 16;
var
  i: SG;
begin
	Result := 'Parameter' + CharSpace + 'Description' + LineSep;
	Result := Result + StringOfChar(CharEmDash, LineWidth) + LineSep;
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
