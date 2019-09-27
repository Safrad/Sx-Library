unit uStringArrayArgument;

interface

uses
  uTypes,
  uCustomArgument;

type
  TStringArrayArgument = class(TCustomArgument)
  private
    FValues: TArrayOfString;
    FSeparator: string;
    FDefaultValues: TArrayOfString;
    procedure SetSeparator(const Value: string);
    function GetValues: TArrayOfString;
    procedure SetDefaultValues(const Value: TArrayOfString);
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    function IsDefault: BG; override;
    procedure SetDefault; override;

    function GetValueAsString: string; override;
    procedure SetValueFromString(const AValue: string); override;

    property Separator: string read FSeparator write SetSeparator;

    property DefaultValues: TArrayOfString read FDefaultValues write SetDefaultValues;
    property Values: TArrayOfString read GetValues;
  end;

implementation

uses
  uStrings;

{ TStringArrayArgument }

constructor TStringArrayArgument.Create;
begin
  inherited;

  FSeparator := ';';
end;

function TStringArrayArgument.GetSyntax: string;
begin
  Result := 'value1' + Separator + 'value2' + Separator + 'value3...';
end;

function TStringArrayArgument.GetValueAsString: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to Length(FValues) - 1 do
  begin
    AppendStr(Result, FValues[i]);
    if i < Length(Values) - 1 then
      AppendStr(Result, Separator);
  end;
end;

function TStringArrayArgument.GetValues: TArrayOfString;
begin
  Used := True;
  Result := FValues;
end;

function TStringArrayArgument.IsDefault: BG;
var
  i: SG;
begin
  Result := False;

  if Length(FValues) <> Length(FDefaultValues) then
  begin
    Exit;
  end;

  for i := 0 to Length(FValues) - 1 do
  begin
    if FValues[i] <> FDefaultValues[i] then
      Exit;
  end;

  Result := True;
end;

procedure TStringArrayArgument.SetDefault;
var
  i: SG;
begin
  inherited;

  SetLength(FValues, Length(FDefaultValues));
  for i := 0 to Length(FValues) - 1 do
  begin
    FValues[i] := FDefaultValues[i];
  end;
end;

procedure TStringArrayArgument.SetDefaultValues(const Value: TArrayOfString);
begin
  FDefaultValues := Value;
end;

procedure TStringArrayArgument.SetSeparator(const Value: string);
begin
  FSeparator := Value;
end;

procedure TStringArrayArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  FValues := SplitStringEx(AValue, Separator);
end;

end.
