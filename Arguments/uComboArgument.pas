unit uComboArgument;

interface

uses
  uTypes,
  uCustomArgument;

type
  TComboArgument = class(TCustomArgument)
  private
    FCaptions: TArrayOfString;
    FValue: SG;
    FDefaultValue: SG;
    procedure SetCaptions(const Value: TArrayOfString);
    procedure SetValue(const Value: SG);
    procedure SetDefaultValue(const Value: SG);
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    function GetValueAsString: string; override;
    procedure SetValueFromString(const AValue: string); override;
    procedure AddCaption(const ACaption: string);

    property Captions: TArrayOfString read FCaptions write SetCaptions;
    property DefaultValue: SG read FDefaultValue write SetDefaultValue;
    property Value: SG read FValue write SetValue;
  end;

implementation

uses
  SysUtils,

  uStrings,
  uEParseError;

{ TComboArgument }

procedure TComboArgument.AddCaption(const ACaption: string);
begin
  SetLength(FCaptions, Length(FCaptions) + 1);
  FCaptions[Length(FCaptions) - 1] := ACaption;
end;

constructor TComboArgument.Create;
begin
  inherited;

  FValue := -1;
end;

function TComboArgument.GetSyntax: string;
var
  i: SG;
begin
  Result := '[';
  for i := 0 to Length(FCaptions) - 1 do
    Result := Result + FCaptions[i] + '|';
  Result := Result + ']';
end;

function TComboArgument.GetValueAsString: string;
begin
  if Value = -1 then
    Result := NAStr
  else
    Result := FCaptions[Value];
end;

procedure TComboArgument.SetCaptions(const Value: TArrayOfString);
begin
  FCaptions := Value;
end;

procedure TComboArgument.SetDefaultValue(const Value: SG);
begin
  FDefaultValue := Value;
end;

procedure TComboArgument.SetValue(const Value: SG);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TComboArgument.SetValueFromString(const AValue: string);
var
  i: SG;
begin
  inherited;

  for i := 0 to Length(FCaptions) - 1 do
  begin
    if SameText(AValue, FCaptions[i]) then
    begin
      Value := i;
      Exit;
    end;
  end;

  raise EParseError.Create('Invalid argument ' + QuotedStr(Shortcut) + ' value, ', FCaptions, AValue);
end;

end.
