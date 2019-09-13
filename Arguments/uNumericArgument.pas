unit uNumericArgument;

interface

uses
  uTypes,
  uNumericalSet,
  uCustomArgument;

type
	TNumericArgument = class(TCustomArgument)
	private
    FValue: S8;
    FNumericalSet: TNumericalSet;
    FDefaultValue: S8;
    procedure SetValue(const Value: S8);
    function GetValue: S8;
    procedure SetNumericalSet(const Value: TNumericalSet);
    procedure SetDefaultValue(const Value: S8);
  protected
    function GetSyntax: string; override;
    property NumericalSet: TNumericalSet read FNumericalSet write SetNumericalSet;
  public
    function IsDefault: BG; override;
    function GetValueAsString: string; override;
    procedure SetValueFromString(const AValue: string); override;

    property DefaultValue: S8 read FDefaultValue write SetDefaultValue;
    property Value: S8 read GetValue write SetValue;
  end;

implementation

uses SysUtils;

{ TNumericArgument }

function TNumericArgument.GetSyntax: string;
begin
  Result := '<' + NumericalSet.Description + '>';
end;

function TNumericArgument.GetValue: S8;
begin
  Used := True;
  Result := FValue;
end;

function TNumericArgument.GetValueAsString: string;
begin
  Result := IntToStr(Value);
end;

function TNumericArgument.IsDefault: BG;
begin
  Result := FValue = FDefaultValue;
end;

procedure TNumericArgument.SetDefaultValue(const Value: S8);
begin
  FDefaultValue := Value;
end;

procedure TNumericArgument.SetNumericalSet(const Value: TNumericalSet);
begin
  FNumericalSet := Value;
end;

procedure TNumericArgument.SetValue(const Value: S8);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TNumericArgument.SetValueFromString(const AValue: string);
begin
  Value := StrToInt64(AValue);

  if not FNumericalSet.Contains(FValue) then
  begin
    raise EArgumentOutOfRangeException.Create('Invalid argument ''' + Shortcut + ''' value ''' + IntToStr(FValue) + '''. ' + FNumericalSet.Description + ' expected.');
  end;
end;

end.
