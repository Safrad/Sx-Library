unit uNumericArgument;

interface

uses
  uTypes,
  uNumericalSet,
  uStringArgument;

type
	TNumericArgument = class(TStringArgument)
	private
    FValue: S8;
    FNumericalSet: TNumericalSet;
    procedure SetValue(const Value: S8);
    function GetValue: S8;
    procedure SetNumericalSet(const Value: TNumericalSet);
  protected
    function GetSyntax: string; override;
    property NumericalSet: TNumericalSet read FNumericalSet write SetNumericalSet;
  public
    procedure SetValueFromString(const AValue: string); override;
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

procedure TNumericArgument.SetNumericalSet(const Value: TNumericalSet);
begin
  FNumericalSet := Value;
end;

procedure TNumericArgument.SetValue(const Value: S8);
begin
  FValue := Value;
end;

procedure TNumericArgument.SetValueFromString(const AValue: string);
begin
  FValue := StrToInt64(AValue);

  if not FNumericalSet.Contains(FValue) then
  begin
    raise EArgumentOutOfRangeException.Create('Invalid argument ''' + Shortcut + ''' value ''' + IntToStr(FValue) + '''. ' + FNumericalSet.Description + ' expected.');
  end;
end;

end.
