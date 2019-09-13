unit uStringArgument;

interface

uses
  uTypes,
  uCustomArgument;

type
	TStringArgument = class(TCustomArgument)
  private
    FDefaultValue: string;
    procedure SetValue(const Value: string);
    function GetValue: string;
    procedure SetDefaultValue(const Value: string);
  protected
    FValue: string;
    function GetSyntax: string; override;
  public
    function IsDefault: BG; override;
    function GetValueAsString: string; override;
    procedure SetValueFromString(const AValue: string); override;

    property Value: string read GetValue write SetValue;
    property DefaultValue: string read FDefaultValue write SetDefaultValue;
  end;

implementation

{ TStringArgument }

function TStringArgument.GetSyntax: string;
begin
  Result := '<text>';
end;

function TStringArgument.GetValue: string;
begin
  Used := True;
  Result := FValue;
end;

function TStringArgument.GetValueAsString: string;
begin
  Result := Value;
end;

function TStringArgument.IsDefault: BG;
begin
  Result := FValue = FDefaultValue;
end;

procedure TStringArgument.SetDefaultValue(const Value: string);
begin
  FDefaultValue := Value;
end;

procedure TStringArgument.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TStringArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  Value := AValue;
end;

end.
