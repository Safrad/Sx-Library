unit uStringArgument;

interface

uses
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
