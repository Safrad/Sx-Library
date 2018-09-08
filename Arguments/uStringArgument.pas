unit uStringArgument;

interface

uses
  uCustomArgument;

type
	TStringArgument = class(TCustomArgument)
  private
    procedure SetValue(const Value: string);
    function GetValue: string;
  protected
    FValue: string;
    function GetSyntax: string; override;
  public
    procedure SetValueFromString(const AValue: string); override;
    property Value: string read GetValue write SetValue;
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

procedure TStringArgument.SetValue(const Value: string);
begin
  FValue := Value;
end;

procedure TStringArgument.SetValueFromString(const AValue: string);
begin
  inherited;

  FValue := AValue;
end;

end.
