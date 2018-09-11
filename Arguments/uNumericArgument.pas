unit uNumericArgument;

interface

uses
  uTypes,
  uStringArgument;

type
	TNumericArgument = class(TStringArgument)
	private
    FValue: S8;
    FMinimalValue: S8;
    FMaximalValue: S8;
    procedure SetValue(const Value: S8);
    function GetValue: S8;
    procedure SetMaximalValue(const Value: S8);
    procedure SetMinimalValue(const Value: S8);
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure SetValueFromString(const AValue: string); override;
    property Value: S8 read GetValue write SetValue;
    property MinimalValue: S8 read FMinimalValue write SetMinimalValue;
    property MaximalValue: S8 read FMaximalValue write SetMaximalValue;
  end;

implementation

uses SysUtils;

{ TNumericArgument }

constructor TNumericArgument.Create;
begin
  inherited;

  FMinimalValue := Low(FMinimalValue);
  FMaximalValue := High(FMaximalValue);
end;

function TNumericArgument.GetSyntax: string;
begin
  Result := '<number>';
end;

function TNumericArgument.GetValue: S8;
begin
  Used := True;
  Result := FValue;
end;

procedure TNumericArgument.SetMaximalValue(const Value: S8);
begin
  FMaximalValue := Value;
end;

procedure TNumericArgument.SetMinimalValue(const Value: S8);
begin
  FMinimalValue := Value;
end;

procedure TNumericArgument.SetValue(const Value: S8);
begin
  FValue := Value;
end;

procedure TNumericArgument.SetValueFromString(const AValue: string);
begin
  FValue := StrToInt64(AValue);

  if (FValue < FMinimalValue) or (FValue > FMaximalValue) then
  begin
    raise EArgumentOutOfRangeException.Create('Argument ' + Shortcut + ' value ' + IntToStr(FValue) + ' out of range [' + IntToStr(FMinimalValue) + '..' + IntToStr(FMaximalValue) + '].');
  end;
end;

end.
