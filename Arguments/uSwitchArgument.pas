unit uSwitchArgument;

interface

uses
  uTypes,
  uCustomArgument;

type
  TSwitchArgument = class(TCustomArgument)
  private
    FValue: BG;
    procedure SetValue(const Value: BG);
  protected
		function GetSyntax: string; override;
  public
    constructor Create;
		procedure SetValueFromString(const AValue: string); override;
    function GetRequired: string; override;
    function GetRequiredOrOptional: string; override;
    property Value: BG read FValue write SetValue;
  end;

implementation

uses
  SysUtils,
  uEParseError;

{ TSwitchArgument }

constructor TSwitchArgument.Create;
begin
  inherited;

  RequireCheck := rcOptional;
end;

function TSwitchArgument.GetRequired: string;
begin
  Result := '';
end;

function TSwitchArgument.GetRequiredOrOptional: string;
begin
  Result := '';
end;

function TSwitchArgument.GetSyntax: string;
begin
  Result := '';
end;

procedure TSwitchArgument.SetValue(const Value: BG);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TSwitchArgument.SetValueFromString(const AValue: string);
var
  AUpperCaseValue: string;
begin
//  raise EArgumentException.Create('Argument ' + QuotedStr(Shortcut) + ' is switch, no value is expected.');
  AUpperCaseValue := UpperCase(AValue);
  if (AUpperCaseValue = 'FALSE') or (AUpperCaseValue = '0') then
    Value := False
  else if (AUpperCaseValue = 'TRUE') or (AUpperCaseValue = '1') then
    Value := True
  else
    raise EParseError.Create('Argument ' + QuotedStr(Shortcut) + ' is switch, ', ['{no value}', 'false', 'true', '0', '1'], AValue);
end;

end.
