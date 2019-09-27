unit uColorArgument;

interface

uses
  uTypes,
  uColor,
  uCustomArgument;

type
  TColorArgument = class(TCustomArgument)
  private
    FValue: TRGBA;
    FDefaultValue: TRGBA;
    function GetValue: TRGBA;
    procedure SetValue(const Value: TRGBA);
    procedure SetDefaultValue(const Value: TRGBA);
  protected
    function GetSyntax: string; override;
  public
    function IsDefault: BG; override;
    procedure SetDefault; override;

    function GetValueAsString: string; override;
    procedure SetValueFromString(const AValue: string); override;

    property DefaultValue: TRGBA read FDefaultValue write SetDefaultValue;
    property Value: TRGBA read GetValue write SetValue;
  end;

implementation

uses
  uInputFormat,
  uStrings;

{ TColorArgument }

function TColorArgument.GetSyntax: string;
begin
  Result := '<color>';
end;

function TColorArgument.GetValue: TRGBA;
begin
  Used := True;
  Result := FValue;
end;

function TColorArgument.GetValueAsString: string;
begin
  Result := ColorToHTMLString(Value.L);
end;

function TColorArgument.IsDefault: BG;
begin
  Result := FValue.C = FDefaultValue.C;
end;

procedure TColorArgument.SetDefault;
begin
  inherited;

  FValue.C := FDefaultValue.C;
end;

procedure TColorArgument.SetDefaultValue(const Value: TRGBA);
begin
  FDefaultValue := Value;
end;

procedure TColorArgument.SetValue(const Value: TRGBA);
begin
  FValue := Value;
end;

procedure TColorArgument.SetValueFromString(const AValue: string);
begin
  FValue.L := StrToValS8(ReplaceF(AValue, '#', '$'), True, MinInt, FValue.L, High(U4), 1, nil);
end;

end.
