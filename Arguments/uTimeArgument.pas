unit uTimeArgument;

interface

uses
  uTypes,
  uTimeSpan,
  uCustomArgument;

type
  TTimeArgument = class(TCustomArgument)
  private
    FValue: TTimeSpan;
    FDefaultValue: TTimeSpan;
    function GetValue: TTimeSpan;
    procedure SetValue(const Value: TTimeSpan);
    procedure SetDefaultValue(const Value: TTimeSpan);
  protected
    function GetSyntax: string; override;
  public
    procedure SetValueFromString(const AValue: string); override;

    property DefaultValue: TTimeSpan read FDefaultValue write SetDefaultValue;
    property Value: TTimeSpan read GetValue write SetValue;
  end;

implementation

uses
  uInputFormat;

{ TTimeArgument }

function TTimeArgument.GetSyntax: string;
begin
  Result := '<time>';
end;

function TTimeArgument.GetValue: TTimeSpan;
begin
  Used := True;
  Result := FValue;
end;

procedure TTimeArgument.SetDefaultValue(const Value: TTimeSpan);
begin
  FDefaultValue := Value;
end;

procedure TTimeArgument.SetValue(const Value: TTimeSpan);
begin
  FValue := Value;
end;

procedure TTimeArgument.SetValueFromString(const AValue: string);
begin
  Value.Milliseconds := SToMs(AValue, ifIO);
end;

end.
