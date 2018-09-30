unit uTimeArgument;

interface

uses
  uTypes,
  uStringArgument;

type
  TTimeArgument = class(TStringArgument)
  private
    FValue: S8;
    function GetValue: S8;
    procedure SetValue(const Value: S8);
  protected
    function GetSyntax: string; override;
  public
    property Value: S8 read GetValue write SetValue;
    procedure SetValueFromString(const AValue: string); override;
  end;

implementation

uses
  uInputFormat;

{ TTimeArgument }

function TTimeArgument.GetSyntax: string;
begin
  Result := '<time>';
end;

function TTimeArgument.GetValue: S8;
begin
  Used := True;
  Result := FValue;
end;

procedure TTimeArgument.SetValue(const Value: S8);
begin
  FValue := Value;
end;

procedure TTimeArgument.SetValueFromString(const AValue: string);
begin
  FValue := SToMs(AValue, ifIO);
end;

end.
