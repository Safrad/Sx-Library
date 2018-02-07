unit uNumericArrayArgument;

interface

uses
  uTypes,
  uStringArrayArgument;

type
  TNumericArrayArgument = class(TStringArrayArgument)
  private
    FValues: TArrayOfS8;
    function GetValues: TArrayOfS8;
  protected
		function GetSyntax: string; override;
  public
    procedure SetValueFromString(const AValue: string); override;
    property Values: TArrayOfS8 read GetValues;
  end;

implementation

uses
  uStrings, SysUtils;

{ TNumericArrayArgument }

function TNumericArrayArgument.GetSyntax: string;
begin
  Result := '1' + Separator + '2' + Separator + '3...';
end;


function TNumericArrayArgument.GetValues: TArrayOfS8;
begin
  Used := True;
  Result := FValues;
end;

procedure TNumericArrayArgument.SetValueFromString(const AValue: string);
var
  A: TArrayOfString;
  i: SG;
begin
  inherited;

  A := SplitStringEx(AValue, Separator);
  SetLength(FValues, Length(A));

  for i := 0 to Length(A) - 1 do
  begin
    FValues[i] := StrToInt64(A[i]);
  end;
end;

end.
