unit uConsoleCustomTheme;

interface

uses
  uTypes,
  uConsoleColor;

type
  TConsoleCustomTheme = class
  private
    FDefaultColor: U2;
    procedure SetDefaultColor(const Value: U2);
  public
    function IsLightColor(const AConsoleColor: TConsoleColor): BG;
    function ChangeColorIntensity(const AConsoleColor: TConsoleColor): TConsoleColor;
    function ReadableColor(const AForegroundColor, ABackgroundColor: TConsoleColor): U2;
    property DefaultColor: U2 read FDefaultColor write SetDefaultColor;
    function DefaultForegroundColor: TConsoleColor;
    function DefaultBackgroundColor: TConsoleColor;

    function GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): U2; virtual; abstract;
    function ErrorColor: U2; virtual; abstract;
  end;

implementation

{ TConsoleCustomTheme }

function TConsoleCustomTheme.ChangeColorIntensity(const AConsoleColor: TConsoleColor): TConsoleColor;
begin
  case AConsoleColor of
  ccBlack,
  ccWhite,
  ccLightGray,
  ccGray:
    Result := TConsoleColor(15 - U1(AConsoleColor));
  else
    if U1(AConsoleColor) < 8 then
      Result := TConsoleColor(U1(AConsoleColor) + 8)
    else
      Result := TConsoleColor(U1(AConsoleColor) - 8);
  end;
end;

function TConsoleCustomTheme.DefaultBackgroundColor: TConsoleColor;
begin
  Result := TConsoleColor(FDefaultColor shr 4);
end;

function TConsoleCustomTheme.DefaultForegroundColor: TConsoleColor;
begin
  Result := TConsoleColor(FDefaultColor and $0f);
end;

function TConsoleCustomTheme.IsLightColor(const AConsoleColor: TConsoleColor): BG;
begin
  Result := (U1(DefaultBackgroundColor) = 7)  or (U1(DefaultBackgroundColor) > 8);
end;

function TConsoleCustomTheme.ReadableColor(const AForegroundColor, ABackgroundColor: TConsoleColor): U2;
var
  ForegroundColor: TConsoleColor;
begin
  if AForegroundColor = ABackgroundColor then
    ForegroundColor := ChangeColorIntensity(AForegroundColor)
  else
    ForegroundColor := AForegroundColor;

  Result := U2(ForegroundColor) or U2(ABackgroundColor) shl 4;
end;

procedure TConsoleCustomTheme.SetDefaultColor(const Value: U2);
begin
  FDefaultColor := ReadableColor(TConsoleColor(Value and $0f), TConsoleColor(Value shr 4));
end;

end.
