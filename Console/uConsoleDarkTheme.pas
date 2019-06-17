unit uConsoleDarkTheme;

interface

uses
  uTypes,
  uConsoleColor,
  uConsoleCustomTheme;

type
  TConsoleDarkTheme = class(TConsoleCustomTheme)
  public
    function GetColor(const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor): TColorAttribute; override;
    function GetColorForMessageLevel(const AMessageLevel: TMessageLevel): TColorAttribute; override;
  end;

implementation

{ TConsoleDarkTheme }

function TConsoleDarkTheme.GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): TColorAttribute;
var
  ForegroundColor: TConsoleColor;
begin
  if IsLightColor(DefaultBackgroundColor) then
    ForegroundColor := ChangeColorIntensity(AForegroundColor)
  else
    ForegroundColor := AForegroundColor;
  Result := ReadableColor(ForegroundColor, ABackgroundColor);
end;

function TConsoleDarkTheme.GetColorForMessageLevel(const AMessageLevel: TMessageLevel): TColorAttribute;
begin
  case AMessageLevel of
    mlConfirmation: Result := GetColor(ccLightAqua, DefaultBackgroundColor);
    mlDebug: Result := GetColor(ccLightAqua, ccBlue);
    mlInformation: Result := GetColor(ccLightBlue, DefaultBackgroundColor);
    mlWarning: Result := GetColor(ccLightYellow, DefaultBackgroundColor);
    mlError: Result := GetColor(ccWhite, ccRed);
    mlFatalError: Result := GetColor(ccWhite, ccPurple);
  else
    Result := GetColor(DefaultForegroundColor, DefaultBackgroundColor);
  end;
end;

end.
