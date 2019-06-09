unit uConsoleDarkTheme;

interface

uses
  uTypes,
  uConsoleColor,
  uConsoleCustomTheme;

type
  TConsoleDarkTheme = class(TConsoleCustomTheme)
  public
    function GetColor(const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor): U2; override;
    function ErrorColor: U2; override;
  end;

implementation

{ TConsoleDarkTheme }

function TConsoleDarkTheme.ErrorColor: U2;
begin
  Result := GetColor(ccWhite, ccRed);
end;

function TConsoleDarkTheme.GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): U2;
var
  ForegroundColor: TConsoleColor;
begin
  if IsLightColor(DefaultBackgroundColor) then
    ForegroundColor := ChangeColorIntensity(AForegroundColor)
  else
    ForegroundColor := AForegroundColor;
  Result := ReadableColor(ForegroundColor, ABackgroundColor);
end;

end.
