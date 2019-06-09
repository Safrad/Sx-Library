unit uConsoleDarkTheme;

interface

uses
  uConsoleColor,
  uConsoleCustomTheme;

type
  TConsoleDarkTheme = class(TConsoleCustomTheme)
  public
    function GetColor(const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor): TColorAttribute; override;
    function ErrorColor: TColorAttribute; override;
    function InformationColor: TColorAttribute; override;
    function DebugColor: TColorAttribute; override;
  end;

implementation

{ TConsoleDarkTheme }

function TConsoleDarkTheme.DebugColor: TColorAttribute;
begin
  Result := GetColor(ccGray, ccLightGreen);
end;

function TConsoleDarkTheme.ErrorColor: TColorAttribute;
begin
  Result := GetColor(ccWhite, ccRed);
end;

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

function TConsoleDarkTheme.InformationColor: TColorAttribute;
begin
  Result := GetColor(ccLightBlue, DefaultBackgroundColor);
end;

end.
