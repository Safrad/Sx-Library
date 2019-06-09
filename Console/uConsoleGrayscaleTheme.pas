unit uConsoleGrayscaleTheme;

interface

uses
  uConsoleColor,
  uConsoleCustomTheme;

type
  TConsoleGrayscaleTheme = class(TConsoleCustomTheme)
  private
    function GrayScaleColor(const AColor: TConsoleColor): TConsoleColor;
  public
    function GetColor(const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor): TColorAttribute; override;
    function ErrorColor: TColorAttribute; override;
    function InformationColor: TColorAttribute; override;
    function DebugColor: TColorAttribute; override;
  end;

implementation

{ TConsoleGrayscaleTheme }

function TConsoleGrayscaleTheme.DebugColor: TColorAttribute;
begin
  Result := GetColor(ccWhite, ccGray);
end;

function TConsoleGrayscaleTheme.ErrorColor: TColorAttribute;
begin
  Result := GetColor(ccBlack, ccWhite);
end;

function TConsoleGrayscaleTheme.GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): TColorAttribute;
begin
  Result := ReadableColor(GrayScaleColor(AForegroundColor), GrayScaleColor(ABackgroundColor));
end;

function TConsoleGrayscaleTheme.GrayScaleColor(const AColor: TConsoleColor): TConsoleColor;
begin
  case AColor of
    ccBlack: Result := ccBlack;
    ccBlue: Result := ccBlack;
    ccGreen: Result := ccLightGray;
    ccAqua: Result := ccLightGray;
    ccRed: Result := ccGray;
    ccPurple: Result := ccLightGray;
    ccYellow: Result := ccLightGray;
    ccLightGray: Result := ccLightGray;
    ccGray: Result := ccGray;
    ccLightBlue: Result := ccGray;
    ccLightGreen: Result := ccWhite;
    ccLightAqua: Result := ccWhite;
    ccLightRed: Result := ccLightGray;
    ccLightPurple: Result := ccLightGray;
    ccLightYellow: Result := ccWhite;
    ccWhite: Result := ccWhite;
  else
    Result := ccLightGray;
  end;
end;

function TConsoleGrayscaleTheme.InformationColor: TColorAttribute;
begin
  Result := GetColor(ccLightGray, DefaultBackgroundColor);
end;

end.
