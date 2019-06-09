unit uConsoleGrayscaleTheme;

interface

uses
  uTypes,
  uConsoleColor,
  uConsoleCustomTheme;

type
  TConsoleGrayscaleTheme = class(TConsoleCustomTheme)
  private
    function GrayScaleColor(const AColor: TConsoleColor): TConsoleColor;
  public
    function GetColor(const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor): U2; override;
    function ErrorColor: U2; override;
  end;

implementation

{ TConsoleGrayscaleTheme }

function TConsoleGrayscaleTheme.ErrorColor: U2;
begin
  Result := GetColor(ccBlack, ccWhite);
end;

function TConsoleGrayscaleTheme.GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): U2;
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

end.
