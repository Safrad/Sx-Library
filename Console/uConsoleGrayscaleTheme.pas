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
    function GetColor(const AForegroundColor: TConsoleColor; const ABackgroundColor: TConsoleColor): TColorAttribute; override;
    function GetColorForMessageLevel(const AMessageLevel: TMessageLevel): TColorAttribute; override;
  end;

implementation

{ TConsoleGrayscaleTheme }

function TConsoleGrayscaleTheme.GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): TColorAttribute;
begin
  Result := ReadableColor(GrayScaleColor(AForegroundColor), GrayScaleColor(ABackgroundColor));
end;

function TConsoleGrayscaleTheme.GetColorForMessageLevel(const AMessageLevel: TMessageLevel): TColorAttribute;
begin
  case AMessageLevel of
    mlConfirmation: Result := GetColor(ccLightGray, DefaultBackgroundColor);
    mlDebug: Result := GetColor(ccWhite, ccGray);
    mlInformation: Result := GetColor(ccLightGray, DefaultBackgroundColor);
    mlWarning: Result := GetColor(ccWhite, ccGray);
    mlError: Result := GetColor(ccBlack, ccWhite);
    mlFatalError: Result := GetColor(ccBlack, ccWhite);
  else
    Result := GetColor(DefaultForegroundColor, DefaultBackgroundColor);
  end;
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
