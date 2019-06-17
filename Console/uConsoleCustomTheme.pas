// Ancestor for TConsoleDarkTheme in uConsoleDarkTheme and TConsoleGrayscaleTheme in uConsoleGrayscaleTheme

unit uConsoleCustomTheme;

interface

uses
  uTypes,
  uConsoleColor;

type
  TConsoleCustomTheme = class
  private
    FDefaultColor: TColorAttribute;
    procedure SetDefaultColor(const Value: TColorAttribute);
  public
    function IsLightColor(const AConsoleColor: TConsoleColor): Boolean;
    function ChangeColorIntensity(const AConsoleColor: TConsoleColor): TConsoleColor;
    function ReadableColor(const AForegroundColor, ABackgroundColor: TConsoleColor): TColorAttribute;
    property DefaultColor: TColorAttribute read FDefaultColor write SetDefaultColor;
    function DefaultForegroundColor: TConsoleColor;
    function DefaultBackgroundColor: TConsoleColor;

    function GetColor(const AForegroundColor, ABackgroundColor: TConsoleColor): TColorAttribute; virtual; abstract;
    function GetColorForMessageLevel(const AMessageLevel: TMessageLevel): TColorAttribute; virtual; abstract;
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
    Result := TConsoleColor(15 - TColorAttribute(AConsoleColor));
  else
    if TColorAttribute(AConsoleColor) < 8 then
      Result := TConsoleColor(TColorAttribute(AConsoleColor) + 8)
    else
      Result := TConsoleColor(TColorAttribute(AConsoleColor) - 8);
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

function TConsoleCustomTheme.IsLightColor(const AConsoleColor: TConsoleColor): Boolean;
begin
  Result := (TColorAttribute(DefaultBackgroundColor) = 7)  or (TColorAttribute(DefaultBackgroundColor) > 8);
end;

function TConsoleCustomTheme.ReadableColor(const AForegroundColor, ABackgroundColor: TConsoleColor): TColorAttribute;
var
  ForegroundColor: TConsoleColor;
begin
  if AForegroundColor = ABackgroundColor then
    ForegroundColor := ChangeColorIntensity(AForegroundColor)
  else
    ForegroundColor := AForegroundColor;

  Result := TColorAttribute(ForegroundColor) or TColorAttribute(ABackgroundColor) shl 4;
end;

procedure TConsoleCustomTheme.SetDefaultColor(const Value: TColorAttribute);
begin
  FDefaultColor := ReadableColor(TConsoleColor(Value and $0f), TConsoleColor(Value shr 4));
end;

end.
