unit uNamedColors;

interface

uses
  uTypes,
  uColor,
  Graphics;

type
  TNamedColorEnum = (
    ncAliceBlue,
    ncAmber, // New
    ncAntiqueWhite,
    ncAqua,
    ncAquamarine,
    ncAzure,
    ncBeige,
    ncBisque,
    ncBlack,
    ncBlanchedAlmond,
    ncBlue,
    ncBlueViolet,
    ncBrown,
    ncBurlyWood,
    ncCadetBlue,
    ncChartreuse,
    ncChocolate,
    ncCoral,
    ncCornflowerBlue,
    ncCornsilk,
    ncCream,
    ncCrimson,
    ncCyan,
    ncDarkBlue,
    ncDarkCyan,
    ncDarkGoldenrod,
    ncDarkGray,
    ncDarkGreen,
    ncDarkKhaki,
    ncDarkMagenta,
    ncDarkOliveGreen,
    ncDarkOrange,
    ncDarkOrchid,
    ncDarkRed,
    ncDarkSalmon,
    ncDarkSeaGreen,
    ncDarkSlateBlue,
    ncDarkSlateGray,
    ncDarkTurquoise,
    ncDarkViolet,
    ncDeepPink,
    ncDeepSkyBlue,
    ncDimGray,
    ncDodgerBlue,
    ncFirebrick,
    ncFlesh, // New
    ncFloralWhite,
    ncForestGreen, // https://en.wikipedia.org/wiki/Variations_of_green#Forest_green
//    ncFuchsia, = mcMagenta
    ncGainsboro,
    ncGhostWhite,
    ncGold,
    ncGoldenrod,
    ncGray,
    ncGreen,
    ncGreenYellow,
    ncHoneydew,
    ncHotPink,
    ncIndianRed,
    ncIndigo,
    ncIvory,
    ncKhaki,
    ncLavender,
    ncLavenderBlush,
    ncLawnGreen,
    ncLemonChiffon,
    ncLightBlue,
    ncLightCoral,
    ncLightCyan,
    ncLightGoldenrodYellow,
    ncLightGray,
    ncLightGreen,
    ncLightPink,
    ncLightSalmon,
    ncLightSeaGreen,
    ncLightSkyBlue,
    ncLightSlateGray,
    ncLightSteelBlue,
    ncLightYellow,
    ncLime,
    ncLimeGreen,
    ncLinen,
    ncMagenta,
    ncMaroon,
    ncMediumAquamarine,
    ncMediumBlue,
    ncMediumOrchid,
    ncMediumPurple,
    ncMediumSeaGreen,
    ncMediumSlateBlue,
    ncMediumSpringGreen,
    ncMediumTurquoise,
    ncMediumVioletRed,
    ncMidnightBlue,
    ncMintCream,
    ncMistyRose,
    ncMoccasin,
    ncMoneyGreen, // New
    ncNavajoWhite,
    ncNavy,
    ncOldLace,
    ncOlive,
    ncOliveDrab,
    ncOrange,
    ncOrangeRed,
    ncOrchid,
    ncPaleGoldenrod,
    ncPaleGreen,
    ncPaleTurquoise,
    ncPaleVioletRed,
    ncPapayaWhip,
    ncPeachPuff,
    ncPeru,
    ncPink,
    ncPlum,
    ncPowderBlue,
    ncPurple,
    ncRed,
    ncRosyBrown,
    ncRoyalBlue,
    ncSaddleBrown,
    ncSalmon,
    ncSandyBrown,
    ncSeaGreen,
    ncSeaShell,
    ncSienna,
    ncSilver,
    ncSkyBlue,
    ncSlateBlue,
    ncSlateGray,
    ncSnow,
    ncSpringGreen,
    ncSteelBlue,
    ncTan,
    ncTeal,
    ncThistle,
    ncTomato,
    ncTurquoise,
    ncViolet,
    ncWater, // New
    ncWheat,
    ncWhite,
    ncWhiteSmoke,
    ncYellow,
    ncYellowGreen
  );

type
  TNamedColors = class
  private
    class function GetNamedColorEnum(const AColor: TColor; out ANamedColorEnum: TNamedColorEnum): BG;
  public
    class function GetRGBAColor(const ANamedColor: TNamedColorEnum): TRGBA;
    class function GetColor(const ANamedColor: TNamedColorEnum): TColor;
    class function GetName(const ANamedColor: TNamedColorEnum): string; overload;
    class function GetName(const AColor: TColor): string; overload;
  end;

implementation

uses
  uMath, uStrings, SysUtils;

const
  ColorRGBValues: array[TNamedColorEnum] of U4 = (
    $FFF0F8FF,
    $FFFFBF00, // New
    $FFFAEBD7,
    $FF00FFFF,
    $FF7FFFD4,
    $FFF0FFFF,
    $FFF5F5DC,
    $FFFFE4C4,
    $FF000000,
    $FFFFEBCD,
    $FF0000FF,
    $FF8A2BE2,
    $FFA52A2A,
    $FFDEB887,
    $FF5F9EA0,
    $FF7FFF00,
    $FFD2691E,
    $FFFF7F50,
    $FF6495ED,
    $FFFFF8DC,
    clCream,
    $FFDC143C,
    $FF00FFFF,
    $FF00008B,
    $FF008B8B,
    $FFB8860B,
    $FFA9A9A9,
    $FF006400,
    $FFBDB76B,
    $FF8B008B,
    $FF556B2F,
    $FFFF8C00,
    $FF9932CC,
    $FF8B0000,
    $FFE9967A,
    $FF8FBC8B,
    $FF483D8B,
    $FF2F4F4F,
    $FF00CED1,
    $FF9400D3,
    $FFFF1493,
    $FF00BFFF,
    $FF696969,
    $FF1E90FF,
    $FFB22222,
    $FFFFAD98, // New
    $FFFFFAF0,
    $FF228B22,
//    $FFFF00FF,
    $FFDCDCDC,
    $FFF8F8FF,
    $FFFFD700,
    $FFDAA520,
    $FF808080,
    $FF008000,
    $FFADFF2F,
    $FFF0FFF0,
    $FFFF69B4,
    $FFCD5C5C,
    $FF4B0082,
    $FFFFFFF0,
    $FFF0E68C,
    $FFE6E6FA,
    $FFFFF0F5,
    $FF7CFC00,
    $FFFFFACD,
    $FFADD8E6,
    $FFF08080,
    $FFE0FFFF,
    $FFFAFAD2,
    $FFD3D3D3,
    $FF90EE90,
    $FFFFB6C1,
    $FFFFA07A,
    $FF20B2AA,
    $FF87CEFA,
    $FF778899,
    $FFB0C4DE,
    $FFFFFFE0,
    $FF00FF00,
    $FF32CD32,
    $FFFAF0E6,
    $FFFF00FF,
    $FF800000,
    $FF66CDAA,
    $FF0000CD,
    $FFBA55D3,
    $FF9370DB,
    $FF3CB371,
    $FF7B68EE,
    $FF00FA9A,
    $FF48D1CC,
    $FFC71585,
    $FF191970,
    $FFF5FFFA,
    $FFFFE4E1,
    $FFFFE4B5,
    $FFC0DCC0, // New
    $FFFFDEAD,
    $FF000080,
    $FFFDF5E6,
    $FF808000,
    $FF6B8E23,
    $FFFFA500,
    $FFFF4500,
    $FFDA70D6,
    $FFEEE8AA,
    $FF98FB98,
    $FFAFEEEE,
    $FFDB7093,
    $FFFFEFD5,
    $FFFFDAB9,
    $FFCD853F,
    $FFFFC0CB,
    $FFDDA0DD,
    $FFB0E0E6,
    $FF800080,
    $FFFF0000,
    $FFBC8F8F,
    $FF4169E1,
    $FF8B4513,
    $FFFA8072,
    $FFF4A460,
    $FF2E8B57,
    $FFFFF5EE,
    $FFA0522D,
    $FFC0C0C0,
    $FF87CEEB,
    $FF6A5ACD,
    $FF708090,
    $FFFFFAFA,
    $FF00FF7F,
    $FF4682B4,
    $FFD2B48C,
    $FF008080,
    $FFD8BFD8,
    $FFFF6347,
    $FF40E0D0,
    $FFEE82EE,
    $FF56D8D1, // New
    $FFF5DEB3,
    $FFFFFFFF,
    $FFF5F5F5,
    $FFFFFF00,
    $FF9ACD32
  );

var
  ColorNames: array[TNamedColorEnum] of string;

{ TNamedColor }

class function TNamedColors.GetColor(const ANamedColor: TNamedColorEnum): TColor;
begin
  Result := ColorRGBValues[ANamedColor] and $FFFFFF;
  Exchange(TRGBA(Result).R, TRGBA(Result).B);
end;

class function TNamedColors.GetName(
  const ANamedColor: TNamedColorEnum): string;
begin
  Result := ColorNames[ANamedColor];
end;

class function TNamedColors.GetNamedColorEnum(const AColor: TColor; out ANamedColorEnum: TNamedColorEnum): BG;
var
  NamedColorEnum: TNamedColorEnum;
begin
  Result := False;
  for NamedColorEnum := Low(TNamedColorEnum) to High(NamedColorEnum) do
  begin
    if AColor = GetColor(NamedColorEnum) then
    begin
      ANamedColorEnum := NamedColorEnum;
      Result := True;
      Break;
    end;
  end;
end;

class function TNamedColors.GetName(const AColor: TColor): string;
var
  NamedColorEnum: TNamedColorEnum;
begin
  if GetNamedColorEnum(AColor, NamedColorEnum) then
    Result := ColorNames[NamedColorEnum]
  else
    Result := '';
end;

class function TNamedColors.GetRGBAColor(
  const ANamedColor: TNamedColorEnum): TRGBA;
begin
  Result.C := ColorRGBValues[ANamedColor];
  Exchange(Result.R, Result.B);
end;

initialization
  EnumToStr(TypeInfo(TNamedColorEnum), ColorNames);
end.
