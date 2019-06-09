unit uConsoleColor;

interface

uses
  uTypes,
  UITypes;

type
  TColorAttribute = U2;

  TConsoleColor = (ccBlack, ccBlue, ccGreen, ccAqua, ccRed, ccPurple, ccYellow, ccLightGray, ccGray, ccLightBlue,
    ccLightGreen, ccLightAqua, ccLightRed, ccLightPurple, ccLightYellow, ccWhite);

function GetColor(const AConsoleColor: TConsoleColor): TColor;

implementation

const
  ConsoleColors: array[TConsoleColor] of TColor = (
    TColorRec.Black,
    TColorRec.Navy,
    TColorRec.Green,
    TColorRec.Teal,
    TColorRec.Maroon,
    TColorRec.Purple,
    TColorRec.Olive,
    TColorRec.Lightgray,
    TColorRec.Gray,
    TColorRec.Blue,
    TColorRec.Lime,
    TColorRec.Aqua,
    TColorRec.Red,
    TColorRec.Magenta,
    TColorRec.Yellow,
    TColorRec.White);

function GetColor(const AConsoleColor: TConsoleColor): TColor;
begin
  Result := ConsoleColors[AConsoleColor];
end;

end.
