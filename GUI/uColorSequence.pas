unit uColorSequence;

interface

uses
  UITypes,
  uTypes,
  uColor;

type
  TArrayOfColor = array of TColor;

  TColorSequence = class
  private
    FCount: SG;
    FIndex: SG;
    FBackground: TColor;
    FFirstColor: TColor;
    FUsedColors: TArrayOfColor;

    function ColorDifference(const AColor: TColor): SG;

    procedure SetCount(const Value: SG);
    procedure SetIndex(const Value: SG);
    procedure SetBackground(const Value: TColor);
    procedure SetFirstColor(const Value: TColor);

    procedure CalculatePalette;
  public
    constructor Create;

    // Setup
    procedure Reset;

    property FirstColor: TColor read FFirstColor write SetFirstColor;
    property Background: TColor read FBackground write SetBackground;

    property Count: SG read FCount write SetCount;

    // Output
    // Known number of colors
    property Palette: TArrayOfColor read FUsedColors;

    // Unknown number of colors
    property Index: SG read FIndex write SetIndex;
    function GetNextColor: TColor;
  end;

implementation

const
  // https://github.com/charted-co/charted/blob/master/src/client/Chart.js#L190
//  NiceColors: array[0..6] of TColor = ($73CC6D, $75771D, $D5CF4F, $51E6FC, $5070FF, $50C0FF, $999999);
  NiceColors: array[0..13] of TColor = (
    $73CC6D, $75771D, $D5CF4F, $51E6FC, $5070FF, $50C0FF, $999999,
    $ccffff, $b4e9c7, $bbcd7f, $c4b641, $c0911d, $a85e22, $842c0c
  );

{ TColorSequence }

procedure TColorSequence.CalculatePalette;
var
  i: SG;
  Score: SG;
  BestScore: SG;
  BestColor: TColor;
  Color: TColor;
begin
  if FFirstColor <> TColors.SysNone then
  begin
    SetLength(FUsedColors, 1);
    FUsedColors[0] := FFirstColor;
  end;
  while Length(FUsedColors) < FCount do
  begin
    BestScore := 0;
    BestColor := 0;
    for i := 0 to Length(NiceColors) - 1 do
    begin
      Color := NiceColors[i];
      Score := ColorDifference(Color);
      if Score > BestScore then
      begin
        BestScore := Score;
        BestColor := Color;
      end;
    end;
    SetLength(FUsedColors, Length(FUsedColors) + 1);
    FUsedColors[Length(FUsedColors) - 1] := BestColor;
  end;
end;

function ColorDifference2(AColor1, AColor2: TColor): SG;
var
  RGBColor1: TRGBA;
  RGBColor2: TRGBA;
  Color1: THLSColor;
  Color2: THLSColor;
begin
  RGBColor1.L := ColorToRGB(AColor1);
  RGBColor2.L := ColorToRGB(AColor2);
  Color1 := RGBToHLS(RGBColor1);
  Color2 := RGBToHLS(RGBColor2);

  Result := 6 * Abs(Color1.L - Color2.L) + Abs(Color1.H - Color2.H);
end;

function TColorSequence.ColorDifference(const AColor: TColor): SG;
var
  i: SG;
  Score: SG;
begin
  Result := MaxInt;
  if FBackground <> TColors.SysNone then
  begin
    Score := ColorDifference2(AColor, FBackground);
    if Score < Result then
      Result := Score;
  end;
  for i := 0 to Length(FUsedColors) - 1 do
  begin
    Score := ColorDifference2(AColor, FUsedColors[i]);
    if Score < Result then
      Result := Score;
  end;
end;

constructor TColorSequence.Create;
begin
  inherited;

  FFirstColor := TColors.SysNone;
  FBackground := TColors.SysNone;
end;

function TColorSequence.GetNextColor: TColor;
begin
  if FIndex > FCount then
  begin
    Count := Count + 1;
  end;
  Result := FUsedColors[FIndex];
  Inc(FIndex);
end;

procedure TColorSequence.Reset;
begin
  FIndex := 0;
  Count := 0;
end;

procedure TColorSequence.SetBackground(const Value: TColor);
begin
  FBackground := Value;
end;

procedure TColorSequence.SetCount(const Value: SG);
begin
  FCount := Value;
  CalculatePalette;
end;

procedure TColorSequence.SetFirstColor(const Value: TColor);
begin
  FFirstColor := Value;
end;

procedure TColorSequence.SetIndex(const Value: SG);
begin
  FIndex := Value;
end;

end.
