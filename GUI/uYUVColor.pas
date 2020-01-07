unit uYUVColor;

interface

uses
  uTypes,
  uRGBColor;

type
  /// <summary>
  /// https://en.wikipedia.org/wiki/YUV
  /// </summary>
  TYUVColor = record
    /// <summary>
    /// Luma component
    /// </summary>
    Y: FG;

    /// <summary>
    /// Blue projection chrominance component (B-Y)
    /// </summary>
    U: FG;

    /// <summary>
    /// Red projection chrominance component (R-Y)
    /// </summary>
    V: FG;

    /// <summary>
    /// |Y|   |+0.299   +0.587   +0.114   | |R|
    /// |U| = |-0.14713 -0.28886 +0.436   | |G|
    /// |V|   |+0.615   -0.51499 -0.10001 | |B|
    /// </summary>
    procedure FromRGB(const ARGB: TRGBColor);

    /// <summary>
    /// |R|   |1  0       +1.13983 | |Y|
    /// |G| = |1 -0.39465 -0.58060 | |U|
    /// |B|   |1 +2.03211  0       | |V|
    /// </summary>
    function GetRGB: TRGBColor;
  end;

implementation

{ TYUVColor }

procedure TYUVColor.FromRGB(const ARGB: TRGBColor);
begin
  Y := 0.299 * ARGB.R + 0.587 * ARGB.G + 0.114 * ARGB.B;
  U := -0.14713 * ARGB.R - 0.28886 * ARGB.G + 0.436 * ARGB.B;
  V := 0.615 * ARGB.R - 0.51499 * ARGB.G - 0.10001 * ARGB.B;
end;

function TYUVColor.GetRGB: TRGBColor;
begin
  Result.R := Y + 1.13983 * V;
  Result.G := Y - 0.39465 * U - 0.58060 * V;
  Result.B := Y + 2.03211 * U;
end;

end.
