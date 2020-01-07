unit uYCbCrColor;

interface

uses
  uTypes,
  uRGBColor;

type
  /// <summary>
  /// https://en.wikipedia.org/wiki/YCbCr
  /// </summary>
  TYCbCrColor = record
    /// <summary>
    /// Luma component
    /// </summary>
    Y: FG;

    /// <summary>
    /// Blue-difference chroma component
    /// </summary>
    Cb: FG;

    /// <summary>
    /// Red-difference chroma component
    /// </summary>
    Cr: FG;

    /// <summary>
    /// |Y|    |+0.299    +0.587    +0.114    | |R|
    /// |Cb| = |-0.168736 -0.331264 +0.5      | |G|
    /// |Cr|   |+0.5      -0.418688 -0.081312 | |B|
    /// </summary>
    procedure FromRGB(const ARGB: TRGBColor);

    /// <summary>
    /// |R|   |1  0        +1.402    | |Y|
    /// |G| = |1 -0.344136 -0.714136 | |Cb|
    /// |B|   |1 +1.772     0        | |Cr|
    /// </summary>
    function GetRGB: TRGBColor;
  end;

implementation

{ TYCbCrColor }

procedure TYCbCrColor.FromRGB(const ARGB: TRGBColor);
begin
  Y := 0.299 * ARGB.R + 0.587 * ARGB.G + 0.114 * ARGB.B;
  Cb := 0.168736 * ARGB.R - 0.331264 * ARGB.G + 0.5 * ARGB.B;
  Cr := 0.5 * ARGB.R - 0.418688 * ARGB.G - 0.081312 * ARGB.B;
end;

function TYCbCrColor.GetRGB: TRGBColor;
begin
  Result.R := Y + 1.402 * Cr;
  Result.G := Y - 0.344136 * Cb - 0.714136 * Cr;
  Result.B := Y + 1.772 * Cb;
end;

end.
