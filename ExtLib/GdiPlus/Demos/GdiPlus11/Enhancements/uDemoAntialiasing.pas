unit uDemoAntialiasing;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoAntialiasing = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoAntialiasing }

{$REGION}
/// GDI+ 1.1 supports an additional smoothing mode called
/// <A>SmoothingModeAntiAlias8x8</A>. This is the highest quality antialiasing
/// mode which produces better quality especially for near-horizontal lines
/// and edges.

procedure TDemoAntialiasing.Run;
const
  SmoothingModes: array [SmoothingModeDefault..SmoothingModeAntiAlias8x8] of String = (
    'SmoothingModeDefault', 'SmoothingModeHighSpeed', 'SmoothingModeHighQuality',
    'SmoothingModeNone', 'SmoothingModeAntiAlias8x4', 'SmoothingModeAntiAlias8x8');
var
  SmoothingMode: TGPSmoothingMode;
  Font: IGPFont;
  BlackBrush, BlueBrush: IGPBrush;
  Y: Integer;
  Points: array [0..2] of TGPPoint;
begin
  Font := TGPFont.Create('Arial', 18, FontStyleRegular, UnitPixel);
  BlackBrush := TGPSolidBrush.Create(TGPColor.Black);
  BlueBrush := TGPSolidBrush.Create(TGPColor.Blue);
  Graphics.Clear(TGPColor.White);

  Y := 0;
  for SmoothingMode := SmoothingModeDefault to SmoothingModeAntiAlias8x8 do
  begin
    Graphics.SmoothingMode := SmoothingMode;

    Graphics.DrawString(SmoothingModes[SmoothingMode], Font,
      TGPPointF.Create(5, Y + 25), BlackBrush);

    Points[0].Initialize(250, Y);
    Points[1].Initialize(250, Y + 60);
    Points[2].Initialize(260, Y + 60);
    Graphics.FillPolygon(BlueBrush, Points);

    Points[0].Initialize(270, Y + 40);
    Points[1].Initialize(360, Y + 40);
    Points[2].Initialize(360, Y + 35);
    Graphics.FillPolygon(BlueBrush, Points);

    Graphics.FillEllipse(BlueBrush, 380, Y, 200, 60);
    Inc(Y, 70);
  end;
end;

/// In the illustration above you can see that <A>SmoothingModeAntiAlias8x8</A>
/// produces a slightly better rendering of the long flat triangle in the
/// bottom row.
{$ENDREGION}

initialization
  RegisterDemo('Enhancements\Additional Antialiasing Options', TDemoAntialiasing);

end.
