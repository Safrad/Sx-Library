unit uSxBitmapUtils;

interface

uses
  UITypes,
  Windows,
  uTypes,
  uColor,
  uSxBitmap,
  uDBitmap,
  uClipping;

type
  TSxBitmapUtils = class
  public
    procedure Copy(const ASource: TDBitmap; const ATarget: TSxBitmap);

    procedure Draw(const ASource: TSxBitmap; const ATarget: TDBitmap; ATargetRect: TRect;
      const AColor: TRGBA; const AClipping: TClipping);
    procedure DrawShadow(const ASource: TSxBitmap; const ATarget: TDBitmap; ATargetRect: TRect;
      const AClipping: TClipping);
  end;

implementation

{ TSxBitmapUtils }

procedure TSxBitmapUtils.Copy(const ASource: TDBitmap; const ATarget: TSxBitmap);
begin
  Assert(SG(ATarget.Width) = ASource.Width);
  Assert(SG(ATarget.Height) = ASource.Height);
  Move(ASource.GLData^, ATarget.Data^, ATarget.DataSize);
end;

procedure TSxBitmapUtils.Draw(const ASource: TSxBitmap; const ATarget: TDBitmap; ATargetRect: TRect;
  const AColor: TRGBA; const AClipping: TClipping);
var
  Source: PPixel;
  Target: PPixel;
  x, y: SG;
  ASourceRect: TRect;
begin
  ASourceRect.Left := 0;
  ASourceRect.Top := 0;
  ASourceRect.Right := ASource.Width - 1;
  ASourceRect.Bottom := ASource.Height - 1;

//  AClipping.ClipRects(ATargetRect, ASourceRect);
  Assert(ASourceRect.Left < SG(ASource.Width));
  Assert(ASourceRect.Top < SG(ASource.Height));

  for y := ATargetRect.Top to ATargetRect.Bottom do
  begin
    Target := ATarget.GetPixelAddr(ATargetRect.Left, y);
    Source := ASource.GetPixelAddr(ASourceRect.Left, ASourceRect.Bottom - y + ATargetRect.Top);
    for x := ATargetRect.Left to ATargetRect.Right do
    begin
      // Correct is 255, 256 is for optimization
      Target.R := (Target.R * (256 - Source.R) + AColor.B * Source.R) div 256;
      Target.G := (Target.G * (256 - Source.G) + AColor.G * Source.G) div 256;
      Target.B := (Target.B * (256 - Source.B) + AColor.R * Source.B) div 256;

      Inc(Source);
      Inc(Target);
    end;
  end;
end;

procedure TSxBitmapUtils.DrawShadow(const ASource: TSxBitmap; const ATarget: TDBitmap; ATargetRect: TRect;
  const AClipping: TClipping);
var
  Source: PPixel;
  Target: PPixel;
  x, y: SG;
  ASourceRect: TRect;
begin
  ASourceRect.Left := 0;
  ASourceRect.Top := 0;
  ASourceRect.Right := ASource.Width - 1;
  ASourceRect.Bottom := ASource.Height - 1;

//  AClipping.ClipRects(ATargetRect, ASourceRect);

  for y := ATargetRect.Top to ATargetRect.Bottom do
  begin
    Target := ATarget.GetPixelAddr(ATargetRect.Left, y);
    Source := ASource.GetPixelAddr(ASourceRect.Left, ASourceRect.Bottom - y + ATargetRect.Top);
    for x := ATargetRect.Left to ATargetRect.Right do
    begin
      Target.R := 256 * Target.R div (256 + Source.R);
      Target.G := 256 * Target.G div (256 + Source.G);
      Target.B := 256 * Target.B div (256 + Source.B);

      Inc(Source);
      Inc(Target);
    end;
  end;
end;

end.
